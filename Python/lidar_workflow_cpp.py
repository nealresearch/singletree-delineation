"""LiDAR classification with pybind11 C++ neighbourhood features.

Requires: numpy, pandas, laspy, scikit-learn, lidar_cpp (.pyd).
The caller must supply its existing tile_list, trained classifier clf3 and
fitted label transformer le. Do not retrain or reorder model features here.
"""

from pathlib import Path
import time

import laspy
import numpy as np
import pandas as pd
from sklearn.impute import SimpleImputer

import lidar_cpp


RADII = [0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]


class Slice:
    def __init__(self, data_path):
        self.path = Path(data_path)
        self.las = laspy.read(str(self.path))

    def bounds(self):
        """XY bounds of the complete LAS/LAZ tile."""
        h = self.las.header
        return h.x_min, h.y_min, h.x_max, h.y_max

    def coordinates(self, tile_size=100):
        """Create 100 x 100 m processing regions (or chosen tile size)."""
        xmin, ymin, xmax, ymax = self.bounds()
        coords = []
        x = xmin
        while x < xmax:
            y = ymin
            while y < ymax:
                coords.append((x, y, x + tile_size, y + tile_size))
                y += tile_size
            x += tile_size
        return coords

    def lidar_data(self, minx, miny, maxx, maxy):
        """Extract points from one region, retaining original behavior."""
        mask = (
            (self.las.x >= minx) & (self.las.x <= maxx)
            & (self.las.y >= miny) & (self.las.y <= maxy)
        )
        return self.las[mask]

    @staticmethod
    def initial_predict(matrix, out_path, las, model, trans,
                        classification_filter=None):
        """Classify the provided LAS subset and save to LAS/LAZ."""
        lidar_data = laspy.read(str(las)) if isinstance(las, (str, Path)) else las
        y_encoded = model.predict(matrix)
        y_pred = trans.inverse_transform(y_encoded).astype(np.uint8)
        if classification_filter is not None:
            lidar_data.classification[classification_filter] = y_pred
        else:
            # Use inverse-transformed LAS codes, not encoded ML labels.
            lidar_data.classification = y_pred
        lidar_data.write(str(out_path))
        return "Data created"

    @staticmethod
    def iteration_predict(matrix, output, las, classification_filter, gb_clf, le):
        """Kept for compatibility with the later iteration workflow."""
        new_lidar = laspy.LasData(las.header)
        new_lidar.points = las.points.copy()
        y_encoded = gb_clf.predict(matrix)
        y_pred = le.inverse_transform(y_encoded).astype(np.uint8)
        new_lidar.classification[classification_filter] = y_pred
        new_lidar.write(str(output))

    @staticmethod
    def impute_dataframe(dataframe):
        """Match the original mean-imputation step."""
        features = np.asarray(dataframe.to_numpy(), dtype=np.float64)
        features[~np.isfinite(features)] = np.nan
        imputer = SimpleImputer(strategy="mean")
        matrix = imputer.fit_transform(features)
        if matrix.shape[1] != features.shape[1]:
            missing = dataframe.columns[np.isnan(features).all(axis=0)].tolist()
            raise ValueError(
                f"All-NaN feature columns were removed by the imputer: {missing}. "
                "This changes the model's expected feature count."
            )
        return matrix


def create_feature_dataframe(points, radii=RADII):
    """Reproduce the *original model's* column names and column order.

    The original code concatenates all eigenvalue-derived columns per radius,
    followed by all verticality columns per radius.
    """
    points = np.ascontiguousarray(points, dtype=np.float64)
    if points.ndim != 2 or points.shape[1] != 3:
        raise ValueError("points must be an N x 3 XYZ array")

    value_columns = {}
    verticality_columns = {}
    start = time.perf_counter()

    for radius in radii:
        results = lidar_cpp.calculate_features(points, float(radius))
        suffix = str(radius)   # Matches f"{metric_name}{radius}" from original.

        # Preserve original eigenvalue feature ordering for EACH radius.
        value_columns[f"planarity{suffix}"] = results["planarity"]
        value_columns[f"sphericity{suffix}"] = results["sphericity"]
        value_columns[f"anisotropy{suffix}"] = results["anisotropy"]
        value_columns[f"surfaceVariation{suffix}"] = results["surface_variation"]
        value_columns[f"omnivariance{suffix}"] = results["omnivariance"]
        value_columns[f"linearity{suffix}"] = results["linearity"]

        # Assumes original calculate_curvature used lambda_min / sum(lambda).
        # VERIFY against the implementation used when training clf3.
        value_columns[f"curvature{suffix}"] = results["surface_variation"]
        value_columns[f"small_eigen{suffix}"] = results["eigenvalues"][:, 0]

        # The original df_vec was appended after ALL df_val columns.
        verticality_columns[f"verticality{suffix}"] = results["verticality"]

    df_val = pd.DataFrame(value_columns)
    df_vec = pd.DataFrame(verticality_columns)
    df = pd.concat([df_val, df_vec], axis=1)

    # Retain original predictor exclusion; Z was added then removed originally.
    df = df.drop(columns=["small_eigen0.6"])

    print(f"Computed {len(df):,} points, {len(radii)} radii, "
          f"{df.shape[1]} features in {time.perf_counter() - start:.2f} s")
    return df


def run_classification(tile_list, clf3, le,
                       folder_path=r"M:\phd\digi",
                       output_dir=r"M:\phd\classified"):
    """Run the same tile -> 100-m chunk -> classify -> LAZ pipeline.

    tile_list: file names (or paths) for LAS/LAZ tiles.
    clf3: existing fitted classifier.
    le: existing fitted label encoder/transformer.
    """
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    for tile in tile_list:
        tile_path = Path(tile)
        if not tile_path.is_absolute():
            tile_path = Path(folder_path) / tile_path

        try:
            cut = Slice(tile_path)
        except Exception as err:
            print(f"Could not read tile {tile_path}: {err}")
            continue

        chunk_index = 0
        for coords in cut.coordinates(tile_size=100):
            try:
                out_data = cut.lidar_data(*coords)
                classes = np.asarray(out_data.classification)
                vegetation_mask = ~np.isin(classes, [0, 1, 2])
                output_data = out_data[vegetation_mask]

                if len(output_data) <= 100:
                    continue

                points = np.column_stack([
                    np.asarray(output_data.x),
                    np.asarray(output_data.y),
                    np.asarray(output_data.z),
                ]).astype(np.float64, copy=False)

                df_features = create_feature_dataframe(points)

                # Model feature schema must be the same as at training.
                if hasattr(clf3, "feature_names_in_"):
                    expected = list(clf3.feature_names_in_)
                    if expected != list(df_features.columns):
                        raise ValueError(
                            "Classifier feature_names_in_ differs from the "
                            "original feature order. Do not predict until "
                            "the training schema is reconciled."
                        )

                feature_mat = cut.impute_dataframe(df_features)
                if hasattr(clf3, "n_features_in_") and (
                    feature_mat.shape[1] != clf3.n_features_in_
                ):
                    raise ValueError(
                        f"Model expects {clf3.n_features_in_} columns, "
                        f"but generated {feature_mat.shape[1]}"
                    )

                output_path = output_dir / f"{tile_path.stem}_{chunk_index}.laz"
                cut.initial_predict(feature_mat, output_path, output_data,
                                    clf3, le)
                chunk_index += 1
                print(f"Saved: {output_path}")

            except Exception as err:
                print(f"Error in {tile_path.name}, region {coords}: {err}")
                continue


# Example integration in your existing script after loading tile_list/clf3/le:
# run_classification(tile_list, clf3, le)
