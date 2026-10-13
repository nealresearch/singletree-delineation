class Slice:
    def __init__(self, data_path):
        self.path = data_path
        self.las = laspy.read(path)

    def bounds(self):
        """
        Get the bounds of the lidar data which will be used to clip the lidar tiles into small chunk
        """
        return self.las.header.x_min, self.las.header.y_min, self.las.header.x_max, \
                                 self.las.header.y_max

    def coordinates(self):
        """
        Get the coordinates of the small chunks. We use bounds from the big lidar data and then extract small chunks
        """
        xcmin, ycmin, xcmax, ycmax = self.bounds()
        cords = []
        current_x = xcmin
        # Think of this in the axis plane. the xmin and ymin is at the bottom then we first increase the y axis then the xaxis by 100 m,
        # to get all the coordinates lidar data in small chunks
        while current_x < xcmax:
            current_y = ycmin
            while current_y < ycmax:
                cords.append([current_x, current_y, current_x+100, current_y+100])
                current_y += 100
            current_x += 100
        return cords

    def lidar_data(self, minx,  miny, maxx, maxy):
        """
        :param minx:
        :param miny:
        :param maxx:
        :param maxy:
        :return: the small chunk of lidar data, which is 100x100 meter
        """
        condition = (self.las.x >= minx) & (self.las.x <= maxx) & (self.las.y >= miny) & (self.las.y <= maxy)
        filtered_points = self.las[condition]
        return filtered_points

    @staticmethod
    def initial_predict(matrix, out_path, las, model, trans, classification_filter=None):
        """
        :param self:
        :param matrix:
        :param out_path:
        :param las:
        :param model:
        :param trans:
        :param classification_filter:
        :return:
        """
        try:
            lidar_data = laspy.read(las)
        except AttributeError as err:
            lidar_data = las

        # Use the loaded model to make predictions
        loaded_y_pred = model.predict(matrix)
        y_pred = trans.inverse_transform(loaded_y_pred)

        if classification_filter is not None:
            lidar_data.classification[classification_filter] = y_pred.astype(np.uint8)
        else:
            lidar_data.classification = loaded_y_pred.astype(np.uint8)
            
        output_las_file = out_path
        lidar_data.write(output_las_file)

        return "Data created"

    @staticmethod
    def iteration_predict(self, matrix, output, las, classification_filter, gb_clf, le):
        """

        :param self:
        :param matrix: features dataframe as matrix
        :param output: las data with classification
        :param las: lidar data
        :param classification_filter: only pass the vegetations
        :param gb_clf: trained model
        :param le: function to inverse the class transformation
        :return:
        """
        lidar_data = las
        # Create a new LAS data
        new_lidar_data = laspy.create(point_format=lidar_data.point_format)

        # Copy over the points from the original file
        new_lidar_data.points = lidar_data.points.copy()

        # Copy over the header
        new_lidar = laspy.LasData(lidar_data.header)

        # predict the classification
        loaded_y_pred = gb_clf.predict(matrix)
        y_pred_i = le.inverse_transform(loaded_y_pred)
        # Update the classification for the filtered points
        new_lidar_data.classification[classification_filter] = y_pred_i.astype(np.uint8)

        # Write the new LiDAR data to a .las file
        new_lidar_data.write(output)

    @staticmethod
    def impute_dataframe(dataframes):
        """
        :param dataframes:
        :return: Matrix
        """
        feature_matrix = dataframes.values
        feature_matrix = np.where(np.isinf(feature_matrix), np.nan, feature_matrix)
        imputer = SimpleImputer(strategy="mean")
        feature_matrix_clean = imputer.fit_transform(feature_matrix)

        return feature_matrix_clean


if __name__ == "__main__":
    
    folder_path = "M:\\phd\\digi\\" #"M:\\lidar\\stadt_daten\\braunschweig\\citytile_normalized\\"
    try:
        for tile in tile_list:
            path = os.path.join(folder_path, tile)
            cut = Slice(path)
            dataframe_list = []
            list_cords = cut.coordinates()
            cords_length = list_cords
            start = False
            # we loop over the nested list of the cordinates and use the inner list as coordinates to cut the lidar data 
            for idx, item in enumerate(list_cords):
                try:
                    xmin, ymin, xmax, ymax = item
                    out_data = cut.lidar_data(xmin, ymin, xmax, ymax)
                    output_list =  (out_data['classification'] != 0) & (out_data['classification'] != 1) & (out_data['classification'] != 2) 
                    output_data = out_data[output_list]
                    if len(output_data) <= 100:
                        continue
                    inPoint = np.vstack((output_data.x, output_data.y, output_data.z)).T
                    # radius for creating the covariacne matrix. We loop over this as well
                    rad = [0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
                    processes = []
                    queue = Queue()
                    eigen_val_dict = {}
                    eigen_vec_dict = {}
                    
                    # looping over the radius.This is needed to ensure we run the calculation in parallel
                    for r in rad:
                        # Create a separate KDTree for each process to avoid serialization issues
                        kdtree = KDTree(inPoint)
                        # Start a process for each value of rad
                        p = Process(target=calculate_eigen, args=(inPoint, r, queue, kdtree))
                        processes.append(p)
                        p.start()
                        print("process start")
                    # here we loop over the radius again but here just to get result from the queue
                    for j in rad:
                        # Retrieve results from the queue
                        results = queue.get()
                        eigen_values = results[0]
                        eigen_vector = results[1]
                        eigen_val_dict[j] = eigen_values
                        eigen_vec_dict[j] = eigen_vector
            
                    # Ensure all processes have finished
                    for p in processes:
                        p.join()
                
                    names = ['planarity', 'verticality', 'sphericity', 'anisotropy', 'surfaceVariation', 'omnivariance', 'linearity',
                             'curvature', 'small_eigen']
            
                    # now we create a nested dictionary to hold the values for each radius. example d {{1: planar, sphericity...}, {2: planar, sphericity}}
                    vec_new_dict = {}
                    for key, value in eigen_vec_dict.items():
                        vec_new_dict[key] = {}
                        
                    # we use the same idea of nested dictonary here.
                    new_dict = {}
                    for key, value in eigen_val_dict.items():
                        new_dict[key] = {}
                
                    for k, val in eigen_vec_dict.items():
                        vec_new_dict[k][names[1]] = calculate_verticality(inPoint, val)
            
                    # As we have already calculated the eigen values. We use this values to now calculate the predicitive feature
                    for key, value in eigen_val_dict.items():
                        # new_dict[key][names[0]] = calculate_pc(inPoint, value)
                        new_dict[key][names[0]] = calculate_planar(inPoint, value)
                        new_dict[key][names[2]] = calculate_sphericity(inPoint, value)
                        new_dict[key][names[3]] = calculate_anisotropy(inPoint, value)
                        new_dict[key][names[4]] = calculate_surface_variation(inPoint, value)
                        new_dict[key][names[5]] = calculate_omnivariance(inPoint, value)
                        new_dict[key][names[6]] = calculate_linearity(inPoint, value)
                        new_dict[key][names[7]] = calculate_curvature(inPoint, value)
                        # we use np.real to convert complex number to real number
                        new_dict[key][names[8]] = np.real_if_close(np.array([val[0] for val in value]))
            
                    # now we just extract the features from dictionary and insert it into the dataframe
                    df_vec = pd.DataFrame()
                    # first we loop over the outer values(radius)
                    for r, m in vec_new_dict.items():
                        #Then we loop over the inner values. the features
                        for m_n, v in m.items():
                            c_name = f"{m_n}{r}"
                            df_vec[c_name] = v
                
                    df_val = pd.DataFrame()
                    for radius, metrics in new_dict.items():
                        for metric_name, values in metrics.items():
                            column_name = f"{metric_name}{radius}"
                            df_val[column_name] = values
                
                    df = pd.DataFrame()
                
                    for radius, metrics in new_dict.items():
                        for metric_name, values in metrics.items():
                            column_name = f"{metric_name}{radius}"
                            df[column_name] = values
               
                    df = pd.concat([df_val, df_vec], axis=1)
                    df["Z"] = np.array(output_data.z)
                    df_z = df.drop(['Z', 'small_eigen0.6'], axis=1)
                    feature_mat = cut.impute_dataframe(df_z)
                    if not start:
                        i = 0
                    # M:\\lidar\\stadt_daten\\braunschweig\\citytile_classified\\{tile}_{i}.laz
                    output_path = f"M:\\phd\\classified\\{tile}_{i}.laz"
                    cut.initial_predict(feature_mat, output_path, output_data, clf3, le)
                    start = True
                    i += 1
                except Exception as err:
                    print("error:", err)
                    continue
    except Exception as er:
        print("Error:", er)
