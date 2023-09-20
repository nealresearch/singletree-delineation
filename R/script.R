library("lidR")
library("raster")
library(rgl)
library(dplyr)
library(RANN)
library(terra)
library(sf)


lasMainData <- readLAS("M:\\lidar\\las\\building.las")
st_crs(lasMainData) <- 32633


#Classify the data into ground and non-ground points.
#This also removes the building points. As building points also have the characteristics of flat surface
lasGround <-
  classify_ground(lasMainData, algorithm = pmf(ws = 3, th = 5))
dtm <- rasterize_terrain(lasGround, 1, knnidw())
lasNormal <- lasGround - dtm
lasGroundClassified <-
  filter_poi(lasNormal, (Classification != 7) &
               (Classification != 2))
lasHeightClassified <-
  filter_poi(lasGroundClassified, Z > 0 & Z < 25)


# As there are wall points where only few points are clustered. We use nearest neighbor search to remove these points.
idl <- length(lasHeightClassified@data$X)
coords = data.frame(X = "X", Y = "Y")
xy <-
  data.frame(x = lasHeightClassified@data$X, y = lasHeightClassified@data$Y)
dist = nn2(
  xy,
  query = xy ,
  treetype = "kd",
  searchtype = "radius",
  radius = 1,
  k = 15
)
indices_nn3 <- apply(dist$nn.idx, 1, function(x) {
  x[x != 0]
})
indices_nn3 <-
  lapply(indices_nn3, function(x) {
    if (length(x) <= 5)
      return(x)
  })
indices_nn3 <- unlist(indices_nn3)

coords_nn3 <- lasHeightClassified@data[indices_nn3, c("X", "Y")]

column_listx <- as.list(coords_nn3$X)
column_listy <- as.list(coords_nn3$Y)
lasNearestNeigbhour <-
  filter_poi(lasHeightClassified,
             (!X %in% column_listx) & (!Y %in% column_listy))

#Using the palanar detection method we remove the edges where there are point which have the charateristics of a

Rcpp::sourceCpp(
  code = "
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
SEXP eigen_values(arma::mat A) {
arma::mat coeff;
arma::mat score;
arma::vec latent;
arma::princomp(coeff, score, latent, A);

return(Rcpp::wrap(latent));
}"
)

plane_metrics2 = function(x, y, z, th1 = 5, th2 = 3) {
  xyz <- cbind(x, y, z)
  eigen_m <- eigen_values(xyz)
  is_planar <-
    eigen_m[2] > (th1 * eigen_m[3]) &&
    (th2 * eigen_m[2]) > eigen_m[1]
  return(list(planar = is_planar))
}

M <-
  point_metrics(
    lasNearestNeigbhour,
    ~ plane_metrics2(X, Y, Z),
    k = 5 ,
    filter = ~ ReturnNumber == 1
  )

#> Computed in 0.5 seconds
is_planar = M$eigen_medium > (5 * M$eigen_smallest) &
  (6 * M$eigen_medium) > M$eigen_largest

# Use the filter argument to process only first returns

all.equal(M$planar, lasNearestNeigbhour$planar)

lasSegmentPlane <-
  segment_shapes(lasNearestNeigbhour, shp_plane(k = 5), "planar")
lasPlanar <- filter_poi(lasSegmentPlane, planar == FALSE)

# The walls points can be detected using a line segment algorithm
lasSegmentline <-
  segment_shapes(lasPlanar, shp_line(k = 3), "line")
lasLine <- filter_poi(lasSegmentline, line == FALSE)



chm <-
  rasterize_canopy(lasLine, res = 0.5, p2r(0.5, na.fill = tin()))
gf <- focalWeight(chm, .2, "Gauss")
chm <- focal(chm, w = gf)
lasSegmentTrees <-
  segment_trees(lasLine, watershed(
    chm = chm,
    th_tree = 10,
    tol = 1,
    ext = 1
  ))
lasFilterZero <- filter_poi(lasSegmentTrees, !(is.na(treeID)))

# The walls point are clustered but the number of points in a cluster is less.
# We loop through every cluster and remove those who have less than 150 cluster of points.
p <- list()
ids = base::unique(lasFilterZero$treeID)
idl <- length(ids)
i = 1
while (i <= idl) {
  las_i <- filter_poi(lasFilterZero, treeID == i)
  dataHeader <- header(las_i)
  nop <- dataHeader@PHB
  points <- nop$`Number of point records`
  
  if (points <= 150)
  {
    p[[i]] <- i
    
  }
  i = i + 1
  
}


treeidlist <- p[lengths(p) != 0]
lasThinned <- filter_poi(lasFilterZero, !treeID %in% treeidlist)


i = 1
ids = base::unique(lasThinned$treeID)
idl <- length(ids)
noplist <- list()
finallist <- list()

while (i <= idl) {
  las_dist <- filter_poi(lasThinned, treeID == i)
  
  dataHeader <- header(las_dist)
  nop <- dataHeader@PHB
  points <- nop$`Number of point records`
  
  
  templist <- list()
  
  las_x <- las_dist@data$X
  xidl <- length(las_x)
  
  las_y <- las_dist@data$Y
  
  las_z <- las_dist@data$Z
  
  zidx <- base::which.max(las_z)
  
  j <- 1
  
  while (j <= xidl) {
    x1 <- las_x[zidx]
    y1 <- las_y[zidx]
    z1 <- las_z[zidx]
    
    x2 <- las_x[j]
    y2 <- las_y[j]
    z2 <-  las_z[j]
    
    euclidean <- function(x1, x2, y1, y2)
      sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
    
    distance <- euclidean(x1, x2, y1, y2)
    
    templist[[j]] <- list(x2, y2, distance)
    
    
    j <- j + 1
    
    
  }
  
  thirdelt <- sapply(templist, function(x)
    x[[3]])
  thirdelt_order <- order(thirdelt)
  templist <- templist[thirdelt_order]
  
  temp_length = length(templist)
  removed_data = tail(templist, round(temp_length * 0.15))
  
  
  finallist <- append(finallist, removed_data)
  
  
  i = i + 1
}


xlist <- lapply(finallist, "[[", 1)
las <- filter_poi(lasThinned, !X %in% xlist)
