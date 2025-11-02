library(StereoMorph)
library(geomorph)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# List your shape files
shape_files <- c(
  "data/shapes/EP2019-ScribOugaritPaleo_img_3358.txt",
  "data/shapes/EP2019-ScribOugaritPaleo_img_3366.txt",
  "data/shapes/EP2019-ScribOugaritPaleo_img_3376.txt"
)

# Read all shapes
shapes <- readShapes(shape_files)

# Extract landmark matrices
landmarks_list <- shapes

plot(NULL, xlim=c(0, 1000), ylim=c(0, 1000), xlab="X", ylab="Y", asp=1, main="Shape Landmarks")
colors <- c("red", "blue", "green")
for(i in seq_along(landmarks_list)) {
  mat <- as.matrix(landmarks_list[[i]])
  if(ncol(mat) < 2) {
    warning(paste("Shape", i, "does not have two columns, skipping."))
    next
  }
  points(mat[,1], mat[,2], col=colors[i], pch=16)
  text(mat[,1], mat[,2], labels=rownames(mat), cex=0.6, pos=3, col=colors[i])
}
legend("topright", legend=basename(shape_files), col=colors, pch=16)

# Combine into 3D array for geomorph
coords_array <- array(NA, dim = c(nrow(landmarks_list[[1]]), 2, length(landmarks_list)))
for(i in seq_along(landmarks_list)) coords_array[,,i] <- as.matrix(landmarks_list[[i]])
# Procrustes alignment
gpa <- gpagen(coords_array)
plot(gpa$coords, main="Procrustes Aligned Shapes")
