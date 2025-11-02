# GMM Analysis Script

# Load necessary libraries
library(StereoMorph)
library(geomorph)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

#Digitize 2D landmarks with Stereomorph
shapes_dir <- file.path(getwd(), "data", "shapes")
shape_files <- list.files(shapes_dir, pattern = "\\.txt$", full.names = TRUE)

# Define landmark types and number of sets
landmark_types <- c("tail_vertex", "left_vertex", "right_vertex", "depth_point")
n_sets <- 9

# Generate landmark names programmatically
lands <- as.vector(outer(landmark_types, sprintf("%02d", 1:n_sets), paste0))

# Read all shapes
wedges <- StereoMorph::readShapes(shape_files)
fixed <- wedges$landmarks.scaled
fixed

#Name curves
curves<-matrix(c("condtocor", "condtoang","condyle","condyle","coronoid", "angular"), nrow=2,ncol=3)

# Read all shapes
wedges <- StereoMorph::readShapes(shape_files)

# Each element in 'wedges' is a landmark matrix
# Combine into a 3D array for geomorph

sapply(wedges, nrow)

expected_n <- 21 # or set manually if you know the correct number
wedges_fixed <- Filter(function(x) nrow(x) == expected_n, wedges)

n_landmarks <- expected_n
n_dim <- ncol(wedges[[1]])
n_specimens <- length(wedges)
fixed <- array(NA, dim = c(n_landmarks, n_dim, n_specimens))
for(i in seq_along(wedges)) {
  fixed[,,i] <- as.matrix(wedges[[i]])
}

# Procrustes fit
gpa.lands <- gpagen(fixed)
plotAllSpecimens(fixed)
plot(gpa.lands)
