# Quick test to see what structure landmarks have
options(rgl.useNULL = TRUE)
library(StereoMorph)

setwd("/home/user/Cuneimorph")

shape_files <- list.files("data/shapes", pattern = "\\.txt$", full.names = TRUE)
shapes <- readShapes(shape_files)

cat("Structure of shapes$landmarks.pixel:\n")
str(shapes$landmarks.pixel)

cat("\n\nFirst landmark set:\n")
print(shapes$landmarks.pixel[[1]])

cat("\n\nClass:\n")
print(class(shapes$landmarks.pixel[[1]]))

cat("\n\nDimensions:\n")
print(dim(shapes$landmarks.pixel[[1]]))

cat("\n\nAs matrix:\n")
m <- as.matrix(shapes$landmarks.pixel[[1]])
print(dim(m))
print(head(m))
