######################### Example network ##########################

if(require(maptools, quiet = TRUE) & require(lattice, quiet = TRUE) 
   & require(spdep, quiet = TRUE)) {
  
  columbus <- readShapePoly(system.file("etc/shapes/columbus.shp", package = "spdep")[1])
  coords <- coordinates(columbus)
  col.gal.nb <- read.gal(system.file("etc/weights/columbus.gal", package = "spdep")[1])
  nbobject <- col.gal.nb
  xyplot(coords[, 2] ~ coords[, 1],
         panel = function(...){adeg.panel.nb(col.gal.nb, coords, col.edge = c("blue", "red"))})
  
  edges <- matrix(c(1, 2, 3, 2, 4, 1, 3, 4), byrow = TRUE, ncol = 2)
  coords <- matrix(c(0, 1, 1, 0, 0, -1, -1, 0), byrow = TRUE, ncol = 2)
  xyplot(coords[,2] ~ coords[,1],
         panel = function(...){adeg.panel.edges(edges, coords, lty = 1:4, cex = 5)})
}


