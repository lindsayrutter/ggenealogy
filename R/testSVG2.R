library(grid)
library(gridSVG)
library(lattice)

############### Interactive scatterplot ####################
load("data/statGeneal.rda")
statGeneal = statGeneal[1:300,]
ig <- dfToIG(statGeneal)
pathCC = getPath("Carl Morris","Cindy Christiansen", ig, statGeneal, isDirected=FALSE)

pMPDF <- buildMinusPathDF(pathCC, statGeneal, ig, 1:100)
pMPDF = pMPDF[!is.na(pMPDF$x),]
pMPDF = pMPDF[!is.na(pMPDF$label),]
coord1 <- pMPDF$x
coord2 <- pMPDF$y

customPanel2 <- function(x, y, ...) {
  for (j in 1:nrow(pMPDF)) {
    grid.circle(x[j], y[j], r = unit(.5, "mm"),
                default.unit = "native",
                name = paste("point", j, sep = "."))
  }
}

# The lattice package’s xyplot() function is used to draw the plot. We are using the customPanel function we defined earlier.
xyplot(coord2 ~ coord1, panel = function(customPanel2){adeg.panel.edges(edges, coords, lty = 1:4, cex = 5)}, xlab = "Year graduated", ylab=NULL, scales=list(tck = c(1,0), y=list(at=NULL)))

#coords <- matrix(c(1955,3,1975,5,1993,3,2005,1), byrow = TRUE, ncol = 2)
#edges <- matrix(c(1, 2, 3, 2, 4, 1, 3, 4), byrow = TRUE, ncol = 2)

#edges <- matrix(c(1, 2, 3, 2, 4, 1, 3, 4), byrow = TRUE, ncol = 2)
#coords <- matrix(c(0, 1, 1, 0, 0, -1, -1, 0), byrow = TRUE, ncol = 2)
#xyplot(coords[,2] ~ coords[,1],panel = function(...){adeg.panel.edges(edges, coords, lty = 1:4, cex = 5)})

# We then draw 10 of grid’s textGrobs to show the name of each of the points. Each of the text labels are drawn at the bottom-left of the plot.
for (i in 1:nrow(pMPDF)) {
  grid.text(as.character(pMPDF$label)[i], x = 0.1, y = 0.01, just = c("left", "bottom"), name = paste("label", i, sep = "."), gp = gpar(fontface = "bold.italic"))
}

# Takes long time
for (i in 1:nrow(pMPDF)) {
  grid.garnish(paste("point", i, sep = "."), onmouseover = paste('highlight("', i, '.1.1")', sep = ""), onmouseout = paste('dim("', i, '.1.1")', sep = ""))
  grid.garnish(paste("label", i, sep = "."), visibility = "hidden")
}

grid.script(filename = "aqm.js", inline = TRUE)
grid.export("testAll.svg")
