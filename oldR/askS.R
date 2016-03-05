library(grid)
library(gridSVG)
library(lattice)
library(adegraphics)

x = rnorm(10)
y = rnorm(10)
dat = data.frame(label = letters[1:10], x, y)

customPanel2 <- function(x, y, ...) {
  for (j in 1:nrow(dat)) {
    grid.circle(x[j], y[j], r = unit(.5, "mm"),
                default.unit = "native",
                name = paste("point", j, sep = "."))
  }
}

#Original
#xyplot(coords[,2] ~ coords[,1],panel = function(...){adeg.panel.edges(edges, coords, lty = 1:4, cex = 5)})

xyplot(y ~ x, panel = customPanel2, xlab = "x variable", ylab=NULL, scales=list(tck = c(1,0), y=list(at=NULL)))

edges = matrix(c(1, 2, 3, 2, 4, 1, 3, 4), byrow = TRUE, ncol = 2)
coords <- matrix(c(x[1], y[1], x[2], y[2], x[3], y[3], x[4], y[4]), byrow = TRUE, ncol = 2)

xyplot(y ~ x, xlab = "x variable", ylab=NULL, scales=list(tck = c(1,0), y=list(at=NULL)), panel = customPanel2(...,{adeg.panel.edges(edges, coords, lty = 1:4, cex = 5)}))

edges <- matrix(c(1, 2, 3, 2, 4, 1, 3, 4), byrow = TRUE, ncol = 2)
coords <- matrix(c(0, 1, 1, 0, 0, -1, -1, 0), byrow = TRUE, ncol = 2)
xyplot(coords[,2] ~ coords[,1],panel = function(customPanel2){adeg.panel.edges(edges, coords, lty = 1:4, cex = 5)})

for (i in 1:nrow(dat)) {
  grid.text(as.character(dat$label)[i], x = 0.1, y = 0.01, just = c("left", "bottom"), name = paste("label", i, sep = "."), gp = gpar(fontface = "bold.italic"))
}

for (i in 1:nrow(dat)) {
  grid.garnish(paste("point", i, sep = "."), onmouseover = paste('highlight("', i, '.1.1")', sep = ""), onmouseout = paste('dim("', i, '.1.1")', sep = ""))
  grid.garnish(paste("label", i, sep = "."), visibility = "hidden")
}

grid.script(filename = "aqm.js", inline = TRUE)
grid.export("interactiveScat.svg")
