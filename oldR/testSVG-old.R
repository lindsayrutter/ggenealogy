
###################################################################

library(grid)
library(gridSVG)

##################### GRID.ANIMINT DEMO ######################

# Creating two rectangles using one graphics object
grid.rect(x = c(0.3, 0.7), y = 0.4, width = 0.2, height = 0.2, gp = gpar(fill = "black"))

# Finding out the name of the object, in this case it is GRID.rect.1
grid.ls()

# Creating an animUnit which described y-position of our rects at each point in time. We make the first rectangle (which has an id of 1), stays at a y value of 0.4 for 3 time periods. The other rectangle, whose id is 2, is going to be animated from 0.4, to 0.7, and back to 0.4 again.
yunits <- animUnit(unit(c(rep(0.4, 3), 0.4, 0.7, 0.4), "npc"), id = rep(1:2, each = 3))

# Animating our rectange object GRID.rect.1 along the y axis using our animUnit yunits
grid.animate("GRID.rect.1", y = yunits, rep = TRUE)

# Drawing to SVG
grid.export("example.svg")

##################### GRID.GARNISH DEMO ######################
#The function grid.garnish() allows a user to add arbitrary attributes to grid graphics objects. These attributes are then applied to the SVG element that the graphics object translates to. This is most useful for adding JavaScript event attributes to grid graphics objects. Examples of such attributes are onmouseover, onmousemove, onclick and onmouseout. The values of these attributes are often JavaScript functions that are called when the appropriate event is triggered. These functions can be defined and included with the use of grid.script().

# Drawing a solid black rectangle
grid.rect(width = 0.25, height = 0.25, gp = gpar(fill = "black"))

# Determining the name of the rectangle. Here it is GRID.rect.3
grid.ls()

# Adding interactivity for the event where a mouse click occurs on GRID.rect.1
grid.garnish("GRID.rect.3",onclick = "alert('Example of interactivity with gridSVG.')")

# Drawing to SVG
grid.export("example2.svg")

#################### GRID.HYPERLINK DEMO ####################

grid.text("SJP", name = "labeltext", gp = gpar(fontsize = 144, col = "lightblue"))
grid.hyperlink("labeltext", "https://sjp.co.nz/", show = "new")
grid.export("example3.svg")

############### Interactive scatterplot ####################
library(lattice)
PC1 <- runif(10, -40, 10)
PC2 <- runif(10, -20, 20)
#group <- factor(sample(c("Estrogen Receptor Negative", "Estrogen Receptor Positive"), 10, replace=TRUE))

# These circles have a naming scheme applied to them so that we can reference them later, which is simply point.[index].
customPanel <- function(x, y, groups, ...) {
  grps <- levels(groups)
  for (i in 1:length(grps)) {
    index <- which(groups == grps[i])
    xx <- x[index]
    yy <- y[index]
    for (j in 1:length(xx)) {
      grid.circle(xx[j], yy[j], r = unit(1, "mm"),
                  default.unit = "native",
                  name = paste("point", index[j], sep = "."),
                  gp = gpar(col = NA,
                            fill = trellis.par.get("superpose.symbol")$col[i]))
    }
  }
}

customPanel2 <- function(x, y, ...) {
  for (j in 1:10) {
    grid.circle(x[j], y[j], r = unit(1, "mm"),
                default.unit = "native",
                name = paste("point", j, sep = "."),
                gp = gpar(col = NA,
                          fill = trellis.par.get("superpose.symbol")$col[j]))
  }
}

# The lattice package’s xyplot() function is used to draw the plot. We are using the customPanel function we defined earlier.
xyplot(PC2 ~ PC1, panel = customPanel2, xlab = "my x lab", ylab = "my y lab")

# We then draw 10 of grid’s textGrobs to show the name of each of the points. Each of the text labels are drawn at the bottom-left of the plot. These textGrobs also have a naming scheme applied to them, the names applied are of the form label.[index].1.1 (where index is 1—10). The plot currently appears messy as several text labels are drawn on top of each other, this will be remedied later.
for (i in 1:10) {
  grid.text(paste("myLabel", i), x = 0.1, y = 0.01, just = c("left", "bottom"), name = paste("label", i, sep = "."), gp = gpar(fontface = "bold.italic"))
}

for (i in 1:10) {
  grid.garnish(paste("point", i, sep = "."), onmouseover = paste('highlight("', i, '.1.1")', sep = ""), onmouseout = paste('dim("', i, '.1.1")', sep = ""))
  grid.garnish(paste("label", i, sep = "."), visibility = "hidden")
}

grid.script(filename = "aqm.js", inline = TRUE)
grid.export("aqm.svg")

