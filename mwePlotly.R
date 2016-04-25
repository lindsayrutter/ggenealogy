library(ggplot2)
library(plotly)

set.seed(1)
# Create data frame for 10 text node labels
labelDF = data.frame(label = paste(("label"), c(1:10), sep=""), x = runif(10, 1, 10), y = runif(10, 1, 10))
# Create data frame for 10 edges
edgeDF = data.frame(x = runif(10, 1, 10), y = runif(10, 1, 10), xend = runif(10, 1, 10), yend = runif(10, 1, 10))

# Create plot with 10 black node labels (that are too small to read) and 10 pink edges
myPlot = ggplot2::ggplot(data = labelDF, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_segment(data = edgeDF, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), colour = "pink") +
  ggplot2::geom_text(data = labelDF, ggplot2::aes(x = x, y = y, label = label), size=0.5)

# Try to create an interactive plot so that hovering over the unreadably small black node labels will interactively reveal their labels, but so that hovering over the pink edges will not interactively reveal anything. However, hovering over the pink edges still interactively reveals a NA label.
ggplotly(myPlot, tooltip = "label")
