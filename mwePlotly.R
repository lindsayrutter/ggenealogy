# MWE for geom duplication in plotly

set.seed(1)
labelDF = data.frame(label = paste(("label"), c(1:10), sep=""), x = runif(10, 1, 10), y = runif(10, 1, 10))
edgeDF = data.frame(x = runif(10, 1, 10), y = runif(10, 1, 10), xend = runif(10, 1, 10), yend = runif(10, 1, 10))

myPlot = ggplot2::ggplot(data = labelDF, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_segment(data = edgeDF, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), colour = "pink") +
  ggplot2::geom_text(data = labelDF, ggplot2::aes(x = x, y = y, label = label))

ggplotly(myPlot, tooltip = c("label"))
