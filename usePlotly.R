
########## New plotPath() function in ggenealogy using geom_label() instead of geom_rect()

plotPath = function(path){
  x <- y <- label <- xstart <- ystart <- xend <- yend <- NULL
  if(sum(names(path)%in%c("pathVertices", "yearVertices"))!=2){
    stop("path does not appear to be a result of the getPath() function")
  }

  pPDF <- buildPathDF(path)
  colnames(pPDF)[which(colnames(pPDF)=="x")] = "Year"
  #ggplot2::ggplot(data = pPDF,ggplot2::aes(x = x, y = y, label = label)) + geom_label()

  if (length(dim(pPDF))>1){ # check to make sure pPDF is a data frame
  
    plotPathImage = ggplot2::ggplot(data = pPDF,ggplot2::aes(x = x, y = y, label=label)) +
    ggplot2::geom_segment(ggplot2::aes(x=xstart, y=ystart, xend=xend, yend=yend)) +
    ggplot2::geom_label(fill = "grey80", size = 3) +
    ggplot2::xlab("Year") +
    ggplot2::scale_x_continuous(expand = c(.1, .1)) +
    ggplot2::theme(axis.text.y=ggplot2::element_blank(),axis.ticks.y=ggplot2::element_blank(),
                   axis.title.y=ggplot2::element_blank(),legend.position="none",
                   panel.grid.major.y=ggplot2::element_blank(),
                   panel.grid.minor=ggplot2::element_blank())
}
else{
  plotPathImage = print("There is no path to display between the two inputted vertices.")
  plotPathImage = NA
}
# Return the plotImage
plotPathImage
}
  
############## MWE posted to https://github.com/ropensci/plotly/issues ##############
  
dat=data.frame(label=paste0("label",seq(1:5)), xstart = seq(10,50, by=10), ystart = seq(1:5)+.1, xend = c(seq(20,50, by=10), 50), yend = c(seq(1:4)+.9, 5.1), x = seq(10,50, by=10), y=seq(1:5))

myPlot = ggplot2::ggplot(data = dat,ggplot2::aes(x = x, y = y, label=label)) +
  ggplot2::geom_segment(ggplot2::aes(x=xstart, y=ystart, xend=xend, yend=yend)) +
  ggplot2::geom_label(fill = "grey80", size = 3) +
  ggplot2::scale_x_continuous(expand = c(.1, .1))

ggplotly(myPlot, tooltip = c("x", "label"))
