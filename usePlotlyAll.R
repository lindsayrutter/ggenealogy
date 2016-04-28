#' Plot a path between two vertices over the full genealogy
#' 
#' This function requires a path and the ig object, and plots the full genealogy 
#' with the path highlighted.
#' The image will correctly position the node labels with x-axis representing the node
#' year, and y-axis representing the node path index. Light grey edges between two nodes
#' represent parent-child relationships between those nodes. To enhance the visual
#' understanding of how the path-of-interest fits into the entire graph structure, the
#' nodes within the path are labelled in boldface, and connected with light-green
#' boldfaced edges.
#' @param path path as returned from getPath() or a vector of two variety names which exist in ig
#' @param geneal the full genealogy  (in data frame format)
#' @param ig the graph representation of the data genealogy (in igraph format)
#' @param binVector vector of numbers between 1 and length(binVector), each repeated exactly once
#' @param edgeCol color of the non-path edges, default is "gray84"
#' @param pathEdgeCol color of the path edges, default is "seagreen"
#' @param nodeSize text size of the non-path node labels, default is 3
#' @param pathNodeSize text size of the path node labels, default is 3
#' @param pathNodeFont font face of text of the path node labels ("plain", "italic", "bold", "bold.italic"), default is "bold"
#' @param nodeLabel If non-path nodes should be in text, default is TRUE. If FALSE, then non-path nodes will be represented as dots, which could conserve space and reduce text overlap in large datasets.
#' @param nodeCol color of the non-path node labels, default is black
#' @examples
#' data(sbGeneal)
#' ig <- dfToIG(sbGeneal)
#' path <- getPath("Brim", "Bedford", ig, sbGeneal)
#' binVector <- sample(1:12, 12)
#' plotTotalImage <- plotPathOnAll(path = path, geneal = sbGeneal, ig = ig, binVector= sample(1:12, 12))
#' plotTotalImage
#' @seealso \url{http://www.r-project.org} for iGraph information
#' @seealso \code{\link{getPath}} for information on input path building
#' @export
#' 
plotPathOnAll = function(path, geneal, ig, binVector=sample(1:12, 12), edgeCol = "gray84", pathEdgeCol = "seagreen", nodeSize = 3, pathNodeSize = 3, pathNodeFont = "bold", nodeLabel = TRUE, nodeCol = "black"){
  x <- y <- xend <- yend <- xstart <- ystart <- label <- NULL
  if(class(ig)!="igraph"){
    stop("ig must be an igraph object")
  }
  
  if(mode(path)=="character"){
    if(length(path)!=2){
      stop("path needs to contain two variety names")
    }
    varieties <- path
    path <- getPath(varieties[1], varieties[2], ig)
  } else if(sum(names(path)%in%c("pathVertices", "yearVertices"))!=2){
    stop("path does not appear to be a result of the getPath() function")
  } 
  
  pMPDF <- buildMinusPathDF(path, geneal, ig, binVector)
  eTDF <- buildEdgeTotalDF(geneal, ig, binVector)
  pTDF <- buildPlotTotalDF(path, geneal, ig, binVector)
  
  eTDF <- na.omit(eTDF) #remove any row that has at least one NA
  
  textFrame = data.frame(x = pMPDF$x, y = pMPDF$y, label = pMPDF$label)
  textFrame = transform(textFrame,
                        w = strwidth(pMPDF$label, 'inches') + 0.25,
                        h = strheight(pMPDF$label, 'inches') + 0.25
  )
  
  textFrame <- na.omit(textFrame) #remove any row that has at least one NA
  
  # The plotTotalImage object creates two line segments (geom_segment), one to create grey
  # edges for non-path connections between pairs of nodes, the other to create light-green
  # edges for path connections between pairs of nodes; and two labels (geom_text), one to
  # create labels of size 2 for non-path connections between pairs of nodes, the other to
  # create labels of size 2.5 and boldfaced for path connections between pairs of nodes.
  plotTotalImage = ggplot2::ggplot(data = pMPDF, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_segment(data = eTDF, ggplot2::aes(x=x, y=y-.1, xend=xend, yend=yend+.1), colour = edgeCol) +
    ggplot2::geom_segment(data = pTDF, ggplot2::aes(x=xstart, y=ystart, xend=xend, yend=yend), colour = pathEdgeCol, size = 1) +
    if (nodeLabel){
      ggplot2::geom_text(data = textFrame, ggplot2::aes(x = x, y = y, label = label), size = nodeSize, colour = nodeCol)
    }else{
      ggplot2::geom_text(data = textFrame, ggplot2::aes(x = x, y = y, label = label), size = nodeSize, colour = nodeCol) # changed label = "." to label
    }
  plotTotalImage = plotTotalImage + ggplot2::geom_text(data = pTDF,ggplot2::aes(x = x, y = y, label = label), size = pathNodeSize, fontface=pathNodeFont) +
    ggplot2::xlab("Year") +
    # Erase the y-axis, and only include grids from the x-axis
    ggplot2::theme(axis.text.y=ggplot2::element_blank(),axis.ticks.y=ggplot2::element_blank(),
                   axis.title.y=ggplot2::element_blank(),legend.position="none",
                   panel.grid.major.y=ggplot2::element_blank(),
                   panel.grid.minor=ggplot2::element_blank())
  # Return the plotTotalImage
  plotTotalImage
}

############## MWE posted to https://github.com/ropensci/plotly/issues ##############

data(statGeneal)
statIG <- dfToIG(statGeneal)
pathCB <- getPath("David Cox", "Petra Buzkova", statIG, statGeneal, isDirected = FALSE)

# I believe it highlights not only the desired geom_text(), but also the geom_segment() as well. I tested this by coloring the edges pink, and see that they all have NA values since the geom_segment() does not have labels. Hence, I need to find a way so that ggplotly only provides interaction for the specified geom.

myPlot = plotPathOnAll(pathCB, statGeneal, statIG, binVector = 1:200, nodeSize = .5, pathNodeSize = 2.5, nodeLabel=FALSE, nodeCol = "dimgray", edgeCol = "pink") + ggplot2::theme(axis.text = ggplot2::element_text(size = 12), axis.title = ggplot2::element_text(size = 12)) + ggplot2::scale_x_continuous(expand = c(.1, .2))

l <- plotly_build(ggplotly(myPlot, tooltip = "label"))
l$data[[1]]$hoverinfo <- "none"
l$data[[2]]$hoverinfo <- "none"
l
