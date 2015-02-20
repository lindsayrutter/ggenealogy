## ------------------------------------------------------------------------
options(warn=-1)
library(ggenealogy)
library(ggplot2)
library(stringr)
library(knitr)

## ------------------------------------------------------------------------
data(sbTree)
head(sbTree)

## ------------------------------------------------------------------------
ig <- processTreeGraph(sbTree)

## ------------------------------------------------------------------------
ig <- processTreeGraph(sbTree, vertexinfo=c("year", "yield", "year.imputed", "min.repro.year"))

## ------------------------------------------------------------------------
library(plyr)
nodes <- unique(sbTree[,1:5])

# get a data frame of all parents whose parents are not known (i.e. parents who are not listed as children as well)
extra.nodes <- unique(data.frame(child=sbTree$parent[!sbTree$parent%in%sbTree$child & !is.na(sbTree$parent)], stringsAsFactors=FALSE))

# We may not have information for these extra nodes, but they still need to be included in the dataset
nodes <- rbind.fill(nodes, extra.nodes)
rm(extra.nodes)

# We can now specify our vertex information using the data frame nodes: 
ig <- processTreeGraph(sbTree, vertexinfo=nodes)

## ------------------------------------------------------------------------
isParent("Young","Essex",sbTree)
isParent("Essex","Young",sbTree)

## ------------------------------------------------------------------------
isChild("Young","Essex",sbTree)
isChild("Essex","Young",sbTree)

## ------------------------------------------------------------------------
getYear("Young",sbTree)
getYear("Essex",sbTree)

## ------------------------------------------------------------------------
getparent("Young",sbTree)
getparent("Tokyo",sbTree)
getYear("Tokyo", sbTree)

## ------------------------------------------------------------------------
getchild("Tokyo",sbTree)
getchild("Ogden",sbTree)

## ------------------------------------------------------------------------
getDegree("Tokyo", "Ogden", ig, sbTree)
getDegree("Tokyo", "Holladay", ig, sbTree)

## ------------------------------------------------------------------------
getBasicStatistics(ig)

## ------------------------------------------------------------------------
getPath("Brim","Bedford", ig, sbTree, isDirected=FALSE)

## ------------------------------------------------------------------------
dirgraph <- processTreeGraph(sbTree, vertexinfo=nodes, isDirected = TRUE)
getPath("Brim", "Bedford", dirgraph, isDirected=TRUE)

## ------------------------------------------------------------------------
getPath("Bedford", "Brim", dirgraph, isDirected=TRUE)

## ------------------------------------------------------------------------
path <- getPath("Brim","Bedford", ig, sbTree, isDirected=F)

## ------------------------------------------------------------------------
plotPathDF <- plotPath(path)

## ------------------------------------------------------------------------
dirgraph <- processTreeGraph(sbTree, vertexinfo=nodes, isDirected = TRUE)

path <- getPath("Narow", "Tokyo", dirgraph, sbTree, isDirected=TRUE)
plotPathImage <- plotPath(path)
plotPathImage

## ------------------------------------------------------------------------
binVector <- c(1,4,7,10,2,5,8,11,3,6,9,12)
spreadTotalDF <- buildSpreadTotalDF(ig, binVector)
head(spreadTotalDF)

## ------------------------------------------------------------------------
plotMinusPathDF <- buildMinusPathDF(path, ig)
head(plotMinusPathDF)

## ------------------------------------------------------------------------
edgeTotalDF <- buildEdgeTotalDF(ig)
head(edgeTotalDF)

## ------------------------------------------------------------------------
plotTotalDF <- buildPlotTotalDF(path, ig)
head(plotTotalDF)

## ------------------------------------------------------------------------
plotTotalImage <- plotPathOnTree(path=path, ig=ig)
plotTotalImage

## ------------------------------------------------------------------------
plotTotalImage <- plotPathOnTree(path=path, ig=ig, binVector=1:12)

## ------------------------------------------------------------------------
plotTotalImage <- plotPathOnTree(path=path, ig=ig, binVector=1:2)

## ------------------------------------------------------------------------
varieties <- c("Beeson", "Brim", "Dillon", "Narow", "Zane", "Hood", "York", "Calland", "Columbus", "Crawford", "Kershaw", "Kent","Bragg", "Davis","Tokyo","Hagood","Young","Essex","Holladay","Cook","Century","Pella","Forrest","Gasoy","Cutler")
heatMapDegreeImage <- plotDegMatrix(varieties,ig,sbTree)
heatMapDegreeImage

## ------------------------------------------------------------------------
varieties <- c("Beeson", "Brim", "Dillon", "Narow", "Zane", "Hood", "York", "Calland", "Columbus", "Crawford", "Kershaw", "Kent","Bragg", "Davis","Tokyo","Hagood","Young","Essex","Holladay","Cook","Century","Pella","Forrest","Gasoy","Cutler")
heatMapYearImage <- plotYearMatrix(varieties,sbTree)
heatMapYearImage

