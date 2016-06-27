### R code from vignette source 'ggenealogy.Rnw'

###################################################
### code chunk number 1: ggenealogy.Rnw:52-53
###################################################
rm(list=ls())


###################################################
### code chunk number 2: ggenealogy.Rnw:74-75 (eval = FALSE)
###################################################
## install.packages("ggenealogy")


###################################################
### code chunk number 3: ggenealogy.Rnw:81-82
###################################################
library(ggenealogy)


###################################################
### code chunk number 4: ggenealogy.Rnw:87-88 (eval = FALSE)
###################################################
## help(package="ggenealogy")


###################################################
### code chunk number 5: ggenealogy.Rnw:93-94 (eval = FALSE)
###################################################
## help(getChild)


###################################################
### code chunk number 6: ggenealogy.Rnw:99-102 (eval = FALSE)
###################################################
## data(sbGeneal)
## getChild("Tokyo", sbGeneal)
## getChild("Essex", sbGeneal)


###################################################
### code chunk number 7: ggenealogy.Rnw:109-113
###################################################
data(sbGeneal)
head(sbGeneal)
dim(sbGeneal)
str(sbGeneal)


###################################################
### code chunk number 8: ggenealogy.Rnw:120-121
###################################################
help(dfToIG)


###################################################
### code chunk number 9: ggenealogy.Rnw:128-130
###################################################
ig <- dfToIG(sbGeneal)
class(ig)


###################################################
### code chunk number 10: ggenealogy.Rnw:143-145
###################################################
isParent("Young","Essex",sbGeneal)
isParent("Essex","Young",sbGeneal)


###################################################
### code chunk number 11: ggenealogy.Rnw:150-152
###################################################
isChild("Young","Essex",sbGeneal)
isChild("Essex","Young",sbGeneal)


###################################################
### code chunk number 12: ggenealogy.Rnw:157-159
###################################################
getYear("Young",sbGeneal)
getYear("Essex",sbGeneal)


###################################################
### code chunk number 13: ggenealogy.Rnw:164-167
###################################################
getParent("Young",sbGeneal)
getParent("Tokyo",sbGeneal)
getYear("Tokyo", sbGeneal)


###################################################
### code chunk number 14: ggenealogy.Rnw:172-174
###################################################
getChild("Tokyo",sbGeneal)
getChild("Ogden",sbGeneal)


###################################################
### code chunk number 15: ggenealogy.Rnw:183-184
###################################################
getAncestors("Young",sbGeneal,1)


###################################################
### code chunk number 16: ggenealogy.Rnw:189-191
###################################################
getAncestors("Young",sbGeneal,5)
nrow(getAncestors("Young",sbGeneal,5))


###################################################
### code chunk number 17: ggenealogy.Rnw:198-199
###################################################
getDescendants("Ogden",sbGeneal,1)


###################################################
### code chunk number 18: ggenealogy.Rnw:204-205
###################################################
getDescendants("Ogden",sbGeneal,2)


###################################################
### code chunk number 19: ggenealogy.Rnw:214-216
###################################################
getDegree("Tokyo", "Ogden", ig, sbGeneal)
getDegree("Tokyo", "Holladay", ig, sbGeneal)


###################################################
### code chunk number 20: ggenealogy.Rnw:227-228
###################################################
getBasicStatistics(ig)


###################################################
### code chunk number 21: ggenealogy.Rnw:235-238
###################################################
eList = getEdges(ig, sbGeneal)
head(eList)
nrow(eList)


###################################################
### code chunk number 22: ggenealogy.Rnw:243-246
###################################################
nList = getNodes(sbGeneal)
head(nList)
length(nList)


###################################################
### code chunk number 23: plotAncDes1
###################################################
plotAncDes("Lee", sbGeneal,5,4)


###################################################
### code chunk number 24: plotAncDes1
###################################################
plotAncDes("Lee", sbGeneal,5,4)


###################################################
### code chunk number 25: plotAncDes2
###################################################
plotAncDes("Tokyo", sbGeneal, vColor = "blue") + ggplot2::labs(x="Generation index",y="")


###################################################
### code chunk number 26: plotAncDes2
###################################################
plotAncDes("Tokyo", sbGeneal, vColor = "blue") + ggplot2::labs(x="Generation index",y="")


###################################################
### code chunk number 27: ggenealogy.Rnw:309-310
###################################################
getPath("Brim","Bedford", ig, sbGeneal, isDirected=FALSE)


###################################################
### code chunk number 28: ggenealogy.Rnw:315-317 (eval = FALSE)
###################################################
## dirIG = dfToIG(sbGeneal, isDirected = TRUE)
## getPath("Brim", "Bedford", dirIG, sbGeneal, isDirected = TRUE)


###################################################
### code chunk number 29: ggenealogy.Rnw:322-323 (eval = FALSE)
###################################################
## getPath("Bedford", "Brim", dirIG, sbGeneal, isDirected=TRUE)


###################################################
### code chunk number 30: ggenealogy.Rnw:330-331
###################################################
pathBB = getPath("Bedford","Brim", ig, sbGeneal, isDirected=FALSE)


###################################################
### code chunk number 31: plotPath1
###################################################
plotPath(pathBB)


###################################################
### code chunk number 32: plotPath1
###################################################
plotPath(pathBB)


###################################################
### code chunk number 33: plotPath2
###################################################
pathNT = getPath("Narow", "Tokyo", ig, sbGeneal, isDirected=FALSE)
plotPath(pathNT)


###################################################
### code chunk number 34: plotPath2
###################################################
pathNT = getPath("Narow", "Tokyo", ig, sbGeneal, isDirected=FALSE)
plotPath(pathNT)


###################################################
### code chunk number 35: ggenealogy.Rnw:377-379 (eval = FALSE)
###################################################
## pathNT = getPath("Narow", "Tokyo", dirIG, sbGeneal, isDirected=TRUE)
## plotPath(pathNT)


###################################################
### code chunk number 36: ggenealogy.Rnw:382-384 (eval = FALSE)
###################################################
## pathTN = getPath("Tokyo", "Narow", dirIG, sbGeneal, isDirected=TRUE)
## plotPath(pathTN)


###################################################
### code chunk number 37: plotPathOnAll1
###################################################
plotPathOnAll(pathNT, sbGeneal, ig, binVector = 1:3)


###################################################
### code chunk number 38: plotPathOnAll1
###################################################
plotPathOnAll(pathNT, sbGeneal, ig, binVector = 1:3)


###################################################
### code chunk number 39: plotPathOnAll2
###################################################
plotPathOnAll(pathNT, sbGeneal, ig, binVector = 1:6)


###################################################
### code chunk number 40: plotPathOnAll2
###################################################
plotPathOnAll(pathNT, sbGeneal, ig, binVector = 1:6)


###################################################
### code chunk number 41: plotDegMatrix1
###################################################
varieties=c("Brim", "Bedford", "Calland", "Narow", "Pella", "Tokyo", "Young", "Zane")
p = plotDegMatrix(varieties, ig, sbGeneal, "Soybean label", "Soybean label", "Degree")
p + ggplot2::scale_fill_continuous(low="white", high="darkgreen")


###################################################
### code chunk number 42: plotDegMatrix1
###################################################
varieties=c("Brim", "Bedford", "Calland", "Narow", "Pella", "Tokyo", "Young", "Zane")
p = plotDegMatrix(varieties, ig, sbGeneal, "Soybean label", "Soybean label", "Degree")
p + ggplot2::scale_fill_continuous(low="white", high="darkgreen")


###################################################
### code chunk number 43: ggenealogy.Rnw:471-472
###################################################
getDegree("Bedford", "Zane", ig, sbGeneal)


###################################################
### code chunk number 44: plotYearMatrix1
###################################################
varieties=c("Brim", "Bedford", "Calland", "Narow", "Pella", "Tokyo", "Young", "Zane")
plotYearMatrix(varieties,sbGeneal)


###################################################
### code chunk number 45: plotYearMatrix1
###################################################
varieties=c("Brim", "Bedford", "Calland", "Narow", "Pella", "Tokyo", "Young", "Zane")
plotYearMatrix(varieties,sbGeneal)


###################################################
### code chunk number 46: ggenealogy.Rnw:503-506
###################################################
data("statGeneal")
dim(statGeneal)
colnames(statGeneal)


###################################################
### code chunk number 47: ggenealogy.Rnw:515-522
###################################################
library("dplyr")
indVec <- getNodes(statGeneal)
indVec <- indVec[which(indVec != "", )]
dFunc <- function(var) nrow(getDescendants(var, statGeneal, gen = 100))
numDesc <- sapply(indVec, dFunc)
table(numDesc)
which(numDesc == 159)


###################################################
### code chunk number 48: dCox
###################################################
plotAncDes("David Cox", statGeneal, mAnc = 6, mDes = 6, vCol = "blue")


###################################################
### code chunk number 49: dCox
###################################################
plotAncDes("David Cox", statGeneal, mAnc = 6, mDes = 6, vCol = "blue")


###################################################
### code chunk number 50: ggenealogy.Rnw:544-546
###################################################
length(getChild("Peter Bloomfield", statGeneal))
nrow(getDescendants("Peter Bloomfield", statGeneal, gen = 100))


###################################################
### code chunk number 51: pathCB
###################################################
statIG <- dfToIG(statGeneal)
pathCB <- getPath("David Cox", "Petra Buzkova", statIG, statGeneal,
  isDirected = FALSE)
plotPath(pathCB, fontFace = 4) + ggplot2::theme(axis.text =
  ggplot2::element_text(size = 10), axis.title =
  ggplot2::element_text(size = 10)) + ggplot2::scale_x_continuous(expand
  = c(.1, .2))


###################################################
### code chunk number 52: pathCB
###################################################
statIG <- dfToIG(statGeneal)
pathCB <- getPath("David Cox", "Petra Buzkova", statIG, statGeneal,
  isDirected = FALSE)
plotPath(pathCB, fontFace = 4) + ggplot2::theme(axis.text =
  ggplot2::element_text(size = 10), axis.title =
  ggplot2::element_text(size = 10)) + ggplot2::scale_x_continuous(expand
  = c(.1, .2))


###################################################
### code chunk number 53: plotCBText
###################################################
plotPathOnAll(pathCB, statGeneal, statIG, binVector = 1:200) +
   ggplot2::theme(axis.text = ggplot2::element_text(size = 8), axis.title =
   ggplot2::element_text(size = 8)) + ggplot2::scale_x_continuous(expand = c(.1, .2))


###################################################
### code chunk number 54: plotCBText
###################################################
plotPathOnAll(pathCB, statGeneal, statIG, binVector = 1:200) +
   ggplot2::theme(axis.text = ggplot2::element_text(size = 8), axis.title =
   ggplot2::element_text(size = 8)) + ggplot2::scale_x_continuous(expand = c(.1, .2))


###################################################
### code chunk number 55: plotCBNoText
###################################################
plotPathOnAll(pathCB, statGeneal, statIG, binVector = 1:200, nodeSize = .5,
   pathNodeSize = 2.5, nodeCol = "darkgray", edgeCol = "lightgray") +
   ggplot2::theme(axis.text = ggplot2::element_text(size = 8), axis.title =
   ggplot2::element_text(size = 8)) + ggplot2::scale_x_continuous(expand = c(.1, .2))


###################################################
### code chunk number 56: plotCBNoText
###################################################
plotPathOnAll(pathCB, statGeneal, statIG, binVector = 1:200, nodeSize = .5,
   pathNodeSize = 2.5, nodeCol = "darkgray", edgeCol = "lightgray") +
   ggplot2::theme(axis.text = ggplot2::element_text(size = 8), axis.title =
   ggplot2::element_text(size = 8)) + ggplot2::scale_x_continuous(expand = c(.1, .2))


