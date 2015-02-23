### R code from vignette source 'ggenealogy.Rnw'

###################################################
### code chunk number 1: ggenealogy.Rnw:55-56
###################################################
rm(list=ls())


###################################################
### code chunk number 2: ggenealogy.Rnw:85-86
###################################################
library(ggenealogy)


###################################################
### code chunk number 3: ggenealogy.Rnw:91-93 (eval = FALSE)
###################################################
## help(package="ggenealogy")
## help(ggenealogy)


###################################################
### code chunk number 4: ggenealogy.Rnw:102-103 (eval = FALSE)
###################################################
## example(ggenealogy)


###################################################
### code chunk number 5: ggenealogy.Rnw:110-113
###################################################
data(sbTree)
head(sbTree)
class(sbTree)


###################################################
### code chunk number 6: ggenealogy.Rnw:120-121
###################################################
help(treeToIG)


###################################################
### code chunk number 7: ggenealogy.Rnw:128-130
###################################################
ig <- treeToIG(sbTree)
class(ig)


###################################################
### code chunk number 8: ggenealogy.Rnw:137-138
###################################################
ig <- treeToIG(sbTree, vertexinfo=c("year", "yield", "year.imputed"))


###################################################
### code chunk number 9: ggenealogy.Rnw:143-155
###################################################
library(plyr)
nodes <- unique(sbTree[,1:4])

# get a data frame of all parents whose parents are not known (i.e. parents who are not listed as children as well)
extra.nodes <- unique(data.frame(child=sbTree$parent[!sbTree$parent%in%sbTree$child & !is.na(sbTree$parent)], stringsAsFactors=FALSE))

# We may not have information for these extra nodes, but they still need to be included in the dataset
nodes <- rbind.fill(nodes, extra.nodes)
rm(extra.nodes)

# We can now specify our vertex information using the data frame nodes: 
ig <- treeToIG(sbTree, vertexinfo=nodes)


###################################################
### code chunk number 10: ggenealogy.Rnw:170-172
###################################################
isParent("Young","Essex",sbTree)
isParent("Essex","Young",sbTree)


###################################################
### code chunk number 11: ggenealogy.Rnw:179-181
###################################################
isChild("Young","Essex",sbTree)
isChild("Essex","Young",sbTree)


###################################################
### code chunk number 12: ggenealogy.Rnw:188-190
###################################################
getYear("Young",sbTree)
getYear("Essex",sbTree)


###################################################
### code chunk number 13: ggenealogy.Rnw:197-200
###################################################
getParent("Young",sbTree)
getParent("Tokyo",sbTree)
getYear("Tokyo", sbTree)


###################################################
### code chunk number 14: ggenealogy.Rnw:207-209
###################################################
getChild("Tokyo",sbTree)
getChild("Ogden",sbTree)


###################################################
### code chunk number 15: ggenealogy.Rnw:218-219
###################################################
getAncestors("Young",sbTree,1)


###################################################
### code chunk number 16: ggenealogy.Rnw:226-228
###################################################
getAncestors("Young",sbTree,5)
dim(getAncestors("Young",sbTree,5))


###################################################
### code chunk number 17: ggenealogy.Rnw:235-236
###################################################
getDescendants("Ogden",sbTree,1)


###################################################
### code chunk number 18: ggenealogy.Rnw:243-244
###################################################
getDescendants("Ogden",sbTree,2)


###################################################
### code chunk number 19: ggenealogy.Rnw:253-255
###################################################
getDegree("Tokyo", "Ogden", ig, sbTree)
getDegree("Tokyo", "Holladay", ig, sbTree)


###################################################
### code chunk number 20: ggenealogy.Rnw:266-267
###################################################
getBasicStatistics(ig)


###################################################
### code chunk number 21: ggenealogy.Rnw:274-277
###################################################
eList = getEdges(ig, sbTree)
head(eList)
dim(eList)


###################################################
### code chunk number 22: ggenealogy.Rnw:282-285
###################################################
nList = getNodes(sbTree)
head(nList)
length(nList)


###################################################
### code chunk number 23: plotAncDes1
###################################################
plotAncDes("Lee", sbTree, 3, 5)


###################################################
### code chunk number 24: plotAncDes1
###################################################
plotAncDes("Lee", sbTree, 3, 5)


###################################################
### code chunk number 25: plotAncDes2
###################################################
plotAncDes("Tokyo", sbTree, vColor = "blue") + ggplot2::labs(x="Generation index",y="")


###################################################
### code chunk number 26: plotAncDes2
###################################################
plotAncDes("Tokyo", sbTree, vColor = "blue") + ggplot2::labs(x="Generation index",y="")


###################################################
### code chunk number 27: ggenealogy.Rnw:348-349
###################################################
getPath("Brim","Bedford", ig, sbTree, isDirected=FALSE)


###################################################
### code chunk number 28: ggenealogy.Rnw:356-358
###################################################
dirIG = treeToIG(sbTree, vertexinfo=nodes, isDirected = TRUE)
getPath("Brim", "Bedford", dirIG, sbTree, isDirected = TRUE)


###################################################
### code chunk number 29: ggenealogy.Rnw:363-364
###################################################
getPath("Bedford", "Brim", dirIG, sbTree, isDirected=TRUE)


###################################################
### code chunk number 30: ggenealogy.Rnw:371-372
###################################################
path = getPath("Bedford","Brim", ig, sbTree, isDirected=FALSE)


###################################################
### code chunk number 31: plotPath1
###################################################
plotPath(path)


###################################################
### code chunk number 32: plotPath1
###################################################
plotPath(path)


###################################################
### code chunk number 33: plotPath2
###################################################
path = getPath("Narow", "Tokyo", ig, sbTree, isDirected=FALSE)
plotPath(path)


###################################################
### code chunk number 34: plotPath2
###################################################
path = getPath("Narow", "Tokyo", ig, sbTree, isDirected=FALSE)
plotPath(path)


###################################################
### code chunk number 35: ggenealogy.Rnw:416-418
###################################################
path = getPath("Narow", "Tokyo", dirIG, sbTree, isDirected=TRUE)
plotPath(path)


###################################################
### code chunk number 36: plotPath3
###################################################
path = getPath("Tokyo", "Narow", dirIG, sbTree, isDirected=TRUE)
plotPath(path)


###################################################
### code chunk number 37: plotPath3
###################################################
path = getPath("Tokyo", "Narow", dirIG, sbTree, isDirected=TRUE)
plotPath(path)


###################################################
### code chunk number 38: plotPathOnTree1
###################################################
plotPathOnTree(path, sbTree, ig, binVector = 1:3)


###################################################
### code chunk number 39: plotPathOnTree1
###################################################
plotPathOnTree(path, sbTree, ig, binVector = 1:3)


###################################################
### code chunk number 40: plotPathOnTree2
###################################################
plotPathOnTree(path, sbTree, ig, binVector = 1:6)


###################################################
### code chunk number 41: plotPathOnTree2
###################################################
plotPathOnTree(path, sbTree, ig, binVector = 1:6)


###################################################
### code chunk number 42: plotDegMatrix1
###################################################
varieties=c("Brim", "Bedford", "Calland", "Narow", "Pella", "Tokyo", "Young", "Zane")
p = plotDegMatrix(varieties, ig, sbTree, "Soybean label", "Soybean label", "Degree")
p + ggplot2::scale_fill_continuous(low="white", high="darkgreen")


###################################################
### code chunk number 43: plotDegMatrix1
###################################################
varieties=c("Brim", "Bedford", "Calland", "Narow", "Pella", "Tokyo", "Young", "Zane")
p = plotDegMatrix(varieties, ig, sbTree, "Soybean label", "Soybean label", "Degree")
p + ggplot2::scale_fill_continuous(low="white", high="darkgreen")


###################################################
### code chunk number 44: ggenealogy.Rnw:514-515
###################################################
getDegree("Bedford", "Zane", ig, sbTree)


###################################################
### code chunk number 45: plotYearMatrix1
###################################################
varieties=c("Brim", "Bedford", "Calland", "Narow", "Pella", "Tokyo", "Young", "Zane")
plotYearMatrix(varieties,sbTree)


###################################################
### code chunk number 46: plotYearMatrix1
###################################################
varieties=c("Brim", "Bedford", "Calland", "Narow", "Pella", "Tokyo", "Young", "Zane")
plotYearMatrix(varieties,sbTree)


