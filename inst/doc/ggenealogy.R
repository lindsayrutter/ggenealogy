### R code from vignette source 'ggenealogy.Rnw'

###################################################
### code chunk number 1: ggenealogy.Rnw:55-56
###################################################
rm(list=ls())


###################################################
### code chunk number 2: ggenealogy.Rnw:91-92
###################################################
library(ggenealogy)


###################################################
### code chunk number 3: ggenealogy.Rnw:97-99 (eval = FALSE)
###################################################
## help(package="ggenealogy")
## help(ggenealogy)


###################################################
### code chunk number 4: ggenealogy.Rnw:108-109 (eval = FALSE)
###################################################
## example(ggenealogy)


###################################################
### code chunk number 5: ggenealogy.Rnw:116-119
###################################################
data(sbGeneal)
head(sbGeneal)
class(sbGeneal)


###################################################
### code chunk number 6: ggenealogy.Rnw:126-127
###################################################
help(dfToIG)


###################################################
### code chunk number 7: ggenealogy.Rnw:134-136
###################################################
ig <- dfToIG(sbGeneal)
class(ig)


###################################################
### code chunk number 8: ggenealogy.Rnw:143-144
###################################################
ig <- dfToIG(sbGeneal, vertexinfo=c("year", "yield", "year.imputed"))


###################################################
### code chunk number 9: ggenealogy.Rnw:149-161
###################################################
library(plyr)
nodes <- unique(sbGeneal[,1:4])

# get a data frame of all parents whose parents are not known (i.e. parents who are not listed as children as well)
extra.nodes <- unique(data.frame(child=sbGeneal$parent[!sbGeneal$parent%in%sbGeneal$child & !is.na(sbGeneal$parent)], stringsAsFactors=FALSE))

# We may not have information for these extra nodes, but they still need to be included in the dataset
nodes <- rbind.fill(nodes, extra.nodes)
rm(extra.nodes)

# We can now specify our vertex information using the data frame nodes: 
ig <- dfToIG(sbGeneal, vertexinfo=nodes)


###################################################
### code chunk number 10: ggenealogy.Rnw:176-178
###################################################
isParent("Young","Essex",sbGeneal)
isParent("Essex","Young",sbGeneal)


###################################################
### code chunk number 11: ggenealogy.Rnw:185-187
###################################################
isChild("Young","Essex",sbGeneal)
isChild("Essex","Young",sbGeneal)


###################################################
### code chunk number 12: ggenealogy.Rnw:194-196
###################################################
getYear("Young",sbGeneal)
getYear("Essex",sbGeneal)


###################################################
### code chunk number 13: ggenealogy.Rnw:203-206
###################################################
getParent("Young",sbGeneal)
getParent("Tokyo",sbGeneal)
getYear("Tokyo", sbGeneal)


###################################################
### code chunk number 14: ggenealogy.Rnw:213-215
###################################################
getChild("Tokyo",sbGeneal)
getChild("Ogden",sbGeneal)


###################################################
### code chunk number 15: ggenealogy.Rnw:224-225
###################################################
getAncestors("Young",sbGeneal,1)


###################################################
### code chunk number 16: ggenealogy.Rnw:232-234
###################################################
getAncestors("Young",sbGeneal,5)
dim(getAncestors("Young",sbGeneal,5))


###################################################
### code chunk number 17: ggenealogy.Rnw:241-242
###################################################
getDescendants("Ogden",sbGeneal,1)


###################################################
### code chunk number 18: ggenealogy.Rnw:249-250
###################################################
getDescendants("Ogden",sbGeneal,2)


###################################################
### code chunk number 19: ggenealogy.Rnw:259-261
###################################################
getDegree("Tokyo", "Ogden", ig, sbGeneal)
getDegree("Tokyo", "Holladay", ig, sbGeneal)


###################################################
### code chunk number 20: ggenealogy.Rnw:272-273
###################################################
getBasicStatistics(ig)


###################################################
### code chunk number 21: ggenealogy.Rnw:280-283
###################################################
eList = getEdges(ig, sbGeneal)
head(eList)
dim(eList)


###################################################
### code chunk number 22: ggenealogy.Rnw:288-291
###################################################
nList = getNodes(sbGeneal)
head(nList)
length(nList)


###################################################
### code chunk number 23: plotAncDes1
###################################################
plotAncDes("Lee", sbGeneal, 3, 5)


###################################################
### code chunk number 24: plotAncDes1
###################################################
plotAncDes("Lee", sbGeneal, 3, 5)


###################################################
### code chunk number 25: plotAncDes2
###################################################
plotAncDes("Tokyo", sbGeneal, vColor = "blue") + ggplot2::labs(x="Generation index",y="")


###################################################
### code chunk number 26: plotAncDes2
###################################################
plotAncDes("Tokyo", sbGeneal, vColor = "blue") + ggplot2::labs(x="Generation index",y="")


###################################################
### code chunk number 27: ggenealogy.Rnw:354-355
###################################################
getPath("Brim","Bedford", ig, sbGeneal, isDirected=FALSE)


###################################################
### code chunk number 28: ggenealogy.Rnw:362-364
###################################################
dirIG = dfToIG(sbGeneal, vertexinfo=nodes, isDirected = TRUE)
getPath("Brim", "Bedford", dirIG, sbGeneal, isDirected = TRUE)


###################################################
### code chunk number 29: ggenealogy.Rnw:369-370
###################################################
getPath("Bedford", "Brim", dirIG, sbGeneal, isDirected=TRUE)


###################################################
### code chunk number 30: ggenealogy.Rnw:377-378
###################################################
path = getPath("Bedford","Brim", ig, sbGeneal, isDirected=FALSE)


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
path = getPath("Narow", "Tokyo", ig, sbGeneal, isDirected=FALSE)
plotPath(path)


###################################################
### code chunk number 34: plotPath2
###################################################
path = getPath("Narow", "Tokyo", ig, sbGeneal, isDirected=FALSE)
plotPath(path)


###################################################
### code chunk number 35: ggenealogy.Rnw:422-424
###################################################
path = getPath("Narow", "Tokyo", dirIG, sbGeneal, isDirected=TRUE)
plotPath(path)


###################################################
### code chunk number 36: plotPath3
###################################################
path = getPath("Tokyo", "Narow", dirIG, sbGeneal, isDirected=TRUE)
plotPath(path)


###################################################
### code chunk number 37: plotPath3
###################################################
path = getPath("Tokyo", "Narow", dirIG, sbGeneal, isDirected=TRUE)
plotPath(path)


###################################################
### code chunk number 38: plotPathOnAll1
###################################################
plotPathOnAll(path, sbGeneal, ig, binVector = 1:3)


###################################################
### code chunk number 39: plotPathOnAll1
###################################################
plotPathOnAll(path, sbGeneal, ig, binVector = 1:3)


###################################################
### code chunk number 40: plotPathOnAll2
###################################################
plotPathOnAll(path, sbGeneal, ig, binVector = 1:6)


###################################################
### code chunk number 41: plotPathOnAll2
###################################################
plotPathOnAll(path, sbGeneal, ig, binVector = 1:6)


###################################################
### code chunk number 42: plotDegMatrix1
###################################################
varieties=c("Brim", "Bedford", "Calland", "Narow", "Pella", "Tokyo", "Young", "Zane")
p = plotDegMatrix(varieties, ig, sbGeneal, "Soybean label", "Soybean label", "Degree")
p + ggplot2::scale_fill_continuous(low="white", high="darkgreen")


###################################################
### code chunk number 43: plotDegMatrix1
###################################################
varieties=c("Brim", "Bedford", "Calland", "Narow", "Pella", "Tokyo", "Young", "Zane")
p = plotDegMatrix(varieties, ig, sbGeneal, "Soybean label", "Soybean label", "Degree")
p + ggplot2::scale_fill_continuous(low="white", high="darkgreen")


###################################################
### code chunk number 44: ggenealogy.Rnw:520-521
###################################################
getDegree("Bedford", "Zane", ig, sbGeneal)


###################################################
### code chunk number 45: plotYearMatrix1
###################################################
varieties=c("Brim", "Bedford", "Calland", "Narow", "Pella", "Tokyo", "Young", "Zane")
plotYearMatrix(varieties,sbGeneal)


###################################################
### code chunk number 46: plotYearMatrix1
###################################################
varieties=c("Brim", "Bedford", "Calland", "Narow", "Pella", "Tokyo", "Young", "Zane")
plotYearMatrix(varieties,sbGeneal)


