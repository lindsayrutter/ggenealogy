pkgname <- "ggenealogy"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ggenealogy')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("buildAncDesTotalDF")
### * buildAncDesTotalDF

flush(stderr()); flush(stdout())

### Name: buildAncDesTotalDF
### Title: Returns data frame with plot coordinates of all ancestors and
###   descendants of a variety.
### Aliases: buildAncDesTotalDF

### ** Examples

data(sbGeneal)
v1 <- "Essex"
buildAncDesTotalDF(v1, sbGeneal)



cleanEx()
nameEx("buildAncList")
### * buildAncList

flush(stderr()); flush(stdout())

### Name: buildAncList
### Title: Returns the ancestors of a particular variety (if they exist).
### Aliases: buildAncList

### ** Examples

data(sbGeneal)
getParent("Essex", sbGeneal)
buildAncList("Essex", sbGeneal)



cleanEx()
nameEx("buildDesList")
### * buildDesList

flush(stderr()); flush(stdout())

### Name: buildDesList
### Title: Returns the descendants of a particular variety (if they exist).
### Aliases: buildDesList

### ** Examples

data(sbGeneal)
getParent("Essex", sbGeneal)
buildDesList("Essex", sbGeneal, 3)



cleanEx()
nameEx("getAncestors")
### * getAncestors

flush(stderr()); flush(stdout())

### Name: getAncestors
### Title: Returns a list of the ancestors of a particular variety (if they
###   exist)
### Aliases: getAncestors

### ** Examples

data(sbGeneal)
getParent("Essex", sbGeneal)
getAncestors("Essex", sbGeneal, 1)
getAncestors("Essex", sbGeneal, 5)



cleanEx()
nameEx("getBasicStatistics")
### * getBasicStatistics

flush(stderr()); flush(stdout())

### Name: getBasicStatistics
### Title: Determine basic statistics of the graph object
### Aliases: getBasicStatistics

### ** Examples

data(sbGeneal)
ig <- dfToIG(sbGeneal)
getBasicStatistics(ig)



cleanEx()
nameEx("getBranchQual")
### * getBranchQual

flush(stderr()); flush(stdout())

### Name: getBranchQual
### Title: Descendant branch calculations for quantitative variable
### Aliases: getBranchQual

### ** Examples

data(statGeneal)
rExpr = "geneal$colName=='The Johns Hopkins University'"
DC_JHU = getBranchQual("David Cox", statGeneal, "school", rExpr, 15)
rExpr = "geneal$colName=='UnitedKingdom'"
DC_UK = getBranchQual("David Cox", statGeneal, "country", rExpr, 15)
rExpr = "grepl('(?i)Stochastic', geneal$colName)"
DC_Stochastic = getBranchQual("David Cox", statGeneal, "thesis", rExpr, 15)



cleanEx()
nameEx("getBranchQuant")
### * getBranchQuant

flush(stderr()); flush(stdout())

### Name: getBranchQuant
### Title: Descendant branch calculations for quantitative variable
### Aliases: getBranchQuant

### ** Examples

data(statGeneal)
DC_Year <- getBranchQuant("David Cox", statGeneal, "gradYear", 15)



cleanEx()
nameEx("getChild")
### * getChild

flush(stderr()); flush(stdout())

### Name: getChild
### Title: Returns the children of a particular variety (if they exist)
### Aliases: getChild

### ** Examples

data(sbGeneal)
getChild("Tokyo", sbGeneal)
getChild("Essex", sbGeneal)



cleanEx()
nameEx("getDegree")
### * getDegree

flush(stderr()); flush(stdout())

### Name: getDegree
### Title: Determine the degree between two varieties
### Aliases: getDegree

### ** Examples

data(sbGeneal)
ig <- dfToIG(sbGeneal)
getDegree("Brim", "Bedford", ig, sbGeneal)



cleanEx()
nameEx("getDescendants")
### * getDescendants

flush(stderr()); flush(stdout())

### Name: getDescendants
### Title: Returns a list of the descendants of a particular variety (if
###   they exist)
### Aliases: getDescendants

### ** Examples

data(sbGeneal)
getChild("Essex", sbGeneal)
getDescendants("Essex", sbGeneal, 1)
getDescendants("Essex", sbGeneal, 3)



cleanEx()
nameEx("getEdges")
### * getEdges

flush(stderr()); flush(stdout())

### Name: getEdges
### Title: Returns edges (vertex names and edge weights) for the full
###   genealogy
### Aliases: getEdges

### ** Examples

data(sbGeneal)
ig <- dfToIG(sbGeneal)
getEdges(ig, sbGeneal)



cleanEx()
nameEx("getNodes")
### * getNodes

flush(stderr()); flush(stdout())

### Name: getNodes
### Title: Returns the nodes for a full genealogy
### Aliases: getNodes

### ** Examples

data(sbGeneal)
getNodes(sbGeneal)



cleanEx()
nameEx("getParent")
### * getParent

flush(stderr()); flush(stdout())

### Name: getParent
### Title: Returns the parents of a particular variety (if they exist)
### Aliases: getParent

### ** Examples

data(sbGeneal)
getParent("Tokyo", sbGeneal)
getParent("Essex", sbGeneal)



cleanEx()
nameEx("getPath")
### * getPath

flush(stderr()); flush(stdout())

### Name: getPath
### Title: Determine the path between two varieties
### Aliases: getPath

### ** Examples

data(sbGeneal)
ig <- dfToIG(sbGeneal)
getPath("Brim", "Bedford", ig, sbGeneal, "devYear")
getPath("Tokyo", "Volstate", ig, sbGeneal, "yield")



cleanEx()
nameEx("getVariable")
### * getVariable

flush(stderr()); flush(stdout())

### Name: getVariable
### Title: Determine the date of a variety
### Aliases: getVariable

### ** Examples

data(sbGeneal)
getVariable("Essex", sbGeneal, "devYear")
getVariable("Tokyo", sbGeneal, "yield")



cleanEx()
nameEx("isChild")
### * isChild

flush(stderr()); flush(stdout())

### Name: isChild
### Title: Determine if a variety is a child of another
### Aliases: isChild

### ** Examples

data(sbGeneal)
isChild("Essex", "Young", sbGeneal)
isChild("Young", "Essex", sbGeneal)



cleanEx()
nameEx("isParent")
### * isParent

flush(stderr()); flush(stdout())

### Name: isParent
### Title: Determine if a variety is a parent of another
### Aliases: isParent

### ** Examples

data(sbGeneal)
isParent("Essex", "Young", sbGeneal)
isParent("Young", "Essex", sbGeneal)



cleanEx()
nameEx("plotAncDes")
### * plotAncDes

flush(stderr()); flush(stdout())

### Name: plotAncDes
### Title: Returns the image object to show the ancestors and descendants
###   of a variety
### Aliases: plotAncDes

### ** Examples

data(sbGeneal)
plotAncDes("Tokyo", sbGeneal, vColor = "red")
plotAncDes("Essex", sbGeneal, 2, 3, "blue") + ggplot2::labs(x = "Generation index", y = "")



cleanEx()
nameEx("plotDegMatrix")
### * plotDegMatrix

flush(stderr()); flush(stdout())

### Name: plotDegMatrix
### Title: Returns the image object to show the heat map of degrees between
###   the inputted set of vertices
### Aliases: plotDegMatrix

### ** Examples

data(sbGeneal)
ig <- dfToIG(sbGeneal)
varieties <- c("Bedford", "Calland", "Narow", "Pella", "Tokyo", "Young", "Zane")
p <- plotDegMatrix(varieties, ig, sbGeneal)
p + ggplot2::scale_fill_continuous(low = "white", high = "darkgreen")



cleanEx()
nameEx("plotPath")
### * plotPath

flush(stderr()); flush(stdout())

### Name: plotPath
### Title: Construct the graphic object of the path
### Aliases: plotPath

### ** Examples

data(sbGeneal)
ig <- dfToIG(sbGeneal)
pathTN <- getPath("Tokyo", "Narow", sbIG, sbGeneal, "devYear")
plotPath(pathTN, sbGeneal, "devYear")

sbFilt <- sbGeneal[complete.cases(sbGeneal[1:3]),]
sbFiltIG <- dfToIG(sbFilt)
pathCL <- getPath("Clark", "Lawrence", sbFiltIG, sbFilt, "yield")
plotPath(pathCL, sbFilt, "devYear", "yield") + ggplot2::xlab("Dev Year") + ggplot2::ylab("Yield")



cleanEx()
nameEx("plotPathOnAll")
### * plotPathOnAll

flush(stderr()); flush(stdout())

### Name: plotPathOnAll
### Title: Plot a path between two vertices over the full genealogy
### Aliases: plotPathOnAll

### ** Examples

data(sbGeneal)
sb <- sbGeneal[complete.cases(sbGeneal[1:3]),]
ig <- dfToIG(sb)
pathCL <- getPath("Clark", "Lawrence", ig, sb, "yield")
plotPathOnAll(pathCL, sb, ig, "yield", bin = 3, pathEdgeCol = "red") + ggplot2::xlab("Yield")
plotPathOnAll(pathCL, sb, ig, "yield", "devYear") + ggplot2::xlab("Yield") + ggplot2::ylab("Year")



cleanEx()
nameEx("plotVariableMatrix")
### * plotVariableMatrix

flush(stderr()); flush(stdout())

### Name: plotVariableMatrix
### Title: Returns the image object to show the heat map of dates between
###   the inputted set of vertices
### Aliases: plotVariableMatrix

### ** Examples

data(sbGeneal)
varieties <- c("Bedford", "Calland", "Narow", "Pella", "Tokyo", "Young", "Zane")
p <- plotVariableMatrix(varieties, sbGeneal, "devYear", "Variety", "Variety", "Difference")
p + ggplot2::scale_fill_continuous(low = "white", high = "darkgreen")



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
