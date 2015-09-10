load("data/statGeneal.rda")
getChild("Christian Robert",statGeneal)
ig <- dfToIG(statGeneal)
getYear("Nicolas Chopin",statGeneal)
getAncestors("Chana Lowenstein",statGeneal,3)
getParent("Shlomo Sawilowsky",statGeneal)
getDegree("Boris Shulkin", "R. Clifford Blair", ig, statGeneal)
getBasicStatistics(ig)
statGeneal[which(statGeneal$child=="Boris Shulkin"),]

pathCC = getPath("Chana Lowenstein","R. Clifford Blair", ig, statGeneal, isDirected=FALSE)
# Have not dealt with cases where year is NA
# Possibly imputation?? (halfway between parent and child)
plotPath(pathCC)

# Probably don't need to write out full name (just points/dots)
# Important points emphasized (are all descended from a few people? )
# Standard network (remove year), but could see people who have lots of advisees (ggNet)

plotPathOnAll(pathCC, statGeneal, ig, binVector = 1:3)

plotPathOnAll(pathCC, statGeneal, ig, binVector = 1:6)

plotPathOnAll(pathCC, statGeneal, ig, binVector = 1:100)

