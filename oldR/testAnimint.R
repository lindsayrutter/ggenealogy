library(ggplot2)

# Animint does not support all of ggplot2’s theme() options - but it does support removing the axes, labels, and axis titles

# EXAMPLE 1 -- TIPS #############################################
data(tips, package = "reshape2")
tips$sex_smoker <- with(tips, interaction(sex, smoker))
p <- ggplot() + geom_point(data = tips, aes(x = total_bill, y = tip, colour = sex_smoker))
p
#The animint2dir() function is most useful for local development and quickly iterating your animint plots. This function compiles a list of ggplot objects (and a list of other options), compiles them, and write a set of files to a directory called "simple".
library(animint)
animint2dir(list(plot = p), out.dir = "simple", open.browser = FALSE)
# To view the result, you’ll probably want to start a local file server in the "simple" directory. This can be done with the servr package
servr::httd("simple") # press ESC to stop server & resume R

# Two aes used in animint are clickSelects and showSelected
p1 <- ggplot() + theme(legend.position = "none") + geom_point(data = tips, aes(x = sex, y = smoker, clickSelects = sex_smoker, colour = sex_smoker), position = "jitter")
p2 <- ggplot() + geom_point(data = tips, aes(x = total_bill, y = tip, showSelected = sex_smoker, colour = sex_smoker))
plots <- list(plot1 = p1, plot2 = p2)
# Doesn't seem to work (this is only for Knitr?)
#structure(plots, class = "animint")
# Try this instead
animint2dir(plots, out.dir = "simple2", open.browser = FALSE)

# EXAMPLE 2 -- TORNADO #############################################
library("plyr")
library("maps")
data(UStornadoes, package = "animint")
# Get map called "state" from maps package into DF format
USpolygons$state = state.abb[match(USpolygons$region, tolower(state.name))]

map <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = USpolygons, fill = "black", colour = "grey") + geom_segment(aes(x = startLong, y = startLat, xend = endLong, yend = endLat, showSelected = year), colour = "#55B1F7", data = UStornadoes) + ggtitle("Tornadoes in the US")

ts <- ggplot() + stat_summary(aes(year, year, clickSelects = year), data = UStornadoes, fun.y = length, geom = "bar") + ggtitle("Number of Recorded Tornadoes, 1950-2006") + ylab("Number of Tornadoes") + xlab("Year")

# Theme_animint() requires Toby's Fork of ggplot2
devtools::install_github("tdhock/ggplot2")
# Specify width to be 970px
map <- map + theme_animint(width = 970)
tornado.bar <- list(map = map, ts = ts) 
animint2dir(tornado.bar, out.dir = "simple3", open.browser = FALSE)
# Be sure to open simple3/index.html and not simple3/scripts.html

# EXAMPLE 3 -- TORNADO PART 2 #############################################
library(plyr)
UStornadoCounts <- ddply(UStornadoes, .(state, year), summarize, count=length(state))

# The make_text() function included in animint makes it easy to create text describing what has been selected. In this case, we would like to display the year on the US map, and we would like to show the state on the bar chart. This interactivity does not work with ggtitle() at this time, but we can create a “title” element on the plot itself instead using make_text
# Syntax is make_text(data, x, y, label.var, format=NULL) where format can be specified using a string containing %d, %f, etc. to represent the variable value

map <- ggplot() + make_text(UStornadoCounts, -100, 50, "year", "Tornadoes in %d") + geom_polygon(aes(x = long, y = lat, group = group, clickSelects = state), data = USpolygons, fill = "black", colour = "grey") + geom_segment(aes(x = startLong, y = startLat, xend = endLong, yend = endLat, showSelected = year), colour = "#55B1F7", data = UStornadoes) + theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()) + theme_animint(width = 970)

