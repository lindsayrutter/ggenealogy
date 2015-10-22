#library(ggplot2)
#map <- ggplot() + 
  #geom_polygon(aes(x = long, y = lat, group = group), 
  #             data = USpolygons, fill = "black", colour = "grey") +
  #geom_segment(aes(x = startLong, y = startLat, xend = endLong, yend = endLat, showSelected = year), 
   #            colour = "#55B1F7", data = UStornadoes) +
  #ggtitle("Tornadoes in the US") + 
  #theme(axis.line = element_blank(), axis.text = element_blank(), 
  #      axis.ticks = element_blank(), axis.title = element_blank()) +
  #theme_animint(width = 970)

#tornado.bar <- list(map = map, ts = ts) 
#animint2dir(tornado.bar, "tornado-bar3")