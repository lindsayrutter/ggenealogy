library(devtools)
devtools::install_github("ropensci/plotly")
library(plotly)
library(ggthemes)

p <- qplot(mpg, data = mtcars)
ggplotly(p)

p <- qplot(mpg, data = mtcars, geom = "density")
ggplotly(p)

p <- qplot(mpg, fill = factor(cyl), data = mtcars, geom = "density")
# click on legend entries to hide/show densities
ggplotly(p)
# restrict the amount of tooltip info
ggplotly(p, mapping = c("x", "y"))

p <- qplot(factor(cyl), data = mtcars, geom = "bar")
ggplotly(p)

# map custom text (which if stat != 'identity', creates new groups)
p <- qplot(factor(cyl), text = row.names(mtcars), data = mtcars, geom = "bar")
ggplotly(p)
# Thanks for nothing qplot()!
p <- ggplot(mtcars, aes(factor(cyl), text = row.names(mtcars))) + geom_bar()
ggplotly(p)

p <- qplot(x = factor(cyl), y = mpg, data = mtcars, geom = "boxplot")
ggplotly(p)

p <- qplot(x = factor(cyl), y = mpg, data = mtcars, geom = "violin")
ggplotly(p)

# 2d density stuff
p <- qplot(x = eruptions, y = waiting, data = faithful, geom = "density2d") %>%
  ggplotly(p)

p <- qplot(x = eruptions, y = waiting, data = faithful, geom = "bin2d") %>%
  ggplotly(p)

g <- qplot(x = wt, y = mpg, data = mtcars) + 
  geom_smooth() + facet_wrap(~ cyl)
ggplotly(g)

# pretty good support for theming!
ggplotly(g + theme_bw())
ggplotly(g + theme_tufte())
ggplotly(g + theme_economist())
ggplotly(g + theme_wsj())
ggplotly(g + theme_dark())

g2 <- g + theme(axis.text = element_text(size = 20))
ggplotly()


plot_ly(z = volcano, type = "surface")
str(volcano)
p <- plot_ly(z = volcano, type = "surface")
str(p)
# get info sent to plotly.js
str(plotly_build(p)) # get nested list of info sent

# maps!
data(canada.cities, package = "maps")
viz <- ggplot(canada.cities, aes(long, lat)) +
  borders(regions = "canada") +
  coord_equal() +
  geom_point(aes(text = name, size = pop), colour = "red", alpha = 1/2)
ggplotly(viz)

# open up some shiny apps that come with plotly
file.edit(system.file("examples/plotlyEvents/app.R", package = "plotly"))
file.edit(system.file("examples/plotlyLinkedClick/app.R", package = "plotly"))
file.edit(system.file("examples/plotlyLinkedBrush/app.R", package = "plotly"))
file.edit(system.file("examples/onRenderHover/index.Rmd", package = "plotly"))