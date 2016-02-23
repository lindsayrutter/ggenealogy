library(ggrepel)
library(ggplot2)
set.seed(1)
data = data.frame(x=mtcars$cyl,y=runif(32, 1, 10),label=paste0("label",seq(1:32)))

origPlot <- ggplot(data) +
  geom_point(aes(x, y), color = 'red') +
  geom_text(aes(x, y, label = label)) +
  theme_classic(base_size = 16)

repelPlot <- ggplot(data) +
  geom_point(aes(x, y), color = 'red') +
  geom_text_repel(aes(x, y, label = label)) +
  theme_classic(base_size = 16)

repelPlot2 <- ggplot(data) +
  geom_text_repel(aes(x, y, label = label), segment.size = 0) +
  theme_classic(base_size = 16)

repelPlot <- ggplot(data) +
  geom_point(aes(x, y), color = 'red') + # remove later
  geom_text_repel(aes(x, y, label = label), box.padding = unit(0.5, "lines")) +
  theme_classic(base_size = 16)

# loses points, if do unit(0, "lines) or unit(0.5, "native")
repelPlot <- ggplot(data) +
  geom_point(aes(x, y), color = 'red') + # remove later
  geom_text_repel(aes(x, y, label = label), box.padding = unit(0.5, "native")) +
  theme_classic(base_size = 16)
