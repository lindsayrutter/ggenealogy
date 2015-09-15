library(ggplot2)
data(tips, package = "reshape2")
tips$sex_smoker <- with(tips, interaction(sex, smoker))
p <- ggplot() + geom_point(data = tips, aes(x = total_bill, y = tip, colour = sex_smoker))
p

library(animint)
library(servr) 

p1 <- ggplot() + theme(legend.position = "none") + geom_point(data = tips, aes(x = sex, y = smoker, clickSelects = sex_smoker, colour = sex_smoker), position = "jitter")
p2 <- ggplot() + geom_point(data = tips, aes(x = total_bill, y = tip, showSelected = sex_smoker, colour = sex_smoker))

plots <- list(plot1 = p1, plot2 = p2)
structure(plots, class = "animint")

animint2dir(list(plot = p), out.dir = "testAnDir", open.browser = FALSE)
servr::httd("testAnDir")
