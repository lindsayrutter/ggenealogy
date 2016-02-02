set.seed(13)
xvar=runif(10,0,1)
yvar=runif(10,0,1)

start = c(1, 1, 1, 9)
end = c(2, 4, 6, 10)

plot(xvar, yvar)
segments(xvar[start], yvar[start], xvar[end],yvar[end], col= 'blue')

dat = data.frame(xvar = xvar, yvar = yvar)    
dat2 = cbind(dat[c(start, end), ], gr = 1:length(start))
ggplot(dat, aes(x = xvar, y = yvar)) + geom_point(shape = 20, size = 1) +
  geom_line(aes(x = xvar, y = yvar, group = gr), data = dat2)
