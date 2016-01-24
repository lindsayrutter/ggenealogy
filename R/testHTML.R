library(gridSVG)
library(ggplot2)
library(XML)
library(rjson)

set.seed(955)
dat <- data.frame(cond = rep(c("A", "B"), each=10), xvar = 1:20 + rnorm(20,sd=3), yvar = 1:20 + rnorm(20,sd=3))

g4 = ggplot(dat, aes(x=xvar, y=yvar)) + geom_smooth() + geom_point(shape=19, aes(color = cond), size=5)
g4

# What does this line do?  It writes the SVG to the file "plot1.svg"?
g4.svg <- grid.export("plot1.svg",addClasses=TRUE)

# create a valid html file
cat("<html><head></head><body>", file="./myAwesomePlot.html")

# I'm assuming this gets the svg content and can write it to a file
cat(g4.svg$svg, file="./myAwesomePlot.html")

# Javascript
cat(
  '<script> ourdata=',
  rjson::toJSON(apply(g4$data,MARGIN=1,FUN=function(x)return(list(x)))),
  '</script>'
)

cat(
  '<script> dataToBind = ',
  'd3.entries(ourdata.map(function(d,i) {return d[0]}))',
  '</script>'
)

cat(
  '<script>\n',
  'scatterPoints = d3.select(".points").selectAll("use");\n',
  'scatterPoints.data(dataToBind)',
  '</script>\n'
)

cat('<script>\n',
    'scatterPoints  
    .on("mouseover", function(d) {      
    //Create the tooltip label
    var tooltip = d3.select(this.parentNode).append("g");
    tooltip
    .attr("id","tooltip")
    .attr("transform","translate("+(d3.select(this).attr("x")+10)+","+d3.select(this).attr("y")+")")
    .append("rect")
    .attr("stroke","white")
    .attr("stroke-opacity",.5)
    .attr("fill","white")
    .attr("fill-opacity",.5)
    .attr("height",30)
    .attr("width",50)
    .attr("rx",5)
    .attr("x",2)
    .attr("y",5);
    tooltip.append("text")
    .attr("transform","scale(1,-1)")
    .attr("x",5)
    .attr("y",-22)
    .attr("text-anchor","start")
    .attr("stroke","gray")
    .attr("fill","gray")
    .attr("fill-opacity",1)
    .attr("opacity",1)
    .text("x:" + Math.round(d.value.xvar*100)/100);
    tooltip.append("text")
    .attr("transform","scale(1,-1)")
    .attr("x",5)
    .attr("y",-10)
    .attr("text-anchor","start")
    .attr("stroke","gray")
    .attr("fill","gray")      
    .attr("fill-opacity",1)
    .attr("opacity",1)
    .text("y:" + Math.round(d.value.yvar*100)/100);
    })              
    .on("mouseout", function(d) {       
    d3.select("#tooltip").remove();  
    });',
'</script>'
)

# close out file
cat("</body></html>", file="myAwesomePlot.html")

