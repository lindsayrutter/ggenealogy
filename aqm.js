// Author: Paul Murrell

var highlight = function(i) {
  var point = document.getElementById("point." + i);
  var label = document.getElementById("label." + i);
  point.setAttribute("r", point.getAttribute("r")*2);
  label.setAttribute("visibility", "visible");
};

var dim = function(i) {
  var point = document.getElementById("point." + i);
  var label = document.getElementById("label." + i);
  point.setAttribute("r", point.getAttribute("r")/2);
  label.setAttribute("visibility", "hidden");
};

