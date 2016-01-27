## Test environments
* local OS X install, R 3.1.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
There are no downstream dependencies

## devtools::build_vignette()
Below are three warnings from build_vignette(). All three warnings are the result of purposeful and necessary demonstration to users what will happen when they input incompatible data, or data that has NA values that will be automatically removed.

Warning 1: ggenealogy.Rnw, lines 317 & 323

Warning: There is no path between those two vertices
list()
Warning messages:
1: In .Call("R_igraph_get_shortest_paths", graph, as.igraph.vs(graph,  :
  At structural_properties.c:740 :Couldn't reach some vertices
2: In .Call("R_igraph_get_shortest_paths", graph, as.igraph.vs(graph,  :
  At structural_properties.c:740 :Couldn't reach some vertices
  
Warning 2: ggenealogy.Rnw, lines 378 & 383

Warning message:
In .Call("R_igraph_get_shortest_paths", graph, as.igraph.vs(graph,  :
  At structural_properties.c:740 :Couldn't reach some vertices

Warning 3: ggenealogy.Rnw, lines 394 & 425

Warning messages:
1: Removed 39 rows containing missing values (geom_segment). 
2: Removed 39 rows containing missing values (geom_text).
