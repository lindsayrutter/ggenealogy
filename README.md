# ggenealogy

**Motivation:**

Genealogists wish to study the parent-child relationship between groups of organisms. Visual representations of genealogical relationships allow scientists to more effectively understand the historical changes that caused novel and desirable traits to arise in lineages. For example, in crops, desirable modifications could include an increase in protein yield or an increase in disease resistance. However, there are also times when lineages of detrimental traits can be viewed, such as to determine the origin of hazardous traits in rapidly-evolving viruses.

While there are visual methods available for genealogical data structures, there is a need for additional development of tools that are more customized to particular needs that arise when scientists want to make informed decisions while visualizing their data.

**Description:** 

This package provides methods for searching through genealogical data and displaying the results. The available plotting algorithms can assist users in performing data exploration, as well as in generating publication-quality images. This package uses the Grammar of Graphics.

Some of the currently-available methods include:

  * Drawing genealogy trees from database of known lineage.
  * Selecting generation number of ancestors and descendants to show around a given variety.
  * Showing shortest path between two given varieties, and superimpose over full lineage structure.
  * Obtaining graph theory measures of the full lineage structure.
  * Producing color matrix plots of variables between a subset of varieties.

**Installation:**

* The latest released version: `install.packages("ggenealogy")`
* The latest development version: `install_github("lrutter/ggenealogy")`

**Resources::**

Installation of the package will automatically download a vignette, which contains a more thorough explanation of the available methods, and example code.

**License:**

GPL