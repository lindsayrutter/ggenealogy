
# ggenealogy

[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/ggenealogy)](https://cranlogs.r-pkg.org/badges/grand-total/ggenealogy)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-ff69b4.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.3.0-6666ff.svg)](https://cran.r-project.org/)

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
  * Interactively visualizing genealogical structure
  * Perform basic parsing and calculations on descendant branches of interest

**Installation:**

* The latest released version: `install.packages("ggenealogy")`
* The latest development version: `install_github("lindsayrutter/ggenealogy")`

**Vignette:**

Installation of the package will automatically download a vignette, which contains a more thorough explanation of the available methods, and example code.

**Paper citation:**

A software paper describing the `ggenealogy` package is available in the Journal of Statistical Software [here](https://www.jstatsoft.org/article/view/v089i13).

```
@article{rutter2019ggenealogy,
  title={ggenealogy: An R Package for Visualizing Genealogical Data},
  author={Rutter, Lindsay and VanderPlas, Susan and Cook, Dianne and Graham, Michelle A},
  journal={Journal of Statistical Software},
  volume={89},
  number={1},
  pages={1--31},
  year={2019}
}
```

**License:**

GPL
