% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allFunctions.R
\name{buildAncList}
\alias{buildAncList}
\title{Returns the ancestors of a particular variety (if they exist).}
\usage{
buildAncList(v1, geneal, gen = 0)
}
\arguments{
\item{v1}{the label of the vertex of interest (in character string format)}

\item{geneal}{the full genealogy  (in data frame format)}

\item{gen}{the generation (note: This should be left as default, as any other input will not affect results anyway)}
}
\description{
This function returns a nested list of the ancestors of the inputted variety.
}
\examples{
data(sbGeneal)
getParent("Essex", sbGeneal)
buildAncList("Essex", sbGeneal)
}
\references{
Rutter L, VanderPlas S, Cook D, Graham MA (2019).
ggenealogy: An R Package for Visualizing Genealogical Data.
\emph{Journal of Statistical Software},
\bold{89}(13), 1--31.
\doi{10.18637/jss.v089.i13}
}
\seealso{
\code{\link{getParent}} for information on determining parents
}
