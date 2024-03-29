% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrangle.R
\name{dup.data.frame}
\alias{dup.data.frame}
\title{Show records with duplicate or duplicated values of grouping variables.}
\usage{
\method{dup}{data.frame}(x, ...)
}
\arguments{
\item{x}{data.frame}

\item{...}{optional grouping columns (named arguments are ignored)}
}
\value{
data.frame
}
\description{
Shows records with duplicate or duplicated values of grouping variables.
}
\examples{
library(dplyr)
dupGroups(mtcars)
dupGroups(group_by(mtcars, mpg))
dup(group_by(mtcars, mpg))
}
\seealso{
Other dup: 
\code{\link{dup}()}
}
\concept{dup}
