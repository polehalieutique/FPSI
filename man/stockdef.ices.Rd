% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stockdef.ices.R
\name{stockdef.ices}
\alias{stockdef.ices}
\title{Get limits values for  stocks provided by NOAA stocksmart packages}
\usage{
stockdef.ices(update = FALSE)
}
\arguments{
\item{Stock_Name}{to get data for a specified stock}

\item{Assessment_Year}{to get data for a specified stock}
}
\description{
Get limits values for  stocks provided by NOAA stocksmart packages
}
\examples{
stockdef.ices.dta<-stockdef.ices(update=TRUE)

}
