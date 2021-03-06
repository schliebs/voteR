% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gles.R
\name{koa_positions}
\alias{koa_positions}
\title{Get coalition members}
\usage{
koa_positions(data_in = "gles2017_out", coalition = "schwarzgelb",
  issue = "soz")
}
\arguments{
\item{data_in}{Character string containing the name the dataset.}

\item{coalition}{Character string containing the name of the coalition.}

\item{issue}{Character string containing the issue.}
}
\value{
The treated dataset.
}
\description{
Calculate mean koalition issue position and create new variables
}
\examples{
koa_members(data_in = "gles2017_out",
            coalition = "schwarzgelb",
            issue = "soz")
}
