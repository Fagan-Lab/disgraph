% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hamming-ipsen-mikhailov.R
\name{dist_hamming_ipsen_mikhailov}
\alias{dist_hamming_ipsen_mikhailov}
\title{Hamming Ipsen Mikhailov Distance}
\usage{
dist_hamming_ipsen_mikhailov(
  graph_1,
  graph_2,
  combination_factor = 1,
  results_list = FALSE
)
}
\arguments{
\item{graph_1}{igraph or matrix object.}

\item{graph_2}{igraph or matrix object.}

\item{combination_factor}{Numeric factor to be combined with IM metric.}

\item{results_list}{Logical indicating whether or not to return results list.}
}
\value{
A numeric distance metric or optional
}
\description{
Hamming Ipsen Mikhailov Distance
}
