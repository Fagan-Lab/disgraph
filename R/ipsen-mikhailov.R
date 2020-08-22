#' Ipsen Mikhailov
#' 
#' @export
dist_ipsen_mikhailov <- function(graph_1, graph_2, hwhm=0.08) UseMethod("dist_ipsen_mikhailov")

#' @export
dist_ipsen_mikhailov.igraph <- function(graph_1, graph_2, hwhm=0.08) {
  assertthat::assert_that(
    all(igraph::is.igraph(graph_1), igraph::is.igraph(graph_2)),
    msg = "Graphs must be igraph objects."
  )
  
  dist_ipsen_mikhailov.matrix(
    igraph::as_adjacency_matrix(graph_1),
    igraph::as_adjacency_matrix(graph_2),
    hwhm
  )
}

dist_ipsen_mikhailov.matrix <- function(graph_1, graph_2, hwhm=0.08) {
  assertthat::assert_that(
    all(is.matrix(graph_1), is.matrix(graph_2)),
    msg = "Graphs must be matrices."
  )
  
  nettools::netdist(graph_1, graph_2, d="IM", ga=hwhm)
}