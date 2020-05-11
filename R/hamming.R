#' Hamming Distance
#'
#' Index disagreement between adjancency matrices.
#' Must have the same number of nodes
#'
#' @export
dist_hamming <- function(graph_1, graph_2) UseMethod("dist_hamming")

#' @export
dist_hamming.igraph <- function(graph_1, graph_2) {
  assertthat::assert_that(
    all(igraph::is.igraph(graph_1), igraph::is.igraph(graph_2)),
    msg = "Graphs must be igraph objects."
  )

  assertthat::assert_that(
    igraph::vcount(graph_1) == igraph::vcount(graph_2),
    msg = "Graphs must have the same number of nodes."
  )

  dist_hamming.matrix(
    igraph::as_adjacency_matrix(graph_1),
    igraph::as_adjacency_matrix(graph_2)
  )
}

#' @export
dist_hamming.matrix <- function(G1, G2) {
  assertthat::assert_that(
    length(G1) == length(G2),
    msg = "Graphs must have the same number of nodes."
  )

  e1071::hamming.distance(G1, G2)
}

#' @export
dist_hamming.numeric <- function(graph_1, graph_2) {
  sum(graph_1 != graph_2)
}
