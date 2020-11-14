#' Frobenius Distance
#'
#' Frobenius norm between two adjacency matrices.
#'
#' @param graph_1 igraph or matrix object.
#' @param graph_2 igraph or matrix object.
#'
#' @return A numeric distance metric.
#'
#' @export
dist_frobenius <- function(graph_1, graph_2) UseMethod("dist_frobenius")

#' @export
dist_frobenius.igraph <- function(graph_1, graph_2) {
  assertthat::assert_that(
    all(igraph::is.igraph(graph_1), igraph::is.igraph(graph_2)),
    msg = "Graphs must be igraph objects."
  )

  dist_frobenius.matrix(
    graph_1 %>% igraph::as_adjacency_matrix(sparse = FALSE),
    graph_2 %>% igraph::as_adjacency_matrix(sparse = FALSE)
  )
}

#' @export
dist_frobenius.matrix <- function(graph_1, graph_2) {
  assertthat::assert_that(
    all(is.matrix(graph_1), is.matrix(graph_2))
  )

  norm(graph_1 - graph_2, "F")
}
