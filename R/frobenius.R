#' Frobenius norm
#'
#' @export
frobenius <- function(graph_1, graph_2) UseMethod("frobenius")

#' @export
frobenius.igraph <- function(graph_1, graph_2) {
  assertthat::assert_that(
    all(igraph::is.igraph(graph_1), igraph::is.igraph(graph_2))
  )

  frobenius.igraph(
    graph_1 %>% igraph::as_adjacency_matrix(),
    graph_2 %>% igraph::as_adjacency_matrix(),
  )
}

#' @export
frobenius.matrix <- function(graph_1, graph_2) {
  assertthat::assert_that(
    all(is.matrix(graph_1), is.matrix(graph_2))
  )

  norm(graph_1 - graph_2, "F")
}
