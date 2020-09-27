#' Jaccard Distance
#'
#' Graph distance based on the Jaccard index between edge sets.
#'
#' @param graph_1 igraph or matrix object.
#' @param graph_2 igraph or matrix object.
#'
#' @export
dist_jaccard <- function(graph_1, graph_2) UseMethod("dist_jaccard")

#' @export
dist_jaccard.igraph <- function(graph_1, graph_2) {
  assertthat::assert_that(
    all(igraph::is.igraph(graph_1), igraph::is.igraph(graph_2)),
    msg = "Graphs must be igraph objects."
  )

  edges <- list(graph_1, graph_2) %>%
    lapply(
      function(graph) {
        graph <- igraph::as.undirected(graph)
        edge_list <- igraph::get.edgelist(graph)

        head_id <-  igraph::vertex_attr(graph, "id", edge_list[, 1])
        tail_id <-  igraph::vertex_attr(graph, "id", edge_list[, 2])

        if (is.null(head_id) || is.null(tail_id) || length(head_id) != length(tail_id)) {
          return(
            paste0(edge_list[, 1], edge_list[, 2])
          )
        }

        paste0(head_id, tail_id)
      }
    )

  cup <- union(edges[[1]], edges[[2]])
  cap <- intersect(edges[[1]], edges[[2]])

  1 - length(cap) / length(cup)
}

#' @export
dist_jaccard.matrix <- function(graph_1, graph_2) {
  dist_jaccard.igraph(
    igraph::graph.adjacency(graph_1, mode = "undirected"),
    igraph::graph.adjacency(graph_2, mode = "undirected")
  )
}
