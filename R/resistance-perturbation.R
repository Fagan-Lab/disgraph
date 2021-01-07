#'  resistance_perturbation
#'  Compares the resistance matrices
#' @export
resistance_perturbation <- function (graph_1, graph_2, p=2) UseMethod("resistance_perturbation")

#' @export
resistance_perturbation.igraph <- function (graph_1, graph_2, p=2) {
  assertthat::assert_that(
    all(igraph::is.igraph(graph_1), igraph::is.igraph(graph_2)),
    msg = "Graphs must be igraph objects."
  )
  
  resistance_perturbation.matrix(
    igraph::as_adj(graph_1, sparse=FALSE),
    igraph::as_adj(graph_2, sparse=FALSE),
    p
  )
}
#' gets resistance of matrices
#' @param  G1 (nx.Graph) networkx graphs to be compared
#' @param G2 (nx.Graph) networkx graphs to be compared
#' @param p norm difference between resistance
#' @return The dist (float) between G1, G2 in a structure dist and G1,G2 are matrices
#' @export
resistance_perturbation.matrix <- function (graph_1, graph_2, p=2) {
  assertthat::assert_that(
    all(is.connected(graph_1), is.connected(graph_2)),
    msg = "Graphs must be connected"
  )
  
  R1 <- get_resistance_matrix(R1, k, alpha)
  R2 <- get_resistance_matrix(R2, k, alpha)
  
  if(!is.infinite(p)){
    currDist <- (sum(abs(R1 - R2)**p)**(1 / p))
  } else{
    currDist <- max(abs(R1 - R2))
  }
  
  structure(
    list(
      adjacency_matrices = c(graph_1, graph_2),
      dist = currDist
    ),
    class = "resistance_matrices")
}

#' Calculate the resistance matrix of a graph
#' @param  G
#' @return resistance matrix of a graph
#' @export
get_resistance_matrix <- function (G) {
  n <- gorder(G)
  A <- G
  D <- diag(rowSums(A))
  L <- D - A
  I <- eye(n)
  J <- (1 / n) * ones(n, n)
  L_i <- solve(L + J, I) - J
  ones <- ones(n)
  ones <- resize(ones, 1, n)
  L_i_diag <- diag(L_i)
  L_i_diag <- resize(L_i_diag, n, 1)
  (L_i_diag %*% ones) + (ones.T %*% L_i_diag.T) - 2 * L_i
}