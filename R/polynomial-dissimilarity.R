#'  polynomial_dissimilarity
#'  Compares polynomials relating to the eigenvalues of the adjacency matrices 
#' @export
dist_polynomial_dissimilarity <- function (graph_1, graph_2, k=5, alpha=1) UseMethod("dist_polynomial_dissimilarity")

#' @export
dist_polynomial_dissimilarity.igraph <- function (graph_1, graph_2, k=5, alpha=1) {
  assertthat::assert_that(
    all(igraph::is.igraph(graph_1), igraph::is.igraph(graph_2)),
    msg = "Graphs must be igraph objects."
  )
  
  dist_polynomial_dissimilarity.matrix(
    igraph::as_adj(graph_1, sparse=FALSE),
    igraph::as_adj(graph_2, sparse=FALSE),
    k,
    alpha
  )
}
#' Compares Polynomials relating to eigenvalues of adjacency matrices
#' @param  G1 (nx.Graph) networkx graphs to be compared
#' @param G2 (nx.Graph) networkx graphs to be compared
#' @param K maximum degree of the polynomial
#' @param alpha weighting factor
#' @return The dist (float) Polynomial Dissimilarity between G1, G2 in a structure dist and G1,G2 are matrices
#' @export
dist_polynomial_dissimilarity.matrix <- function (graph_1, graph_2, k=5, alpha=1) {
  assertthat::assert_that(
    all(is.matrix(graph_1), is.matrix(graph_2)),
    msg = "Graphs must be adjacency matrices."
  )
  
  A1 <- graph_1
  A2 <- graph_2
  
  P_A1 <- similarity_score(A1, k, alpha)
  P_A2 <- similarity_score(A2, k, alpha)
  difference <- P_A1 - P_A2
  
  currDist <- norm(difference, type = c("F"))/ nrow(A1)**2

  structure(
   list(
      adjacency_matrices = c(graph_1, graph_2),
      dist = currDist
   ),
   class = "polynomial_dissimilarity")
 }

#' Calculate the similarity score used in the polynomial dissimilarity distance. 
#' @param  A adjacency matrix
#' @param K maximum degree of the polynomial
#' @param alpha weighting factor
#' @return similarity score
#' @export
similarity_score <- function (A, k, alpha) {
  e <- eigen(A)
  eig_vals <- e$values
  Q <- e$vectors 
  n <- nrow(A)
  
  W = diag(sum(sapply(1:k,  function(kp) {
    eig_vals ** kp / (n - 1) ** (alpha * (kp - 1))
  })))
  pracma::dot(pracma::dot(Q, W), t(Q))
}
