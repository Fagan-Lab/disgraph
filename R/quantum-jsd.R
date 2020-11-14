#'  quantum_jsd
#'  Compares the spectral entropies of the density matrices
#' @export
quantum_jsd <- function (graph_1, graph_2, beta=0.1, q=NULL) UseMethod("quantum_jsd")

#' @export
quantum_jsd.igraph <- function (graph_1, graph_2, beta=0.1, q=NULL) {
  assertthat::assert_that(
    all(igraph::is.igraph(graph_1), igraph::is.igraph(graph_2)),
    msg = "Graphs must be igraph objects."
  )
  
  quantum_jsd.matrix(
    igraph::as_adj(graph_1, sparse=FALSE),
    igraph::as_adj(graph_2, sparse=FALSE),
    beta,
    q
  )
}
#' Compares Polynomials relating to eigenvalues of adjacency matrices
#' @param  G1 (nx.Graph) networbetax graphs to be compared
#' @param G2 (nx.Graph) networbetax graphs to be compared
#' @param beta maximum degree of the polynomial
#' @param q weighting factor
#' @return The dist (float) Polynomial Dissimilarity between G1, G2 in a structure dist and G1,G2 are matrices
#' @export
quantum_jsd.matrix <- function (graph_1, graph_2, beta=5, q=1) {
  assertthat::assert_that(
    all(is.matrix(graph_1), is.matrix(graph_2)),
    msg = "Graphs must be adjacency matrices."
  )
  
  A1 <- graph_1
  A2 <- graph_2
  
  rho1 <- density_matrix(A1, beta)
  rho2 <- density_matrix(A2, beta)
  mix <- (rho1 + rho2) / 2
  
  H0 <- renyi_entropy(mix, q)
  H1 <- renyi_entropy(rho1, q)
  H2 <- renyi_entropy(rho2, q)
  
  currDist <- sqrt(H0 - 0.5 * (H1 + H2))
  structure(
    list(
      density_matrix_1 = rho1,
      density_matrix_2 = rho2,
      mixture_matrix = mix,
      entropy_1 = H1,
      entropy_2 = H2,
      entropy_mixture = H0,
      dist = currDist
    ),
    class = "quantum_jsd")
}

#' Create the density matrix encoding probabilities for entropies. 
#' @param  A adjacency matrix
#' @param beta beta value
#' @return Create the density matrix encoding probabilities for entropies.
#' @export
density_matrix <- function (A, beta) {
  L <- diag(rowSums(A)) - A
  rho = expm(-1 * beta * L)
  rho = rho / tr(rho)
  rho
}

#'  Calculate the Rényi entropy with order :math:`q`, or the Von Neumann entropy if :math:`q` is `None` or 1.
#' @param  X X Value
#' @param q q value
#' @return Create the Renyi entropy
#' @export
renyi_entropy <- function (X, q=NULL) {
  eigs <- eigen.Hermitian(X)
  zero_eigenvalues <- all.equal.envRefClass(abs(eigs), 0, atol=1e-6)
  eigs <- eigs[np$logical_not(zero_eigenvalues)]
  
  if(is.null(q) | q == 1){
    H <- -1 * sum(eigs * log2(eigs))
  } else{
    prefactor = 1 / (1 - q)
    H <- prefactor * log2(sum(matrix.power(eigs, q)))
  }
  H
}
