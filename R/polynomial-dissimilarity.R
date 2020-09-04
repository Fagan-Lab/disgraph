#'  polynomial_dissimilarity
#' 
#' @export
dist_polynomial_dissimilarity <- function (graph_1, graph_2, k=5, alpha=1, results_list=FALSE) UseMethod("dist_polynomial_dissimilarity")

#' @export
dist_polynomial_dissimilarity.igraph <- function (graph_1, graph_2, k=5, alpha=1, results_list=FALSE) {
  assertthat::assert_that(
    all(igraph::is.igraph(graph_1), igraph::is.igraph(graph_2)),
    msg = "Graphs must be igraph objects."
  )
  
  dist_polynomial_dissimilarity.matrix(
    igraph::as_adj(graph_1, sparse=FALSE),
    igraph::as_adj(graph_2, sparse=FALSE),
    k,
    alpha,
    results_list
  )
}

#' @export
dist_polynomial_dissimilarity.matrix <- function (graph_1, graph_2, k=5, alpha=1, results_list=FALSE) {
  assertthat::assert_that(
    all(is.matrix(graph_1), is.matrix(graph_2)),
    msg = "Graphs must be adjacency matrices."
  )
  
  A1 <- graph_1
  A2 <- graph_2
  
  P_A1 <- similarity_score(A1, k, alpha)
  P_A2 <- similarity_score(A2, k, alpha)
  
  dist <- norm(P_A1 - P_A2, type = c("F"))/ nrow(A1)**2
  
  # create optional results list
  results <- list()
  results[[ "adjacency_matrices" ]] <- c(graph_1, graph_2)
  results[[ "dist" ]] <- dist

  if(results_list) {
    results
  } else{
    dist
  }
}

# Calculate the similarity score used in the polynomial dissimilarity distance. 
similarity_score <- function (A, k, alpha) {
  
  e <- eigen(A)
  eig_vals <- e$values
  Q <- e$vectors 
  n <- nrow(A)
  polynomial <- function(kp) {
    eig_vals ** kp / (n - 1) ** (alpha * (kp - 1))
  }
  
  W = Diagonal(sum(sapply(1:k, polynomial)))
  P_A = dot(dot(Q, W), t(Q))
  P_A
}
