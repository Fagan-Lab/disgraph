#' Ipsen Mikhailov
#'
#' @param graph_1 igraph or matrix object.
#' @param graph_2 igraph or matrix object.
#' @param hwhm Numeric parameter for the lorentzian kernel.
#' @param results_list Logical indicating whether or not to return results list.
#'
#' @export
dist_ipsen_mikhailov <- function(graph_1, graph_2, hwhm = 0.08, results_list = FALSE) UseMethod("dist_ipsen_mikhailov")

#' @export
dist_ipsen_mikhailov.igraph <- function(graph_1, graph_2, hwhm = 0.08, results_list = FALSE) {
  assertthat::assert_that(
    all(igraph::is.igraph(graph_1), igraph::is.igraph(graph_2)),
    msg = "Graphs must be igraph objects."
  )

  dist_ipsen_mikhailov.matrix(
    igraph::as_adjacency_matrix(graph_1, sparse = FALSE),
    igraph::as_adjacency_matrix(graph_2, sparse = FALSE),
    hwhm,
    results_list
  )
}

#' @export
dist_ipsen_mikhailov.matrix <- function(graph_1, graph_2, hwhm = 0.08, results_list = FALSE) {
  assertthat::assert_that(
    all(is.matrix(graph_1), is.matrix(graph_2)),
    msg = "Graphs must be adjacency matrices."
  )

  # initialize optional results list
  results <- list()
  results[["adjacency_matrices"]] <- list(graph_1, graph_2)

  N <- dim(graph_1)[1]

  # Laplacian matrices for both graphs
  # the only laplacian function in igraph takes graphs, not matrices
  # this still seems easier than doing the work in the igraph method
  L1 <- igraph::laplacian_matrix(igraph::graph_from_adjacency_matrix(graph_1), normalized = FALSE, sparse = FALSE)
  L2 <- igraph::laplacian_matrix(igraph::graph_from_adjacency_matrix(graph_2), normalized = FALSE, sparse = FALSE)

  # modes for positive-semidefinite Laplacian
  w1 <- sqrt(abs(eigen(L1, symmetric = TRUE, only.values = TRUE)$values[2:N]))
  w2 <- sqrt(abs(eigen(L2, symmetric = TRUE, only.values = TRUE)$values[2:N]))

  # calculate the norm of each spectrum
  norm1 <- (N - 1) * pi / 2 - sum(atan(-w1 / hwhm))
  norm2 <- (N - 1) * pi / 2 - sum(atan(-w2 / hwhm))

  # define spectral densities
  density1 <- function(w) {
    sum(hwhm / ((w - w1)^2 + hwhm^2)) / norm1
  }
  density2 <- function(w) {
    sum(hwhm / ((w - w2)^2 + hwhm^2)) / norm2
  }

  func <- function(w) {
    (density1(w) - density2(w))^2
  }

  dist <- sqrt(stats::integrate(Vectorize(func), 0, Inf, subdivisions = 100)$value)

  if (results_list) {
    ret <- list(dist, c(graph_1, graph_2))
    names(ret) <- c("dist", "adjacency matrices")
    ret
  } else {
    dist
  }
}
