#' Laplacian spectral method
#' 
#' @export
dist_laplacian_spectral <- function(
  graph_1,
  graph_2,
  normed=TRUE,
  kernel="normal",           # Can be "normal", "lorentzian", or NULL
  hwhm=0.011775,             # half width at half maximum
  measure="jensen-shannon",  # Can be "jensen-shannon" or "euclidean"
  k=NULL,                    # Number of eigenvalues kept, NULL means all
  which="LM",
  results_list=FALSE         # Returns just distance if false, list of additional info if true
) UseMethod("dist_laplacian_spectral")

#' @export
dist_laplacian_spectral.igraph <- function(
  graph_1,
  graph_2,
  normed=TRUE,
  kernel="normal",
  hwhm=0.011775,
  measure="jensen-shannon",
  k=NULL,
  which="LM",
  results_list=FALSE
) {
  assertthat::assert_that(
    all(igraph::is.igraph(graph_1), igraph::is.igraph(graph_2)),
    msg = "Graphs must be igraph objects."
  )
  
}

#' @export
dist_laplacian_spectral.matrix <- function(
  graph_1,
  graph_2,
  normed=TRUE,
  kernel="normal",
  hwhm=0.011775,
  measure="jensen-shannon",
  k=NULL,
  which="LM",
  results_list=FALSE
) {
  
}