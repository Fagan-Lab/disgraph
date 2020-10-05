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
  which="LM",                # Prioritizes which eigenvalues are kept, see get_eigvs below for more
  results_list=FALSE         # Returns just distance if FALSE, list of additional info if TRUE
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
  
  dist_laplacian_spectral(
    igraph::as_adjacency_matrix(graph_1, sparse=FALSE),
    igraph::as_adjacency_matrix(graph_2, sparse=FALSE),
    normed,
    kernel,
    hwhm,
    measure,
    k,
    which,
    results_list
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
  assertthat::assert_that(
    all(is.matrix(graph_1), is.matrix(graph_2)),
    msg = "Graphs must be adjacency matrices."
  )
  
  results <- list()
  results[[ "adjacency_matrices" ]] <- list(graph_1, graph_2)
  
  directed <- !(isSymmetric(graph_1) && isSymmetric(graph_2))
  
  if (directed) {
    # create augmented adjacency matrices
    N1 <- length(graph_1)
    N2 <- length(graph_2)
    null_mat1 <- matrix(0, N1, N1)
    null_mat2 <- matrix(0, N2, N2)
    graph_1 <- rbind(cbind(null_mat1, t(graph_1)), cbind(graph_1, null_mat1))
    graph_2 <- rbind(cbind(null_mat2, t(graph_2)), cbind(graph_2, null_mat2))
    results[[ "augmented_adjacency_matrices" ]] <- list(graph_1, graph_2)
  }
  
  # Laplacian matrices for both graphs
  # the only laplacian function in igraph takes graphs, not matrices
  # this still seems easier than doing the work in the igraph method
  lap1 <- igraph::laplacian_matrix(igraph::graph_from_adjacency_matrix(graph_1), normalized=FALSE, sparse=FALSE)
  lap2 <- igraph::laplacian_matrix(igraph::graph_from_adjacency_matrix(graph_2), normalized=FALSE, sparse=FALSE)
  results[[ "laplacian_matrices" ]] <- list(lap1, lap2)
  
  if (is.null(k)) {
    ev1 <- abs(eigen(lap1, only.values=TRUE)$values)
    ev2 <- abs(eigen(lap2, only.values=TRUE)$values)
  } else {
    ev1 <- abs(get_eigvs(lap1, k=k, which=which))
    ev2 <- abs(get_eigvs(lap2, k=k, which=which))
  }
  results[[ "eigenvalues" ]] <- list(ev1, ev2)
  
  if (!is.null(kernel)) {
    a <- 0
    if (normed) {
      b <- 2
    } else {
      b <- Inf
    }
    
    # create continuous spectra
    density1 <- create_continuous_spectrum(ev1, kernel, hwhm, a, b)
    density2 <- create_continuous_spectrum(ev2, kernel, hwhm, a, b)
    
    dist <- spectra_comparison(density1, density2, a, b, measure)
    results[[ "dist" ]] <- dist
  } else {
    dist <- norm(ev1 - ev2)
    results[[ "dist" ]] <- dist
  }
  
  dist
}

# Partial implementation of scipy.sparse.linalg.eigsh
# Selects k eigenvalues of mat, prioritizing based on modes from eigsh:
#   "LM" : Largest absolute value (Magnitude)
#   "SM" : Smallest absolute value (Magnitude)
#   "LA" : Largest value (Algebraic)
#   "SA" : Smallest value (Algebraic)
#   "BE" : Half from each end of the spectrum 
# Modes are selected with the which parameter
# Note: The scipy documentation is rather ambiguous, I have made some assumptions
#   about how the modes work which may need correction
get_eigvs <- function(mat, k, which) {
  evs <- eigen(mat, only.values=TRUE)$values
  
  if (which == "LA") {
    # eigen returns values in decreasing order, so no sorting is needed
    head(evs, k)
  } else if (which == "LM") {
    evs[order(abs(evs), decreasing=TRUE)]
  } else if (which == "SM") {
    evs[order(abs(evs))]
  } else if (which == "SA") {
    tail(evs, k)
  } else if (which == "BA") {
    k1 <- ceiling(k / 2)
    k2 <- floor(k / 2)
    c(head(evs, k1), tail(evs, k2))
  }
}

create_continuous_spectrum <- function(eigenvalues, kernel, hwhm, a, b) {
  if (kernel == "normal") {
    std <- hwhm / 1.1775
    f <- function(x, xp) {
      exp(-((x - xp)^2) / (2 * std^2)) / sqrt(2 * pi * std^2)
    }
    F <- function(x, xp) {
      (1 + erf((x - xp) / (sqrt(2) * std))) / 2
    }
  } else if (kernel == "lorentzian") {
    f <- function(x, xp) {
      hwhm / (pi * (hwhm^2 + (x - xp)^2))
    }
    F <- function(x, xp) {
      atan((x - xp) / hwhm) / pi + 1 / 2
    }
  }
  
  Z <- sum(F(b, eigenvalues) - F(a, eigenvalues))
  density <- function(x) {
    sum(f(x, eigenvalues)) / Z
  }
  
  density
}

spectra_comparison <- function(density1, density2, a, b, measure) {
  if (measure == "jensen-shannon") {
    M <- function(x) {
      (density1(x) + density2(x)) / 2
    }
    jensen_shannon <- 
      ((kullback_leiber(density1, M, a, b) 
      + kullback_leiber(density2, M, a, b)) / 2)
    dist <- sqrt(jensen_shannon)
  } else if (measure == "euclidean") {
    integrand <- function(x) {
      (density1(x) - density2(x))^2
    }
    dist <- sqrt(integrate(Vectorize(integrand), a, b)$value)
  }
  
  dist
}

# I have modified this function slightly from the netrd version because
# as-is, for some inputs it returns a negative number which causes the
# measurement to fail (return NaN). I think this modification is valid
# but I'm not 100% sure.
kullback_leiber <- function(f1, f2, a, b) {
  integrand <- function(x) {
    if (f1(x) > 0 && f2(x) > 0) {
      result <- f1(x) * log(f1(x) / f2(x))
      if (result < 0) {
        result <- 0
      }
    } else {
      result = 0
    }
    result
  }
  
  integrate(Vectorize(integrand), a, b)$value
}

# From the examples section of the pnorm() documentation.
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/Normal
erf <- function(x) {
  2 * pnorm(x * sqrt(2)) - 1
}