#' Hamming Ipsen Mikhailov Distance
#'
#' @param graph_1 igraph or matrix object.
#' @param graph_2 igraph or matrix object.
#' @param combination_factor Numeric factor to be combined with IM metric.
#' @param results_list Logical indicating whether or not to return results list.
#'
#' @return A numeric distance metric or optional
#'
#' @export
dist_hamming_ipsen_mikhailov <- function (graph_1, graph_2, combination_factor=1, results_list=FALSE) UseMethod("dist_hamming_ipsen_mikhailov")

#' @export
dist_hamming_ipsen_mikhailov.igraph <- function (graph_1, graph_2, combination_factor=1, results_list=FALSE) {
  assertthat::assert_that(
    all(igraph::is.igraph(graph_1), igraph::is.igraph(graph_2)),
    msg = "Graphs must be igraph objects."
  )

  dist_hamming_ipsen_mikhailov.matrix(
    igraph::as_adj(graph_1, sparse=FALSE),
    igraph::as_adj(graph_2, sparse=FALSE),
    combination_factor,
    results_list
  )
}

#' @export
dist_hamming_ipsen_mikhailov.matrix <- function (graph_1, graph_2, combination_factor=1, results_list=FALSE) {
  assertthat::assert_that(
    all(is.matrix(graph_1), is.matrix(graph_2)),
    msg = "Graphs must be adjacency matrices."
  )

  # create optional results list
  results <- list()
  results[[ "adjacency_matrices" ]] <- c(graph_1, graph_2)

  # number of vertices
  N = dim(graph_1)[1]

  directed = !(isSymmetric(graph_1) && isSymmetric(graph_2))

  if (directed) {
    null_mat = matrix(0,N,N)

    # create augmented adjacency matrices
    g1_aug = rbind(cbind(null_mat, t(graph_1)), cbind(graph_1, null_mat))
    g2_aug = rbind(cbind(null_mat, t(graph_2)), cbind(graph_2, null_mat))
    results[[ "augmented_adjacency_matrices" ]] <- c(g1_aug, g2_aug)

    # get Hamming distance
    H = sum(abs(g1_aug - g2_aug)) / (2 * N * (N - 1))
    results[[ "hamming_dist" ]] <- H

    # calculate half-width at half-max
    hwhm <- get_hwhm_directed(N)
    results[[ "hwhm" ]] <- hwhm

    IM = dist_ipsen_mikhailov(g1_aug, g2_aug, hwhm)
    results[[ "IM" ]] <- IM
  } else {
    # get Hamming distance
    H = sum(abs(graph_1 - graph_2)) / (2 * N * (N - 1))
    results[[ "hamming_dist" ]] <- H

    # calculate half-width at half-max
    hwhm <- get_hwhm_undirected(N)
    results[[ "hwhm" ]] <- hwhm

    # get Ipsen-Mikhailov distance
    IM = dist_ipsen_mikhailov(graph_1, graph_2, hwhm)
    results[[ "IM" ]] <- IM
  }

  # combine H and IM distances
  HIM = sqrt(H^2 + combination_factor * IM^2) / sqrt(1 + combination_factor)
  results[[ "HIM" ]]

  # return the optional results list
  if (results_list) {
    results
  } else {
    HIM
  }
}

# calculates optimal half-width at half-max for undirected graphs
get_hwhm_undirected <- function (n_nodes) {
  func <- function(g) {
    n_sqrt <- sqrt(n_nodes)
    v <- atan(n_sqrt/g)

    (
      -1
      + 1 / (pi * g)
      + (pi / 2 + g * n_sqrt / (g^2 + n_nodes) + v) / (2 * g * (pi / 2 + v)^2)
      - 4
      * g
      * (pi - g * log(g^2 / (g^2 + n_nodes)) / n_sqrt + v)
      / ((pi / 2 + v) * pi * (4 * g^2 + n_nodes))
    )
  }

  stats::uniroot(func, c(0.01, 1))$root
}

# calculates optimal half-width at half-max for directed graphs
get_hwhm_directed <- function (N) {
  func <- function(g) {
    Nm2 <- N - 2
    sN <- sqrt(N)
    sNm2 <- sqrt(N - 2)
    s2Nm2 <- sqrt(2 * N - 2)
    atN <- atan(sN / g)
    atNm2 <- atan(sNm2 / g)
    at2Nm2 <- atan(s2Nm2 / g)
    K <- 1 / ((2 * N - 1) * pi / 2 + (N - 1) * (atNm2 + atN) + at2Nm2)
    Z <- 2 * g / pi
    W <- g * (N - 1) * K
    Wp <- W / (N - 1)
    M0 <- pi / (4 * g ** 3)
    MN <- (g ** 2 * atN + N * atN + g * sN) / (2 * (g ** 5 + N * g ** 3)) + pi / (
      4 * g ** 3
    )
    MNm2 <- (g ** 2 * atNm2 + Nm2 * atNm2 + g * sNm2) / (
      2 * (g ** 5 + Nm2 * g ** 3)
    ) + pi / (4 * g ** 3)
    M2Nm2 <- (g ** 2 * at2Nm2 + (2 * N - 2) * at2Nm2 + g * s2Nm2) / (
      2 * (g ** 5 + (2 * N - 2) * g ** 3)
    ) + pi / (4 * g ** 3)
    L <- function(T, U) {
      (-log(g ** 2 + U) + log(g ** 2 + T)) / (
        (4 * g ** 2 + T + 3 * U) * sqrt(T)
        - (4 * g ** 2 + 3 * T + U) * sqrt(U)
      ) + (pi + atan(sqrt(T) / g) + atan(sqrt(U) / g)) / (
        4 * g ** 3 + g * T - 2 * g * sqrt(U * T) + g * U
      )
    }

    (
      -1
      + Z ** 2 * M0
      + W ** 2 * (MNm2 + MN)
      + Wp ** 2 * M2Nm2
      - 2 * Z * W * L(0, Nm2)
      - 2 * Z * W * L(0, N)
      - 2 * Z * Wp * L(0, 2 * N - 2)
      + 2 * W ** 2 * L(Nm2, N)
      + 2 * W * Wp * L(Nm2, 2 * N - 2)
      + 2 * W * Wp * L(N, 2 * N - 2)
    )

  }

  stats::uniroot(func, c(0.01, 1))$root
}
