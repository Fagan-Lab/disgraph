context("Testing Network Distance")

### Testing Hamming Distance --------------------------------------------------
test_that("Test Hamming Zero Distance", {
  graph <- matrix(
    cbind(
      c(0.0, 1.0, 0.0, 0.0),
      c(0.0, 0.0, 1.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0),
      c(0.0, 0.0, 0.0, 0.0)
    ),
    nrow = 4
  )

  dist <- dist_hamming(graph, graph)

  expect_equal(dist, 0)
})

test_that("Test Hamming Wrong Degree Distance", {
  graph <- matrix(
    cbind(
      c(0.0, 1.0, 0.0, 0.0),
      c(0.0, 0.0, 1.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0),
      c(0.0, 0.0, 0.0, 0.0)
    ),
    nrow = 4
  )

  graph_wrong_degree <- matrix(
    cbind(
      c(0.0, 1.0),
      c(0.0, 0.0)
    ),
    nrow = 2
  )

  expect_error(dist_hamming(graph, graph_wrong_degree))
})

test_that("Test Hamming Distance Correctly Calculated", {
  dist_numeric <- dist_hamming(c(1, 2), c(2, 1))

  expect_equal(dist_numeric, 2)

  # dist_graph <- dist_hamming(karate_club, disgraph:::test_graph)
  #
  # expect_equal(dist_graph, 0)
})
