context("Testing Network Distance")

MAX_ERROR <- 0.01

### Testing Frobenius Distance ------------------------------------------------
test_that("Test Frobenius Zero Distance", {
  graph <- matrix(
    cbind(
      c(0.0, 1.0, 0.0, 0.0),
      c(0.0, 0.0, 1.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0),
      c(0.0, 0.0, 0.0, 0.0)
    ),
    nrow = 4
  )

  dist <- dist_frobenius(graph, graph)

  expect_equal(dist, 0)
})

test_that("Test Frobenius Distance is Correctly Calculated", {
  dist_graph <- dist_frobenius(karate_club, disgraph:::test_graph)

  expect_true(dist_graph - 18.76 < MAX_ERROR)
})
