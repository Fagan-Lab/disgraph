context("Testing Network Distance")

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
