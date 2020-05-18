context("Testing Network Distance")

### Testing Jaccard Distance --------------------------------------------------
test_that("Test Jaccard Zero Distance", {
  graph <- matrix(
    cbind(
      c(0.0, 1.0, 0.0, 0.0),
      c(0.0, 0.0, 1.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0),
      c(0.0, 0.0, 0.0, 0.0)
    ),
    nrow = 4
  )

  dist <- dist_jaccard(graph, graph)

  expect_equal(dist, 0)
})

test_that("Test Jaccard Distance Correctly Calculated", {
  dist_graph <- dist_jaccard(karate_club, disgraph:::test_graph)

  expect_equal(dist_graph, 1)
})
