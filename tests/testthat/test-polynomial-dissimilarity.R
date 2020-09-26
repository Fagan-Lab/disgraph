context("Testing Network Distance")

MAX_ERROR <- 0.01

### Testing Polynomial Dissimilarity ------------------------------------------------
test_that("Test Zero Polynomial Dissimilarity", {
  graph <- matrix(
    rbind(
      	c(0, 1, 0, 0, 0, 1),
      	c(0, 0, 1, 0, 0, 1),
      	c(1, 1, 1, 0, 1, 1),
      	c(1, 1, 1, 1, 1, 1),
      	c(1, 1, 1, 0, 0, 0),
      	c(1, 0, 0, 1, 1 ,0)
    ),
    nrow = 6
  )

  output <- dist_polynomial_dissimilarity(graph, graph)
  expect_equal(output$dist, 0)
})

test_that("Test Polynomial Dissimilarity is Correctly Calculated", {

  graph <- matrix(
    rbind(
      	c(0, 1, 0, 0, 0, 1),
      	c(0, 0, 1, 0, 0, 1),
      	c(1, 1, 1, 0, 1, 1),
      	c(1, 1, 1, 1, 1, 1),
      	c(1, 1, 1, 0, 0, 0),
      	c(1, 0, 0, 1, 1 ,0)
    ),
    nrow = 6
  )

  graph2 <- matrix(
    rbind(
      	c(0, 0, 0, 1, 1, 0),
      	c(1, 0, 1, 0, 1, 1),
      	c(1, 0, 0, 1, 0, 0),
      	c(0, 1, 1, 1, 0, 0),
      	c(0, 0, 1, 1, 1, 0),
      	c(1, 0, 1, 0, 0, 0)    
    ),
    nrow = 6
  )
  
  dist_poly <- dist_polynomial_dissimilarity(graph, graph2)
  expect_true((dist_poly$dist - 0.1506) < MAX_ERROR)
})
