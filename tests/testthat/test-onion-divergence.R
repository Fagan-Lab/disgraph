context("Testing Network Distance")

MAX_ERROR <- 0.01

### Testing Polynomial Dissimilarity ------------------------------------------------
test_that("Test Zero Onion Divergence", {
  graph1 <- matrix(
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
  
  G1 <- igraph::graph_from_adjacency_matrix(graph1)
  
  #output <- dist_onion_divergence(graph1, graph1)
  output <- dist_onion_divergence(G1, G1)
  #print(output)
  expect_equal(output$dist, 0)
})

test_that("Test Onion Divergence is Correctly Calculated", {
  
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
})