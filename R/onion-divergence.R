#' onion-divergence
#'
#' @export
dist_onion_divergence <- function (graph_1, graph_2, dist='lccm') UseMethod("dist_onion_divergence")

#' @export
dist_onion_divergence.igraph <- function (graph_1, graph_2, dist='lccm') {
  assertthat::assert_that(
    all(igraph::is.igraph(graph_1), igraph::is.igraph(graph_2)),
    msg = "Graphs must be igraph objects."
  )

  dist_onion_divergence.matrix(
    igraph::as_adj(graph_1, sparse=FALSE),
    igraph::as_adj(graph_2, sparse=FALSE),
    dist
  )
}

#' @export
dist_onion_divergence.matrix <- function (graph_1, graph_2, dist='lccm') {
  assertthat::assert_that(
    all(is.matrix(graph_1), is.matrix(graph_2)),
    msg = "Graphs must be adjacency matrices."
  )

   G1_simple <- simplify(graph_1, remove.loops = TRUE)
   G2_simple <- simplify(graph_2, remove.loops = TRUE)
   matrices_G1 <- create_sparse_matrices_for_graph(G1_simple)
   matrices_G2 <- create_sparse_matrices_for_graph(G2_simple)

   cm_dist <- divergence_of_sparse_matrices(matrices_G1['cm'], matrices_G2['cm'])
   ccm_dist <- divergence_of_sparse_matrices(matrices_G1['ccm'], matrices_G2['ccm'])
   lccm_node_dist <- divergence_of_sparse_matrices(matrices_G1['lccm_node'], matrices_G2['lccm_node'])
   lccm_dist <- divergence_of_sparse_matrices(matrices_G1['lccm'], matrices_G2['lccm'])

   results <- list()
   results[["cm_dist"]] <- cm_dist
   results[["ccm_dist"]] <- ccm_dist
   results[["lccm_node_dist"]] <- lccm_node_dist
   results[["lccm_dist"]] <- lccm_dist
   dist_id <- paste(dist, "_dist", sep = "")
   results[["dist"]] <- results[dist_id]
   results[dist_id]
}


onion_decomposition <- function (G){
    # Creates a copy of the graph (to be able to remove vertices and edges)
    G_copy <- G
    # listionaries to register the k-core/onion decompositions.
    coreness_map = list()
    layer_map = list()
    local_layer_map = list()
    # Performs the onion decomposition.
    current_core = 0
    current_layer = 1
    local_layer = 1
    while (gorder(G_copy) > 0) {
        # Sets properly the current core.
        degree_sequence = degree(G_copy)
        min_degree = min(degree_sequence)
        if (min_degree >= (current_core + 1)){
            current_core <- min_degree
            local_layer <- 1
        }
        # Identifies vertices in the current layer.
        this_layer_ = vector()
        for (v in V(G_copy)){
          if (degree(G_copy, v) <= current_core){
            this_layer_.append(v)
          }
        }
          
        # Identifies the core/layer of the vertices in the current layer.
        for (v in this_layer_){
          coreness_map[v] = current_core
          layer_map[v] = current_layer
          local_layer_map[v] = local_layer
          delete_vertices(G_copy, v)
        }
            
        # Updates the layer count.
        current_layer <- current_layer + 1
        local_layer <- local_layer + 1
    }
    # Returns the listionaries containing the k-shell and onion layer of
    # each vertices.
    c(layer_map, local_layer_map, coreness_map)
}

update_sparse_matrix <- function (listionary, values, key, index){
    # Creates entry if it doesn't exist or else update the corresponding value

    if(has.key(key, listionary) == FALSE){
      index_i <- index
      listionary[[key]] <- index
      values <- append(values, 1)
      index <- index + 1
    } else{
       index_i <- listionary[[key]]
       values[[index_i]] <- values[[index_i]] + 1
    }

    # Returns new index (i.e. the nb of nonzero entry in sparse matrix)
    index
}


create_sparse_matrices_for_graph <- function(G){
    
    onionReturned <- onion_decomposition(G)
    onion <- onionReturned[1]
    local_layer <- onionReturned[2]
    kcore <- onionReturned[3]
    # creates of copy of the graph
    G_copy = G
    cm_sparse_stub_matrix = list()
    ccm_sparse_stub_matrix = list()
    lccm_sparse_stub_matrix = list()
    lccm_sparse_node_matrix = list()
    cm_values = vector()
    cm_index = 0
    ccm_values = vector()
    ccm_index = 0
    lccm_node_values = vector()
    lccm_node_index = 0
    lccm_values = vector()
    lccm_index = 0
    
    for (n in V(G_copy)){
      d1 = G_copy.degree(n)
      c1 = kcore[n]
      l1 = local_layer[n]
      cm_index <- update_sparse_matrix(cm_sparse_stub_matrix, cm_values, d1, cm_index)
      lccm_node_index <- update_sparse_matrix(
        lccm_sparse_node_matrix, lccm_node_values, c(d1, c1, l1), lccm_node_index
      )
    }
    
    for (e in E(G_copy)){
      d1 = degree(G_copy, e[0])
      d2 = degree(G_copy, e[1])
      c1 = kcore[e[0]]
      c2 = kcore[e[1]]
      l1 = local_layer[e[0]]
      l2 = local_layer[e[1]]
      ccm_index <- update_sparse_matrix(
        ccm_sparse_stub_matrix, ccm_values, c(d1, d2), ccm_index
      )
      lccm_index <- update_sparse_matrix(
        lccm_sparse_stub_matrix, lccm_values, c(d1, c1, l1, d2, c2, l2), lccm_index
      )
      ccm_index <- update_sparse_matrix(
        ccm_sparse_stub_matrix, ccm_values, c(d2, d1), ccm_index
      )
      lccm_index <- update_sparse_matrix(
        lccm_sparse_stub_matrix, lccm_values, c(d2, c2, l2, d1, c1, l1), lccm_index
      )
    }
    
    cm_values <- sapply(cm_values, function(x) {x / gorder(G_copy)})
    ccm_values <- sapply(ccm_values, function(x){x / (2 * gsize(G_copy))})
    lccm_values <- sapply(lccm_values, function(x){x / (2 * gsize(G_copy))}) 
    lccm_node_values  <- sapply(lccm_node_values, function(x){x / (gorder(G_copy))})

    # output the results in a listionary
    results = list()
    results[['cm']] = c(cm_sparse_stub_matrix, cm_values)
    results[['ccm']] = c(ccm_sparse_stub_matrix, ccm_values)
    results[['lccm']] = c(lccm_sparse_stub_matrix, lccm_values)
    results[['lccm_node']] = c(lccm_sparse_node_matrix, lccm_node_values)
    results
}

divergence_of_sparse_matrices <- function (keys1, values1, keys2, values2){
  
    keys <- names(list(keys1, keys2))
    JSD = 0
    
    for (key in keys){
      if (key %in% keys1){
        value1 <- values1[[keys1[key]]]
      } 
      else {
        value1 <- 0.0
      }
      
      if (key %in% keys2){
        value2 <- values2[[keys2[key]]]
      } else{
        value2 <- 0.0
      }
      avg <- (value1 + value2) / 2
      
      if (value1 > 0){
        JSD <- JSD + 0.5 * value1 * log2(value1 / avg)
      }
      
      if (value2 > 0){
        JSD <- JSD + 0.5 * value2 * log2(value2 / avg)
      }
    }
      
    JSD
}
