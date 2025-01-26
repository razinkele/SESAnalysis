# Helper function to create edge matrix for a cycle
create_cycle_edge_matrix <- function(cycle) {
  cycle_edges <- matrix(nrow = length(cycle), ncol = 2)
  for(i in 1:length(cycle)) {
    cycle_edges[i, 1] <- cycle[i]
    cycle_edges[i, 2] <- if(i < length(cycle)) cycle[i + 1] else cycle[1]
  }
  return(cycle_edges)
}

# Helper function for edge colors
# Helper function for edge colors
get_edge_colors <- function(edges, cycle_edges) {
  # Handle empty edges case
  if (nrow(edges) == 0) {
    return(edges)
  }
  
  edges$value <- 1  # For controlling opacity
  edges$color <- sapply(1:nrow(edges), function(i) {
    edge_in_cycle <- any(apply(cycle_edges, 1, function(ce) {
      ce[1] == edges$from[i] && ce[2] == edges$to[i]
    }))
    
    # Base colors for positive and negative edges
    base_color <- if(edges$type[i] == "positive") {
      c(43, 124, 233)  # Blue for positive
    } else {
      c(233, 43, 43)   # Red for negative
    }
    
    # Return full or dim opacity based on cycle membership
    if(edge_in_cycle) {
      sprintf("rgba(%d, %d, %d, 1)", base_color[1], base_color[2], base_color[3])
    } else {
      sprintf("rgba(%d, %d, %d, 0.15)", base_color[1], base_color[2], base_color[3])
    }
  })
  
  # Add dashed style for negative edges
  if (nrow(edges) > 0) {
    edges$dashes <- edges$type == "negative"
  }
  
  return(edges)
}

# Helper function to determine node colors based on cycle membership
get_node_colors <- function(nodes, cycle) {
  nodes$value <- 1  # For controlling opacity
  nodes$color <- sapply(1:nrow(nodes), function(i) {
    if(nodes$id[i] %in% cycle) {
      "rgba(151, 194, 252, 1)"  # Full opacity light blue
    } else {
      "rgba(151, 194, 252, 0.15)"  # 15% opacity light blue
    }
  })
  return(nodes)
}

# Helper function for default colors
set_default_colors <- function(nodes, edges) {
  # Handle empty edges case
  if (nrow(edges) == 0) {
    return(list(nodes = nodes, edges = edges))
  }
  
  edges$color <- sapply(1:nrow(edges), function(i) {
    if(edges$type[i] == "positive") {
      "rgba(43, 124, 233, 1)"  # Blue for positive
    } else {
      "rgba(233, 43, 43, 1)"   # Red for negative
    }
  })
  
  # Only set dashes if there are edges
  if (nrow(edges) > 0) {
    edges$dashes <- edges$type == "negative"
  }
  
  nodes$color <- "rgba(151, 194, 252, 1)"  # Full opacity light blue
  return(list(nodes = nodes, edges = edges))
}

# Helper function to find simple cycles in the graph
find_cycles <- function(g) {
  # Function to find all cycles starting from a vertex
  find_cycles_from_vertex <- function(graph, start_v) {
    cycles <- list()
    
    dfs_search <- function(current, visited, path) {
      # Get neighbors of current vertex
      neighbors <- neighbors(graph, current, mode = "out")
      
      for (n in neighbors) {
        # Ensure we're working with numeric values
        n <- as.numeric(n)
        start_v <- as.numeric(start_v)
        
        # Check if we've found a cycle
        if (!is.na(n) && !is.na(start_v) && n == start_v && length(path) > 2) {
          # Found a cycle
          cycles[[length(cycles) + 1]] <<- c(path, current)
        } else if (!is.na(n) && !any(visited == n)) {
          # Continue DFS
          dfs_search(n, c(visited, n), c(path, current))
        }
      }
    }
    
    # Start DFS with numeric values
    dfs_search(as.numeric(start_v), as.numeric(start_v), numeric(0))
    return(cycles)
  }
  
  # Find cycles starting from each vertex
  all_cycles <- list()
  vertices <- as.numeric(V(g))
  
  for (v in vertices) {
    if (!is.na(v)) {
      cycles <- find_cycles_from_vertex(g, v)
      all_cycles <- c(all_cycles, cycles)
    }
  }
  
  # Remove duplicate cycles (cycles that are rotations of each other)
  if (length(all_cycles) == 0) return(list())
  
  unique_cycles <- list()
  for (cycle in all_cycles) {
    # Skip if cycle is NULL or empty
    if (is.null(cycle) || length(cycle) == 0) next
    
    # Create all possible rotations of the cycle
    n <- length(cycle)
    rotations <- lapply(1:n, function(i) {
      c(cycle[i:n], cycle[1:(i-1)])
    })
    
    # Check if any rotation is already in unique_cycles
    is_new <- TRUE
    for (existing in unique_cycles) {
      if (length(existing) > 0 && 
          any(sapply(rotations, function(r) 
            identical(as.numeric(r), as.numeric(existing))))) {
        is_new <- FALSE
        break
      }
    }
    
    if (is_new) unique_cycles[[length(unique_cycles) + 1]] <- cycle
  }
  
  return(unique_cycles)
}

# Helper function to analyze loops
analyze_loop <- function(cycle, edges) {
  # Get edges in the cycle
  cycle_edges <- sapply(1:(length(cycle)), function(i) {
    from_node <- cycle[i]
    to_node <- if(i < length(cycle)) cycle[i + 1] else cycle[1]
    
    # Find the edge type
    edge_idx <- which(edges$from == from_node & edges$to == to_node)
    edges$type[edge_idx]
  })
  
  # Count negative edges
  neg_count <- sum(cycle_edges == "negative")
  
  # Determine loop type
  type <- if(neg_count %% 2 == 0) "Reinforcing" else "Balancing"
  
  list(
    type = type,
    negative_edges = neg_count,
    total_edges = length(cycle_edges)
  )
}
# Helper function for leverage point analysis
analyze_leverage_points <- function(nodes, edges, cycles) {
  if (length(cycles) == 0) {
    return(NULL)
  }
  
  # Initialize node statistics
  node_stats <- data.frame(
    node_id = nodes$id,
    node_label = nodes$label,
    total_loops = 0,
    reinforcing_loops = 0,
    balancing_loops = 0,
    stringsAsFactors = FALSE
  )
  
  # Analyze each cycle
  for (cycle in cycles) {
    # Get cycle type
    analysis <- analyze_loop(cycle, edges)
    
    # Update statistics for each node in the cycle
    for (node_id in cycle) {
      idx <- which(node_stats$node_id == node_id)
      node_stats$total_loops[idx] <- node_stats$total_loops[idx] + 1
      
      if (analysis$type == "Reinforcing") {
        node_stats$reinforcing_loops[idx] <- node_stats$reinforcing_loops[idx] + 1
      } else {
        node_stats$balancing_loops[idx] <- node_stats$balancing_loops[idx] + 1
      }
    }
  }
  
  # Calculate leverage scores
  node_stats$leverage_score <- with(node_stats, {
    # Weight reinforcing loops slightly higher (1.2) than balancing loops (1.0)
    (reinforcing_loops * 1.2 + balancing_loops) / (max(total_loops, 1))
  })
  
  # Sort by leverage score
  node_stats <- node_stats[order(-node_stats$leverage_score), ]
  
  return(node_stats)
}
# Helper function to get node colors based on leverage scores
get_leverage_node_colors <- function(nodes, leverage_points, threshold) {
  nodes$value <- 1
  nodes$color <- sapply(1:nrow(nodes), function(i) {
    node_id <- nodes$id[i]
    leverage_score <- leverage_points$leverage_score[leverage_points$node_id == node_id]
    
    if (leverage_score >= threshold) {
      "rgba(255, 165, 0, 1)"  # Orange for high leverage nodes
    } else {
      "rgba(151, 194, 252, 0.3)"  # Dimmed light blue for other nodes
    }
  })
  return(nodes)
}

# Helper function to get edge colors based on leverage points
get_leverage_edge_colors <- function(edges, nodes_above_threshold) {
  edges$value <- 1
  edges$color <- sapply(1:nrow(edges), function(i) {
    if (edges$from[i] %in% nodes_above_threshold && 
        edges$to[i] %in% nodes_above_threshold) {
      if (edges$type[i] == "positive") {
        "rgba(43, 124, 233, 1)"  # Full opacity blue
      } else {
        "rgba(233, 43, 43, 1)"   # Full opacity red
      }
    } else {
      if (edges$type[i] == "positive") {
        "rgba(43, 124, 233, 0.15)"  # Dimmed blue
      } else {
        "rgba(233, 43, 43, 0.15)"   # Dimmed red
      }
    }
  })
  edges$dashes <- edges$type == "negative"
  return(edges)
}