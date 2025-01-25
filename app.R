library(shiny)
library(igraph)
library(bslib)
library(tidygraph)
library(ggraph)
library(DT)
library(visNetwork)
library(shinyjs)

analyze_loops <- function(g) {
  # Ensure the graph is directed
  if (!is_directed(g)) {
    g <- as_directed(g, mode = "mutual")
  }
  
  # Function to check if a sequence forms a directed cycle
  is_directed_cycle <- function(nodes, graph) {
    n <- length(nodes)
    for (i in 1:n) {
      from_node <- nodes[i]
      to_node <- nodes[if(i == n) 1 else i + 1]
      
      # Check if there's a directed edge from current node to next node
      if (length(E(graph)[from_node %->% to_node]) == 0) {
        return(FALSE)
      }
    }
    return(TRUE)  # All edges exist, it's a cycle
  }
  
  # Find all simple cycles in the graph
  all_cycles <- lapply(3:6, function(k) {
    # Get all possible k-length paths
    paths <- combn(V(g), k, simplify = FALSE)
    
    # Filter only those that form directed cycles
    cycles <- paths[sapply(paths, is_directed_cycle, graph = g)]
    
    # Return the cycles for this length
    if (length(cycles) > 0) {
      return(cycles)
    } else {
      return(NULL)
    }
  })
  
  # Remove NULL elements and flatten the list
  all_cycles <- unlist(all_cycles, recursive = FALSE)
  
  # Convert to unique cycles (remove rotations of same cycle)
  unique_cycles <- list()
  if (length(all_cycles) > 0) {
    for (cycle in all_cycles) {
      # Generate all rotations of current cycle
      rotations <- lapply(1:length(cycle), function(i) {
        c(cycle[i:length(cycle)], cycle[1:(i-1)])
      })
      
      # Check if any rotation exists in unique_cycles
      exists <- FALSE
      for (existing_cycle in unique_cycles) {
        if (any(sapply(rotations, function(r) identical(r, existing_cycle)))) {
          exists <- TRUE
          break
        }
      }
      
      if (!exists) {
        unique_cycles[[length(unique_cycles) + 1]] <- cycle
      }
    }
  }
  
  return(unique_cycles)
}

# Function to analyze loop dominance
analyze_loop_dominance <- function(g) {
  # Ensure the graph is directed
  if (!is_directed(g)) {
    g <- as_directed(g, mode = "mutual")
  }
  
  # Get all loops
  loop_analysis <- analyze_loops(g)
  all_cycles <- loop_analysis$cycles
  
  # Calculate dominance metrics for each loop
  loop_dominance <- list()
  loop_counter <- 1
  
  for(length_idx in 1:length(all_cycles)) {
    cycles_matrix <- all_cycles[[length_idx]]
    if(nrow(cycles_matrix) > 0) {
      for(i in 1:nrow(cycles_matrix)) {
        cycle <- cycles_matrix[i,]
        
        # Calculate metrics for this loop
        total_weight <- 0
        avg_centrality <- 0
        connections_to_other_loops <- 0
        
        # Calculate edge weights and connections
        for(j in 1:length(cycle)) {
          from_node <- cycle[j]
          to_node <- cycle[if(j == length(cycle)) 1 else j + 1]
          
          # Get edge weight (only for edges in the correct direction)
          edge_id <- get_edge_ids(g, c(from_node, to_node))
          if(length(edge_id) > 0) {
            total_weight <- total_weight + E(g)$weight[edge_id]
          }
          
          # Count connections to nodes outside this loop
          # For directed graphs, consider both in and out neighbors
          in_neighbors <- neighbors(g, from_node, mode = "in")
          out_neighbors <- neighbors(g, from_node, mode = "out")
          all_neighbors <- unique(c(in_neighbors, out_neighbors))
          connections_to_other_loops <- connections_to_other_loops + 
            sum(!all_neighbors %in% cycle)
        }
        
        # Calculate average centrality of nodes in the loop
        loop_nodes_centrality <- eigen_centrality(g)$vector[cycle]
        avg_centrality <- mean(loop_nodes_centrality)
        
        # Calculate dominance score
        dominance_score <- (total_weight * avg_centrality * 
                              (1 + log(1 + connections_to_other_loops)))
        
        loop_dominance[[loop_counter]] <- list(
          loop_id = loop_counter,
          nodes = paste(cycle, collapse="→"),
          length = length(cycle),
          total_weight = total_weight,
          avg_centrality = avg_centrality,
          external_connections = connections_to_other_loops,
          dominance_score = dominance_score
        )
        
        loop_counter <- loop_counter + 1
      }
    }
  }
  
  # Convert to data frame
  if(length(loop_dominance) > 0) {
    dominance_df <- do.call(rbind, lapply(loop_dominance, function(x) {
      data.frame(
        Loop_ID = x$loop_id,
        Nodes = x$nodes,
        Length = x$length,
        Total_Weight = x$total_weight,
        Avg_Centrality = x$avg_centrality,
        External_Connections = x$external_connections,
        Dominance_Score = x$dominance_score
      )
    }))
    # Sort by dominance score
    dominance_df <- dominance_df[order(-dominance_df$Dominance_Score),]
  } else {
    dominance_df <- data.frame(
      Loop_ID = numeric(0),
      Nodes = character(0),
      Length = numeric(0),
      Total_Weight = numeric(0),
      Avg_Centrality = numeric(0),
      External_Connections = numeric(0),
      Dominance_Score = numeric(0)
    )
  }
  
  return(dominance_df)
}
# New function to identify key drivers and outcomes
identify_key_nodes <- function(g) {
  # Calculate in and out degrees
  in_deg <- degree(g, mode = "in")
  out_deg <- degree(g, mode = "out")
  
  # Calculate PageRank scores
  pr_scores <- page_rank(g)$vector
  
  # Calculate HITS scores (both authority and hub)
  auth_scores <- authority_score(g)$vector
  hub_scores <- hub_score(g)$vector
  
  # Combine metrics
  node_roles <- data.frame(
    Node = 1:vcount(g),
    In_Degree = in_deg,
    Out_Degree = out_deg,
    PageRank = pr_scores,
    Authority = auth_scores,
    Hub = hub_scores
  )
  
  # Calculate driver and outcome scores
  node_roles$driver_score <- scale(node_roles$Out_Degree) + 
    scale(node_roles$Hub) + 
    scale(node_roles$PageRank)
  
  node_roles$outcome_score <- scale(node_roles$In_Degree) + 
    scale(node_roles$Authority)
  
  # Classify nodes
  node_roles$role <- ifelse(
    node_roles$driver_score > mean(node_roles$driver_score) + sd(node_roles$driver_score),
    "Key Driver",
    ifelse(
      node_roles$outcome_score > mean(node_roles$outcome_score) + sd(node_roles$outcome_score),
      "Key Outcome",
      "Intermediate"
    )
  )
  
  return(node_roles)
}

# Function to create a fixed sample graph with edge attributes
create_sample_graph <- function() {
  # Create a graph with 12 nodes and edge probability 0.3
  g <- sample_gnp(n = 15, p = 0.4)
  
  # Add edge attributes
  E(g)$weight <- runif(ecount(g), 1, 10)  # Keep existing weight attribute
  E(g)$strength <- runif(ecount(g), -5, 5)  # Add strength attribute (-5-+5)
  E(g)$confidence <- sample(1:5, ecount(g), replace = TRUE)  # Add confidence attribute (1-5)
  
  return(g)
}


# Function to calculate centrality metrics
calculate_centralities <- function(g) {
  data.frame(
    Node = 1:vcount(g),
    Degree = degree(g),
    Betweenness = betweenness(g),
    Closeness = closeness(g),
    Eigenvector = eigen_centrality(g)$vector,
    PageRank = page_rank(g)$vector
  )
}

# Function to detect communities
detect_communities <- function(g, method = "louvain") {
  if (method == "louvain") {
    comm <- cluster_louvain(g)
  } else if (method == "fast_greedy") {
    comm <- cluster_fast_greedy(g)
  } else {
    comm <- cluster_edge_betweenness(g)
  }
  return(comm)
}
# [Previous helper functions remain the same]

# Add new function to identify leverage points
identify_leverage_points <- function(g, top_n = 5) {
  # Calculate different centrality measures
  degree_cent <- degree(g)
  betweenness_cent <- betweenness(g)
  eigenvector_cent <- eigen_centrality(g)$vector
  pagerank_cent <- page_rank(g)$vector
  
  # Combine all measures
  leverage_df <- data.frame(
    Node = 1:vcount(g),
    Degree = degree_cent,
    Betweenness = betweenness_cent,
    Eigenvector = eigenvector_cent,
    PageRank = pagerank_cent
  )
  
  # Calculate composite score (normalized sum of all measures)
  leverage_df$composite_score <- scale(leverage_df$Degree) +
    scale(leverage_df$Betweenness) +
    scale(leverage_df$Eigenvector) +
    scale(leverage_df$PageRank)
  
  # Sort by composite score
  leverage_df[order(-leverage_df$composite_score)[1:top_n], ]
}

# Add new function to identify bottlenecks
identify_bottlenecks <- function(g) {
  # Find bridges (edges whose removal would disconnect the graph)
  bridges <- bridges(g)
  
  # Find articulation points (nodes whose removal would disconnect the graph)
  cut_vertices <- articulation_points(g)
  
  # Find high betweenness edges
  edge_betweenness <- edge_betweenness(g)
  high_betweenness_edges <- which(edge_betweenness > mean(edge_betweenness) + sd(edge_betweenness))
  
  list(
    bridges = bridges,
    cut_vertices = cut_vertices,
    high_betweenness_edges = high_betweenness_edges
  )
}

# [Previous UI code remains the same until the Analytics tab]

ui <- fluidPage(
  useShinyjs(),  #
  tags$head(
    tags$script(HTML("
    // Initialize global variables
    window.selectedLoopNodes = [];
    
    // Define highlight function
    window.highlightLoop = function(nodes) {
      if (typeof window.network !== 'undefined' && window.network.body) {
        var allNodes = window.network.body.data.nodes.get();
        var allEdges = window.network.body.data.edges.get();
        
        // Update nodes
        allNodes.forEach(function(node) {
          if (nodes.includes(parseInt(node.id))) {
            node.color = node.originalColor;
          } else {
            node.color = 'rgba(200,200,200,0.3)';
          }
        });
        
        // Update edges
        allEdges.forEach(function(edge) {
          var isLoopEdge = false;
          for (var i = 0; i < nodes.length; i++) {
            var currentNode = nodes[i];
            var nextNode = nodes[(i + 1) % nodes.length];
            if ((parseInt(edge.from) === currentNode && parseInt(edge.to) === nextNode) ||
                (parseInt(edge.from) === nextNode && parseInt(edge.to) === currentNode)) {
              isLoopEdge = true;
              break;
            }
          }
          
          if (isLoopEdge) {
            edge.color = edge.originalColor;
            edge.width = 2;
          } else {
            edge.color = 'rgba(200,200,200,0.3)';
            edge.width = 1;
          }
        });
        
        window.network.body.data.nodes.update(allNodes);
        window.network.body.data.edges.update(allEdges);
      }
    };
    
    // Function to reset highlighting
    window.resetHighlight = function() {
      if (typeof window.network !== 'undefined' && window.network.body) {
        var allNodes = window.network.body.data.nodes.get();
        var allEdges = window.network.body.data.edges.get();
        
        allNodes.forEach(function(node) {
          node.color = node.originalColor;
        });
        
        allEdges.forEach(function(edge) {
          edge.color = edge.originalColor;
          edge.width = 1;
        });
        
        window.network.body.data.nodes.update(allNodes);
        window.network.body.data.edges.update(allEdges);
      }
    };
  ")),
    tags$style(HTML("
    .selected-loop {
      background-color: #e6f3ff !important;
    }
  ")),
    tags$script(HTML("
    $(document).ready(function() {
      // Function to toggle chevron
      function toggleChevron(e) {
        $(e.target)
          .prev('.card-header')
          .find('i.fas')
          .toggleClass('fa-chevron-down fa-chevron-up');
      }
      
      // Add listeners for collapse events
      $('.collapse').on('show.bs.collapse', toggleChevron);
      $('.collapse').on('hide.bs.collapse', toggleChevron);
      
      // Show/hide specific options based on layout selection
      $('#layout').change(function() {
        if (this.value === 'barnesHut') {
          $('#barnesHutOptions').collapse('show');
          $('#frOptions').collapse('hide');
        } else if (this.value === 'fruchterman.reingold') {
          $('#barnesHutOptions').collapse('hide');
          $('#frOptions').collapse('show');
        } else {
          $('#barnesHutOptions').collapse('hide');
          $('#frOptions').collapse('hide');
        }
      });
    });
  ")),
    tags$link(rel = "stylesheet", 
              href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")
  ),
  theme = bs_theme(version = 5),
  
  tabsetPanel(
    # Tab 1: Network Visualization (unchanged)
    tabPanel(
      "Network Visualization",
      fluidRow(
        column(3,
               div(class = "card",
                   div(class = "card-header", 
                       HTML('<a data-bs-toggle="collapse" href="#layoutControls" role="button" aria-expanded="true">
                    Layout Controls <i class="fas fa-chevron-down float-end"></i>
                    </a>')),
                   div(class = "collapse show", id = "layoutControls",
                       div(class = "card-body",
                           selectInput("layout", "Layout Algorithm",
                                       choices = c("barnesHut", "random", "fruchterman.reingold",
                                                   "kamada.kawai", "graphopt")),
                           
                           # Barnes Hut layout options
                           div(class = "collapse", id = "barnesHutOptions",
                               conditionalPanel(
                                 condition = "input.layout == 'barnesHut'",
                                 numericInput("barnes_grav", "Gravitational Constant", value = -2000, step = 100),
                                 numericInput("barnes_cent_grav", "Central Gravity", value = 0.3, step = 0.1),
                                 numericInput("barnes_spring_len", "Spring Length", value = 95, step = 5),
                                 numericInput("barnes_spring_const", "Spring Constant", value = 0.04, step = 0.01),
                                 numericInput("barnes_damping", "Damping", value = 0.09, step = 0.01),
                                 numericInput("barnes_avoid", "Avoid Overlap", value = 1, min = 0, max = 1, step = 0.1)
                               )),
                           
                           # Fruchterman-Reingold layout options
                           div(class = "collapse", id = "frOptions",
                               conditionalPanel(
                                 condition = "input.layout == 'fruchterman.reingold'",
                                 numericInput("vis_gravity", "Gravity", value = -50, step = 5),
                                 numericInput("vis_spring_length", "Spring Length", value = 100, step = 10),
                                 numericInput("vis_spring_strength", "Spring Strength", value = 0.2, step = 0.1),
                                 numericInput("vis_damping", "Damping", value = 0.09, step = 0.01)
                               )),
                           
                           # General layout options
                           div(class = "collapse show", id = "generalOptions",
                               hr(),
                               strong("General Options"),
                               numericInput("node_spacing", "Node Spacing", value = 100, step = 10),
                               checkboxInput("avoid_overlap", "Avoid Node Overlap", value = TRUE)
                           )
                       ),
                       br(),
                       div(class = "card",
                           div(class = "card-header", 
                               HTML('<a data-bs-toggle="collapse" href="#loopsList" role="button" aria-expanded="true">
                    Found Loops <i class="fas fa-chevron-down float-end"></i>
                    </a>')),
                           div(class = "collapse show", id = "loopsList",
                               div(class = "card-body",
                                   DTOutput("loops_summary_table")
                               )
                           )
                       ) 
                       
                   )
               )
               
        ),
        column(9,
               div(class = "card",
                   div(class = "card-header", "Network Graph"),
                   div(class = "card-body",
                       visNetworkOutput("network_plot_interactive", height = "600px")
                   )
               )
        )
      )
    ),
    
    # Tab 2: Centrality Metrics
    tabPanel("Centrality Metrics",
             div(class = "card",
                 div(class = "card-header", "Centrality Metrics"),
                 div(class = "card-body",
                     DTOutput("centrality_table")
                 )
             )
    ),
    
    # Tab 3: Community Detection
    tabPanel("Community Detection",
             div(class = "card",
                 div(class = "card-header", "Community Detection"),
                 div(class = "card-body",
                     selectInput("comm_method", "Community Detection Method",
                                 choices = c("louvain", "fast_greedy", "edge_betweenness")),
                     verbatimTextOutput("community_summary"),
                     plotOutput("community_plot", height = "400px")
                 )
             )
    ),
    
    # Tab 4: Node Roles
    tabPanel("Node Roles",
             div(class = "card",
                 div(class = "card-header", "Key Drivers and Outcomes Analysis"),
                 div(class = "card-body",
                     fluidRow(
                       column(6,
                              h4("Node Roles Table"),
                              DTOutput("node_roles_table")
                       ),
                       column(6,
                              h4("Network Role Visualization"),
                              plotOutput("roles_plot", height = "400px")
                       )
                     )
                 )
             )
    ),
    
    # Tab 5: Leverage Points
    tabPanel("Leverage Points",
             fluidRow(
               column(6,
                      div(class = "card",
                          div(class = "card-header", "Leverage Points"),
                          div(class = "card-body",
                              numericInput("top_n_leverage", "Number of top leverage points to show:", 5, min = 1, max = 20),
                              DTOutput("leverage_points_table")
                          )
                      )
               ),
               column(6,
                      div(class = "card",
                          div(class = "card-header", "Leverage Points Visualization"),
                          div(class = "card-body",
                              plotOutput("leverage_plot", height = "400px")
                          )
                      )
               )
             )
    ),
    
    # Tab 6: Bottlenecks
    tabPanel("Bottlenecks",
             div(class = "card",
                 div(class = "card-header", "Network Bottlenecks"),
                 div(class = "card-body",
                     verbatimTextOutput("bottlenecks_summary"),
                     plotOutput("bottlenecks_plot", height = "300px")
                 )
             ),
             br(),
             fluidRow(
               column(4,
                      div(class = "card",
                          div(class = "card-header", "Average Path Length"),
                          div(class = "card-body",
                              textOutput("avg_path")
                          )
                      )
               ),
               column(4,
                      div(class = "card",
                          div(class = "card-header", "Network Density"),
                          div(class = "card-body",
                              textOutput("density")
                          )
                      )
               ),
               column(4,
                      div(class = "card",
                          div(class = "card-header", "Network Diameter"),
                          div(class = "card-body",
                              textOutput("diameter")
                          )
                      )
               )
             )
    ),
    
    # Tab 7: Loop Analysis
    tabPanel("Loop Analysis",
             fluidRow(
               column(6,
                      div(class = "card",
                          div(class = "card-header", "Loop Metrics"),
                          div(class = "card-body",
                              DTOutput("loop_metrics_table")
                          )
                      ),
                      br(),
                      div(class = "card",
                          div(class = "card-header", "Nodes in Loops"),
                          div(class = "card-body",
                              DTOutput("loop_nodes_table")
                          )
                      )
               ),
               column(6,
                      div(class = "card",
                          div(class = "card-header", "Loop Visualization"),
                          div(class = "card-body",
                              selectInput("loop_length", "Highlight Loops of Length:",
                                          choices = c("3" = 3, "4" = 4, "5" = 5, "6" = 6)),
                              plotOutput("loop_plot", height = "500px")
                          )
                      )
               )
             ),
             br(),
             fluidRow(
               column(12,
                      div(class = "card",
                          div(class = "card-header", "Loop Dominance Analysis"),
                          div(class = "card-body",
                              fluidRow(
                                column(6,
                                       DTOutput("loop_dominance_table")
                                ),
                                column(6,
                                       plotOutput("loop_dominance_plot", height = "400px")
                                )
                              )
                          )
                      )
               )
             )
    )
  )
)
server <- function(input, output, session) {
  # [Previous server code remains the same]
  # Reactive value for storing the graph
  graph_data <- reactiveVal(create_sample_graph())
  
  # Generate new graph when button is clicked
  observeEvent(input$regenerate, {
    graph_data(create_sample_graph(input$n_nodes, input$edge_prob))
  })
  
  # Add edge when button is clicked
  observeEvent(input$add_edge_btn, {
    if (input$add_edge != "") {
      try({
        edge_pair <- as.numeric(strsplit(input$add_edge, ",")[[1]])
        if (length(edge_pair) == 2) {
          current_graph <- graph_data()
          new_graph <- add_edges(current_graph, edge_pair)
          E(new_graph)$weight[ecount(new_graph)] <- runif(1, 1, 10)
          graph_data(new_graph)
        }
      })
    }
  })
  
  
  
  output$network_plot_interactive <- renderVisNetwork({
    g <- graph_data()
    
    # Define groups and their shapes
    groups <- c("Activities", "Pressures", "Drivers", 
                "Societal Goods and Services", "Ecosystem Services", 
                "Marine Processes")
    
    # Assign random groups to nodes
    set.seed(123) # for reproducibility
    node_groups <- sample(groups, vcount(g), replace = TRUE)
    
    nodes <- data.frame(
      id = 1:vcount(g),
      label = as.character(1:vcount(g)),
      group = node_groups,
      size = 20,
      shape = sapply(node_groups, function(g) {
        switch(g,
               "Activities" = "diamond",
               "Pressures" = "square",
               "Drivers" = "hexagon",
               "Societal Goods and Services" = "star",
               "Ecosystem Services" = "triangle",
               "Marine Processes" = "dot")
      }),
      color = sapply(node_groups, function(g) {
        switch(g,
               "Activities" = "#FF9999",
               "Pressures" = "#99FF99",
               "Drivers" = "#9999FF",
               "Societal Goods and Services" = "#FFFF99",
               "Ecosystem Services" = "#FF99FF",
               "Marine Processes" = "#99FFFF")
      }),
      title = paste("Node:", 1:vcount(g), "<br>Group:", node_groups)  # Tooltip for nodes
    )
    
    # Store original colors in the nodes data frame
    nodes$originalColor <- nodes$color
    
    # Create edges data frame with original colors
    edges <- as.data.frame(as_edgelist(g))
    colnames(edges) <- c("from", "to")
    edges$width <- E(g)$weight
    
    # Add tooltip information to edges
    edges$title <- paste0(
      "Weight: ", round(E(g)$weight, 2),
      "<br>Strength: ", round(E(g)$strength, 2),
      "<br>Confidence: ", E(g)$confidence
    )
    
    # Scale confidence to alpha values (1-5 to 0.2-1)
    alpha_values <- scales::rescale(E(g)$confidence, to = c(0.2, 1))
    
    # Create hex color values with alpha and store original colors
    edges$color <- sapply(1:nrow(edges), function(i) {
      if (E(g)$strength[i] < 0) {
        rgb_color <- col2rgb("red")
        sprintf("rgba(%d, %d, %d, %f)", 
                rgb_color[1], rgb_color[2], rgb_color[3], 
                alpha_values[i])
      } else {
        rgb_color <- col2rgb("darkgreen")
        sprintf("rgba(%d, %d, %d, %f)", 
                rgb_color[1], rgb_color[2], rgb_color[3], 
                alpha_values[i])
      }
    })
    edges$originalColor <- edges$color
    
    # Find this section in the network visualization code and replace the visEvents part:
    network <- visNetwork(nodes, edges, main = "Network") %>%
      visOptions(
        selectedBy = list(
          variable = "group",
          main = "Select by group"
        )
      ) %>%
      visInteraction(
        hover = TRUE,
        hoverConnectedEdges = TRUE,
        selectConnectedEdges = TRUE
      ) %>%
      visEvents(
        type = "once",
        startStabilizing = "function() {
      this.moveTo({scale: 0.8});
    }",
        stabilized = "function() {
      window.network = this;
    }",
        afterDrawing = "function() {
      if (window.selectedLoopNodes && window.selectedLoopNodes.length > 0) {
        window.highlightLoop(window.selectedLoopNodes);
      }
    }",
        selectNode = "function(data) {
      // Store node selection
      var selectedNode = data.nodes[0];
      var allNodes = this.body.data.nodes.get();
      var allEdges = this.body.data.edges.get();
      var connected = this.getConnectedNodes(selectedNode);
      
      // Reset all nodes and edges first
      allNodes.forEach(function(node) {
        node.color = node.originalColor;
      });
      allEdges.forEach(function(edge) {
        edge.color = edge.originalColor;
        edge.width = 1;
      });
      
      // If we have a selected loop, prioritize it
      if (window.selectedLoopNodes && window.selectedLoopNodes.length > 0) {
        // Gray out non-loop nodes
        allNodes.forEach(function(node) {
          if (!window.selectedLoopNodes.includes(parseInt(node.id))) {
            node.color = 'rgba(200,200,200,0.3)';
          }
        });
        
        // Update edges for loop
        allEdges.forEach(function(edge) {
          var isLoopEdge = false;
          for (var i = 0; i < window.selectedLoopNodes.length; i++) {
            var currentNode = window.selectedLoopNodes[i];
            var nextNode = window.selectedLoopNodes[(i + 1) % window.selectedLoopNodes.length];
            if ((parseInt(edge.from) === currentNode && parseInt(edge.to) === nextNode) ||
                (parseInt(edge.from) === nextNode && parseInt(edge.to) === currentNode)) {
              isLoopEdge = true;
              break;
            }
          }
          if (!isLoopEdge) {
            edge.color = 'rgba(200,200,200,0.3)';
            edge.width = 1;
          } else {
            edge.width = 2;
          }
        });
      } else {
        // No loop selected, highlight connected nodes and edges
        allNodes.forEach(function(node) {
          if (node.id !== selectedNode && !connected.includes(node.id)) {
            node.color = 'rgba(200,200,200,0.3)';
          }
        });
        
        allEdges.forEach(function(edge) {
          if (edge.from === selectedNode || edge.to === selectedNode) {
            edge.width = 2;
          } else {
            edge.color = 'rgba(200,200,200,0.3)';
            edge.width = 1;
          }
        });
      }
      
      // Update the network
      this.body.data.nodes.update(allNodes);
      this.body.data.edges.update(allEdges);
      
      // Send selection to Shiny
      Shiny.setInputValue('selected_type', 'node');
      Shiny.setInputValue('selected_id', selectedNode);
    }",
        deselectNode = "function(params) {
      var allNodes = this.body.data.nodes.get();
      var allEdges = this.body.data.edges.get();
      
      // If we have a selected loop, maintain its highlighting
      if (window.selectedLoopNodes && window.selectedLoopNodes.length > 0) {
        allNodes.forEach(function(node) {
          if (window.selectedLoopNodes.includes(parseInt(node.id))) {
            node.color = node.originalColor;
          } else {
            node.color = 'rgba(200,200,200,0.3)';
          }
        });
        
        allEdges.forEach(function(edge) {
          var isLoopEdge = false;
          for (var i = 0; i < window.selectedLoopNodes.length; i++) {
            var currentNode = window.selectedLoopNodes[i];
            var nextNode = window.selectedLoopNodes[(i + 1) % window.selectedLoopNodes.length];
            if ((parseInt(edge.from) === currentNode && parseInt(edge.to) === nextNode) ||
                (parseInt(edge.from) === nextNode && parseInt(edge.to) === currentNode)) {
              isLoopEdge = true;
              break;
            }
          }
          if (isLoopEdge) {
            edge.color = edge.originalColor;
            edge.width = 2;
          } else {
            edge.color = 'rgba(200,200,200,0.3)';
            edge.width = 1;
          }
        });
      } else {
        // No loop selected, reset to original colors
        allNodes.forEach(function(node) {
          node.color = node.originalColor;
        });
        allEdges.forEach(function(edge) {
          edge.color = edge.originalColor;
          edge.width = 1;
        });
      }
      
      // Update the network
      this.body.data.nodes.update(allNodes);
      this.body.data.edges.update(allEdges);
      
      // Clear selection in Shiny
      Shiny.setInputValue('selected_type', null);
      Shiny.setInputValue('selected_id', null);
    }"
      ) %>%
      visGroups(
        groupname = "Activities", 
        shape = "diamond", 
        color = list(background = "#FF9999", border = "#FF9999")
      ) %>%
      visGroups(
        groupname = "Pressures", 
        shape = "square", 
        color = list(background = "#99FF99", border = "#99FF99")
      ) %>%
      visGroups(
        groupname = "Drivers", 
        shape = "hexagon", 
        color = list(background = "#9999FF", border = "#9999FF")
      ) %>%
      visGroups(
        groupname = "Societal Goods and Services", 
        shape = "star", 
        color = list(background = "#FFFF99", border = "#FFFF99")
      ) %>%
      visGroups(
        groupname = "Ecosystem Services", 
        shape = "triangle", 
        color = list(background = "#FF99FF", border = "#FF99FF")
      ) %>%
      visGroups(
        groupname = "Marine Processes", 
        shape = "dot", 
        color = list(background = "#99FFFF", border = "#99FFFF")
      ) %>%
      visLegend(
        useGroups = TRUE,
        position = "left",
        main = "Node Types"
      ) %>%
      visEdges(
        smooth = FALSE,
        arrows = "to",
        color = list(
          inherit = FALSE,
          highlight = "#000000"
        )
      )
    
    # Apply layout-specific options
    if (input$layout == "fruchterman.reingold") {
      network <- network %>%
        visPhysics(
          solver = "forceAtlas2Based",
          stabilization = FALSE,
          forceAtlas2Based = list(
            gravitationalConstant = input$vis_gravity,
            centralGravity = 0.01,
            springLength = input$vis_spring_length,
            springConstant = input$vis_spring_strength,
            damping = input$vis_damping,
            avoidOverlap = 0.5
          )
        )
    } else if (input$layout == "barnesHut") {
      network <- network %>%
        visPhysics(
          solver = "barnesHut",
          enabled = TRUE,
          stabilization = FALSE,
          barnesHut = list(
            gravitationalConstant = input$barnes_grav,
            centralGravity = input$barnes_cent_grav,
            springLength = input$barnes_spring_len,
            springConstant = input$barnes_spring_const,
            damping = input$barnes_damping,
            avoidOverlap = input$barnes_avoid
          )
        )
    } else if (input$layout == "random") {
      network <- network %>%
        visPhysics(enabled = FALSE) %>%
        visLayout(
          randomSeed = sample.int(1000, 1)
        )
    } else if (input$layout == "kamada.kawai") {
      network <- network %>%
        visPhysics(
          solver = "repulsion",
          stabilization = FALSE,
          repulsion = list(
            nodeDistance = 200,
            centralGravity = 0.2,
            springLength = 200,
            springConstant = 0.05,
            damping = 0.09
          )
        )
    } else if (input$layout == "graphopt") {
      network <- network %>%
        visPhysics(
          solver = "forceAtlas2Based",
          stabilization = FALSE,
          forceAtlas2Based = list(
            gravitationalConstant = -50,
            centralGravity = 0.01,
            springLength = 200,
            springConstant = 0.08,
            damping = 0.4,
            avoidOverlap = 1
          )
        )
    }
    
    # Apply general options
    network <- network %>%
      visOptions(
        nodesIdSelection = TRUE,
        manipulation = TRUE
      )
    
    # Apply node spacing and overlap avoidance if requested
    if (input$avoid_overlap) {
      network <- network %>%
        visPhysics(
          minVelocity = 0.75,
          solver = "repulsion",
          repulsion = list(
            nodeDistance = input$node_spacing
          )
        )
    }
    
    network
  })
  # Render centrality metrics table
  output$centrality_table <- renderDT({
    g <- graph_data()
    centralities <- calculate_centralities(g)
    datatable(centralities, 
              options = list(pageLength = 10),
              rownames = FALSE)
  })
  
  # Render community detection results
  output$community_summary <- renderPrint({
    g <- graph_data()
    comm <- detect_communities(g, input$comm_method)
    print(comm)
  })
  
  # Render community plot
  output$community_plot <- renderPlot({
    g <- graph_data()
    comm <- detect_communities(g, input$comm_method)
    plot(comm, g, main = "Community Structure")
  })
  
  # Render network metrics
  output$avg_path <- renderText({
    g <- graph_data()
    round(mean_distance(g), 2)
  })
  
  output$density <- renderText({
    g <- graph_data()
    round(edge_density(g), 3)
  })
  
  output$diameter <- renderText({
    g <- graph_data()
    diameter(g)
  })
  # Add new outputs for leverage points and bottlenecks
  output$leverage_points_table <- renderDT({
    g <- graph_data()
    leverage_points <- identify_leverage_points(g, input$top_n_leverage)
    datatable(leverage_points,
              options = list(pageLength = 5),
              rownames = FALSE) %>%
      formatRound(columns = c("Betweenness", "Eigenvector", "PageRank", "composite_score"), digits = 3)
  })
  output$leverage_plot <- renderPlot({
    g <- graph_data()
    leverage_points <- identify_leverage_points(g, input$top_n_leverage)
    
    # Highlight leverage points in the graph
    V(g)$color <- "lightgray"  # Default color for non-leverage points
    V(g)$size <- 15           # Default size for non-leverage points
    
    # Get the nodes that are leverage points
    leverage_nodes <- leverage_points$Node
    
    # Color and size based on leverage point status
    V(g)$color[leverage_nodes] <- "orange"
    V(g)$size[leverage_nodes] <- 25
    
    # Create the plot
    plot(g,
         vertex.label = V(g),
         vertex.color = V(g)$color,
         vertex.size = V(g)$size,
         edge.arrow.size = 0.5,
         main = "Network Leverage Points Visualization",
         sub = "Orange: Leverage Points, Gray: Other Nodes")
  })
  output$bottlenecks_summary <- renderPrint({
    g <- graph_data()
    bottlenecks <- identify_bottlenecks(g)
    cat("Bridges (Critical Edges):\n")
    if(length(bottlenecks$bridges) > 0) {
      cat(paste("Edge", bottlenecks$bridges, collapse = "\n"))
    } else {
      cat("No bridges found\n")
    }
    cat("\nArticulation Points (Critical Nodes):\n")
    if(length(bottlenecks$cut_vertices) > 0) {
      cat(paste("Node", bottlenecks$cut_vertices, collapse = "\n"))
    } else {
      cat("No articulation points found\n")
    }
  })
  
  output$bottlenecks_plot <- renderPlot({
    g <- graph_data()
    bottlenecks <- identify_bottlenecks(g)
    
    # Create a plot highlighting bottlenecks
    V(g)$color <- "lightblue"
    V(g)$color[bottlenecks$cut_vertices] <- "red"
    E(g)$color <- "gray"
    E(g)$color[bottlenecks$bridges] <- "red"
    E(g)$color[bottlenecks$high_betweenness_edges] <- "orange"
    
    plot(g,
         vertex.label = V(g),
         vertex.size = 20,
         edge.width = 2,
         main = "Network Bottlenecks",
         sub = "Red: Critical nodes/edges, Orange: High betweenness edges")
  })
  
  # Add new outputs for key drivers and outcomes analysis
  output$node_roles_table <- renderDT({
    g <- graph_data()
    # Convert to directed graph if not already
    if (!is_directed(g)) {
      g <- as_directed(g, mode = "mutual")
    }
    
    roles_df <- identify_key_nodes(g)
    datatable(roles_df,
              options = list(pageLength = 5),
              rownames = FALSE) %>%
      formatRound(columns = c("PageRank", "Authority", "Hub", 
                              "driver_score", "outcome_score"), 
                  digits = 3) %>%
      formatStyle(
        'role',
        backgroundColor = styleEqual(
          c("Key Driver", "Key Outcome", "Intermediate"),
          c('#ffcdd2', '#c8e6c9', '#ffffff')
        )
      )
  })
  
  output$roles_plot <- renderPlot({
    g <- graph_data()
    # Convert to directed graph if not already
    if (!is_directed(g)) {
      g <- as_directed(g, mode = "mutual")
    }
    
    roles_df <- identify_key_nodes(g)
    
    # Add role information to graph
    V(g)$role <- roles_df$role
    V(g)$size <- ifelse(V(g)$role == "Intermediate", 15,
                        ifelse(V(g)$role == "Key Driver", 25, 20))
    V(g)$color <- ifelse(V(g)$role == "Key Driver", "red",
                         ifelse(V(g)$role == "Key Outcome", "green", "lightblue"))
    
    # Create plot
    plot(g,
         vertex.label = V(g),
         vertex.size = V(g)$size,
         vertex.color = V(g)$color,
         edge.arrow.size = 0.5,
         main = "Network Roles Visualization",
         sub = "Red: Key Drivers, Green: Key Outcomes, Blue: Intermediate")
  })
  # Render loop metrics table
  output$loop_metrics_table <- renderDT({
    g <- graph_data()
    # Convert to directed if not already
    if (!is_directed(g)) {
      g <- as_directed(g, mode = "mutual")
    }
    
    loop_analysis <- analyze_loops(g)
    datatable(loop_analysis$metrics,
              options = list(pageLength = 10),
              rownames = FALSE)
  })
  
  # Render loop nodes table
  output$loop_nodes_table <- renderDT({
    g <- graph_data()
    if (!is_directed(g)) {
      g <- as_directed(g, mode = "mutual")
    }
    
    loop_analysis <- analyze_loops(g)
    node_freq <- as.data.frame(loop_analysis$node_frequencies)
    colnames(node_freq) <- c("Node", "Times_in_Loops")
    
    datatable(node_freq,
              options = list(pageLength = 10),
              rownames = FALSE) %>%
      formatStyle('Times_in_Loops',
                  background = styleColorBar(c(0, max(node_freq$Times_in_Loops)), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  })
  
  # Render loop visualization
  output$loop_plot <- renderPlot({
    g <- graph_data()
    if (!is_directed(g)) {
      g <- as_directed(g, mode = "mutual")
    }
    
    loop_analysis <- analyze_loops(g)
    selected_length <- as.numeric(input$loop_length)
    cycles <- loop_analysis$cycles[[selected_length - 2]]
    
    # Color edges and nodes involved in loops
    V(g)$color <- "lightgray"
    E(g)$color <- "lightgray"
    E(g)$width <- 1
    
    if(nrow(cycles) > 0) {
      # Highlight nodes in cycles
      cycle_nodes <- unique(as.vector(cycles))
      V(g)$color[cycle_nodes] <- "orange"
      
      # Highlight edges in cycles
      for(i in 1:nrow(cycles)) {
        cycle <- cycles[i,]
        for(j in 1:(length(cycle))) {
          from_node <- cycle[j]
          to_node <- cycle[if(j == length(cycle)) 1 else j + 1]
          # Using the updated get_edge_ids() function instead of get.edge.ids()
          edge_id <- get_edge_ids(g, c(from_node, to_node))
          if(length(edge_id) > 0) {
            E(g)$color[edge_id] <- "red"
            E(g)$width[edge_id] <- 2
          }
        }
      }
    }
    
    # Create the plot
    plot(g,
         vertex.label = V(g),
         vertex.size = 20,
         edge.arrow.size = 0.5,
         main = paste("Loops of Length", selected_length),
         sub = "Orange: Nodes in loops, Red: Edges in loops")
  })
  # Render loop dominance table
  output$loop_dominance_table <- renderDT({
    g <- graph_data()
    if (!is_directed(g)) {
      g <- as_directed(g, mode = "mutual")
    }
    
    dominance_df <- analyze_loop_dominance(g)
    
    if(nrow(dominance_df) > 0) {
      datatable(dominance_df,
                options = list(pageLength = 5),
                rownames = FALSE) %>%
        formatRound(columns = c("Total_Weight", "Avg_Centrality", 
                                "Dominance_Score"), digits = 3) %>%
        formatStyle('Dominance_Score',
                    background = styleColorBar(c(0, max(dominance_df$Dominance_Score)), 
                                               'lightblue'),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center')
    }
  })
  # Render loops summary table for first tab
  # Create reactive value for selected loop
  selected_loop <- reactiveVal(NULL)
  
  # Render loops summary table for first tab
  output$loops_summary_table <- renderDT({
    g <- graph_data()
    if (!is_directed(g)) {
      g <- as_directed(g, mode = "mutual")
    }
    
    dominance_df <- analyze_loop_dominance(g)
    
    if(nrow(dominance_df) > 0) {
      # Create a simplified version of the dominance table
      summary_df <- data.frame(
        Loop = dominance_df$Nodes,
        Length = dominance_df$Length,
        Score = round(dominance_df$Dominance_Score, 2)
      )
      
      datatable(summary_df,
                options = list(
                  pageLength = 5,
                  dom = 't',  # Only show table, no search/pagination
                  scrollY = "200px",
                  scrollCollapse = TRUE
                ),
                selection = 'single',  # Enable single row selection
                rownames = FALSE) %>%
        formatStyle('Score',
                    background = styleColorBar(c(0, max(summary_df$Score)), 'lightblue'),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center')
    } else {
      # Return empty table with message
      datatable(data.frame(Message = "No loops found in the network"),
                options = list(dom = 't'),
                rownames = FALSE)
    }
  })
  
  # Observer for loop selection
  # Observer for loop selection
  observeEvent(input$loops_summary_table_rows_selected, {
    selected_idx <- input$loops_summary_table_rows_selected
    
    if (!is.null(selected_idx)) {
      g <- graph_data()
      if (!is_directed(g)) {
        g <- as_directed(g, mode = "mutual")
      }
      
      dominance_df <- analyze_loop_dominance(g)
      if(nrow(dominance_df) > 0) {
        # Get nodes in the selected loop
        selected_nodes <- as.numeric(strsplit(dominance_df$Nodes[selected_idx], "→")[[1]])
        
        # Set the selected nodes and trigger highlighting
        runjs(sprintf("
          window.selectedLoopNodes = %s;
          window.highlightLoop(window.selectedLoopNodes);
        ", jsonlite::toJSON(selected_nodes)))
      }
    } else {
      # Clear the selected loop and reset visualization
      runjs("
        window.selectedLoopNodes = [];
        if (typeof window.network !== 'undefined') {
          var allNodes = window.network.body.data.nodes.get();
          var allEdges = window.network.body.data.edges.get();
          
          allNodes.forEach(function(node) {
            node.color = node.originalColor;
          });
          
          allEdges.forEach(function(edge) {
            edge.color = edge.originalColor;
            edge.width = 1;
          });
          
          window.network.body.data.nodes.update(allNodes);
          window.network.body.data.edges.update(allEdges);
        }
      ")
    }
  })
}
app <- shinyApp(ui, server)
