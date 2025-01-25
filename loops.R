library(shiny)
library(visNetwork)
library(igraph)
library(bslib)
library(shinyjs)

# Source helper functions
source("helpers.R")

ui <- page_sidebar(
  title = "Network Loop Detector",
  theme = bs_theme(version = 5),
  
  # Enable shinyjs
  useShinyjs(),
  
  # Sidebar with controls
  sidebar = sidebar(
    h4("Network Controls"),
    numericInput("num_nodes", "Number of Nodes:", 
                 value = 8, min = 3, max = 20),
    numericInput("num_edges", "Number of Edges:", 
                 value = 12, min = 3, max = 40),
    actionButton("generate", "Generate New Network", 
                 class = "btn-primary"),
    hr(),
    uiOutput("loop_selector")  # Dynamic loop selection UI
  ),
  
  # Main panel with network visualization and loop info
  tabsetPanel(
    tabPanel("Network",
             card(
               card_header("Network Visualization"),
               card_body(
                 visNetworkOutput("network_plot", height = "600px")
               )
             )
    ),
    tabPanel("Detected Loops",
             card(
               card_header("Loop Information"),
               card_body(
                 uiOutput("loop_info")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value to store current graph
  graph_data <- reactiveVal()
  
  observeEvent(input$generate, {
    # Create random graph using igraph
    g <- sample_gnm(n = input$num_nodes, 
                    m = min(input$num_edges, input$num_nodes * (input$num_nodes - 1)),
                    directed = TRUE)
    
    # Find cycles in the graph
    cycles <- find_cycles(g)
    
    # Convert to visNetwork format
    nodes <- data.frame(
      id = 1:vcount(g),
      label = paste("Node", 1:vcount(g)),
      title = paste("Node", 1:vcount(g))
    )
    
    edges <- as_data_frame(g, what = "edges")
    names(edges) <- c("from", "to")
    
    # Add random edge types (positive/negative)
    edges$type <- sample(c("positive", "negative"), 
                         size = nrow(edges), 
                         replace = TRUE, 
                         prob = c(0.7, 0.3))  # 70% positive, 30% negative
    
    # Store graph data and cycles
    graph_data(list(
      nodes = nodes,
      edges = edges,
      cycles = cycles
    ))
  })
  
  # Dynamic UI for loop selection
  output$loop_selector <- renderUI({
    req(graph_data())
    cycles <- graph_data()$cycles
    
    if (length(cycles) == 0) {
      return(div(
        style = "color: #666;",
        "No loops detected in the network"
      ))
    }
    
    # Create choices for selectInput
    choices <- lapply(seq_along(cycles), function(i) {
      cycle_str <- paste(cycles[[i]], collapse = " -> ")
      paste("Loop", i, ":", cycle_str, "->", cycles[[i]][1])
    })
    choices <- setNames(seq_along(cycles), unlist(choices))
    
    selectInput("selected_loop", 
                "Select Loop to Highlight:",
                choices = c("None" = "0", choices),
                selected = "0"
    )
  })
  
  
  # Modified network_plot render to include edge styling:
  output$network_plot <- renderVisNetwork({
    req(graph_data())
    
    data <- graph_data()
    
    # Create base network
    edges <- data$edges
    edges$width <- 1
    nodes <- data$nodes
    
    # Highlight selected loop by dimming other edges and nodes
    selected <- try(as.numeric(input$selected_loop), silent = TRUE)
    if (!inherits(selected, "try-error") && !is.na(selected) && selected != 0) {
      if (selected <= length(data$cycles)) {
        cycle <- data$cycles[[selected]]
        
        # Get cycle edge matrix
        cycle_edges <- create_cycle_edge_matrix(cycle)
        
        # Update colors based on cycle membership
        edges <- get_edge_colors(edges, cycle_edges)
        nodes <- get_node_colors(nodes, cycle)
      }
    } else {
      # Set default colors when no loop is selected
      colored_data <- set_default_colors(nodes, edges)
      nodes <- colored_data$nodes
      edges <- colored_data$edges
    }
    
    visNetwork(nodes, edges) %>%
      visNodes(
        borderWidth = 1,
        borderWidthSelected = 2,
        color = list(
          border = "#2B7CE9",
          highlight = list(
            background = "#97C2FC",
            border = "#2B7CE9"
          )
        )
      ) %>%
      visEdges(
        arrows = "to",
        smooth = FALSE  # Makes dashed lines more visible
      ) %>%
      visLayout(randomSeed = 123)
  })
  # Display information about detected loops
  output$loop_info <- renderUI({
    req(graph_data())
    
    cycles <- graph_data()$cycles
    edges <- graph_data()$edges
    
    if (length(cycles) == 0) {
      return(div(
        style = "color: #666;",
        "No loops detected in the network"
      ))
    }
    
    # Analyze all loops
    loop_analyses <- lapply(cycles, analyze_loop, edges = edges)
    
    # Count loop types
    reinforcing_count <- sum(sapply(loop_analyses, function(x) x$type == "Reinforcing"))
    balancing_count <- sum(sapply(loop_analyses, function(x) x$type == "Balancing"))
    
    # Create the output
    tagList(
      h4("Loop Analysis Summary"),
      div(
        style = "margin-bottom: 20px;",
        tags$ul(
          tags$li(sprintf("Total loops detected: %d", length(cycles))),
          tags$li(sprintf("Reinforcing loops: %d", reinforcing_count)),
          tags$li(sprintf("Balancing loops: %d", balancing_count))
        )
      ),
      h4("Detailed Loop Information"),
      div(
        style = "margin-bottom: 20px;",
        lapply(seq_along(cycles), function(i) {
          analysis <- loop_analyses[[i]]
          cycle_str <- paste(cycles[[i]], collapse = " -> ")
          div(
            style = "margin-bottom: 15px; padding: 10px; border-left: 4px solid #2B7CE9; background-color: #f8f9fa;",
            h5(sprintf("Loop %d", i)),
            tags$ul(
              tags$li(sprintf("Path: %s -> %s", cycle_str, cycles[[i]][1])),
              tags$li(sprintf("Type: %s", analysis$type)),
              tags$li(sprintf("Negative edges: %d of %d", 
                              analysis$negative_edges, 
                              analysis$total_edges))
            )
          )
        })
      )
    )
  })
  
  # Initialize the graph on startup
  observe({
    if (is.null(graph_data())) {
      shinyjs::click("generate")
    }
  })
}

shinyApp(ui, server)
