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
    uiOutput("loop_selector"),  # Dynamic loop selection UI
    hr(),
    h4("Leverage Points"),
    uiOutput("leverage_selector"),
    sliderInput("leverage_threshold", 
                "Highlight nodes with leverage score above:",
                min = 0, max = 1.8, value = 0.5, step = 0.1)
  ),
  
  # Main panel with network visualization and loop info
  tabsetPanel(
    tabPanel("Network",
             card(
               card_header(textOutput("network_card_header")),
               card_body(
                 visNetworkOutput("network_plot", height = "600px")
               )
             )
    ),
    tabPanel("Leverage Points",
             card(
               card_header("Leverage Point Analysis"),
               card_body(
                 uiOutput("leverage_info")
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
  network_header <- reactiveVal("Network Visualization")
  # Reactive value to store current graph
  graph_data <- reactiveVal()
  # Display leverage point analysis
  output$leverage_info <- renderUI({
    req(graph_data())
    
    data <- graph_data()
    
    if (length(data$cycles) == 0) {
      return(div(
        style = "color: #666;",
        "No loops detected in the network. Leverage point analysis requires loops."
      ))
    }
    
    # Perform leverage point analysis
    leverage_points <- analyze_leverage_points(data$nodes, data$edges, data$cycles)
    
    # Create the output
    tagList(
      h4("Network Leverage Point Analysis"),
      p("Nodes are ranked based on their involvement in feedback loops, with higher scores indicating greater potential leverage."),
      div(
        style = "margin-bottom: 20px;",
        tags$table(
          class = "table table-striped",
          tags$thead(
            tags$tr(
              tags$th("Node"),
              tags$th("Total Loops"),
              tags$th("Reinforcing"),
              tags$th("Balancing"),
              tags$th("Leverage Score")
            )
          ),
          tags$tbody(
            lapply(1:nrow(leverage_points), function(i) {
              tags$tr(
                tags$td(leverage_points$node_label[i]),
                tags$td(leverage_points$total_loops[i]),
                tags$td(leverage_points$reinforcing_loops[i]),
                tags$td(leverage_points$balancing_loops[i]),
                tags$td(sprintf("%.2f", leverage_points$leverage_score[i]))
              )
            })
          )
        )
      ),
      hr(),
      div(
        style = "margin-top: 20px;",
        h5("Interpretation Guide:"),
        tags$ul(
          tags$li("Nodes with higher leverage scores are more central to the network's feedback structure."),
          tags$li("The leverage score considers both the number and types of loops a node participates in."),
          tags$li("Reinforcing loops are weighted slightly higher (1.2x) than balancing loops in the score calculation."),
          tags$li("Nodes participating in many loops but with low scores might indicate redundant pathways.")
        )
      )
    )
  })
  
  # Add this new output renderer
  output$network_card_header <- renderText({
    network_header()
  })
  
  # Add this observer to update the header when a loop is selected
  observe({
    req(graph_data())
    
    selected <- try(as.numeric(input$selected_loop), silent = TRUE)
    if (!inherits(selected, "try-error") && !is.na(selected) && selected != 0) {
      if (selected <= length(graph_data()$cycles)) {
        # Get the loop analysis for the selected loop
        cycle <- graph_data()$cycles[[selected]]
        analysis <- analyze_loop(cycle, graph_data()$edges)
        network_header(sprintf("%s Loop %d", analysis$type, selected))
      }
    } else {
      network_header("Network Visualization")
    }
  })
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
  
  
  # Leverage point selector UI
  output$leverage_selector <- renderUI({
    req(graph_data())
    data <- graph_data()
    
    if (length(data$cycles) == 0) {
      return(NULL)
    }
    
    leverage_points <- analyze_leverage_points(data$nodes, data$edges, data$cycles)
    
    radioButtons("leverage_view", "View Mode:",
                 choices = c(
                   "Normal View" = "normal",
                   "Highlight High Leverage Nodes" = "highlight"
                 ),
                 selected = "normal")
  })
  
  # Modify the network_plot renderer to include leverage point highlighting
  output$network_plot <- renderVisNetwork({
    req(graph_data())
    
    data <- graph_data()
    
    # Create base network
    edges <- data$edges
    edges$width <- 1
    nodes <- data$nodes
    
    # Handle different view modes
    if (!is.null(input$leverage_view) && input$leverage_view == "highlight" && length(data$cycles) > 0) {
      # Leverage point highlighting mode
      leverage_points <- analyze_leverage_points(nodes, edges, data$cycles)
      threshold <- input$leverage_threshold
      
      # Get nodes above threshold
      nodes_above_threshold <- leverage_points$node_id[leverage_points$leverage_score >= threshold]
      
      # Update colors
      nodes <- get_leverage_node_colors(nodes, leverage_points, threshold)
      edges <- get_leverage_edge_colors(edges, nodes_above_threshold)
      
    } else {
      # Normal mode (existing loop highlighting or default view)
      colored_data <- set_default_colors(nodes, edges)
      nodes <- colored_data$nodes
      edges <- colored_data$edges
      
      if (length(data$cycles) > 0) {
        selected <- try(as.numeric(input$selected_loop), silent = TRUE)
        if (!inherits(selected, "try-error") && !is.na(selected) && selected != 0) {
          if (selected <= length(data$cycles)) {
            cycle <- data$cycles[[selected]]
            cycle_edges <- create_cycle_edge_matrix(cycle)
            edges <- get_edge_colors(edges, cycle_edges)
            nodes <- get_node_colors(nodes, cycle)
          }
        }
      }
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
        smooth = list(
          type = "curvedCW",
          roundness = 0.2
        )
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
