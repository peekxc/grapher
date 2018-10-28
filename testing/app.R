library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Testing grapher shiny updates"),
    sidebarLayout(
        sidebarPanel(
            checkboxInput(inputId = "do_thing", label = "do thing", value = FALSE),
            checkboxInput(inputId = "do_other_thing", label = "do other thing", value = FALSE),
            checkboxInput(inputId = "change_color", label = "Change color", value = FALSE), 
            checkboxInput(inputId = "remove_vertices", label = "Remove vertices", value = FALSE)
        ),
        mainPanel(
           grapherOutput("grapher")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  set.seed(1234)
  n_nodes <- 5L
  G <- igraph::sample_grg(n_nodes, radius = 0.4)
  net_config <-  grapher::getDefaultJsonConfig(G)
  
  ## Render the grapher 
  output$grapher <- renderGrapher({ grapher(net_config) %>% addForce() })
  
  ## Observers
  observeEvent(input$do_thing, {
    # new_config <- net_config
    # new_config$nodes <- new_config$nodes[1,]
    # new_config$nodes$color <- rgb(0, 0, 1)
    # new_config$links <- data.frame(from = 1, to = 2, color = rgb(0, 0, 1))
    # updateNodes(id = "grapher", node_ids = c(1L), net_config = new_config)
    
    node_config <- data.frame(id=10,x=0.5,y=0.5,r=5,color=rgb(0, 0, 0))
    link_config <- data.frame(id=max(net_config$links$id)+1,from=0,to=1,color=rgb(0, 0, 0))
    # # addNodes(id = "grapher", node_config = node_config)
    addConfiguration(id = "grapher", net = list(nodes=node_config, links=link_config))
    # grapher::toggleLasso(id = "grapher")
  })
  
  observeEvent(input$do_other_thing, {
    grapher:::logNetwork(id = "grapher")
    # grapher::addLasso(id = "grapher")
  })
  observeEvent(input$change_color, {
    updateNodeColor(id = "grapher", color = c(rep(rgb(1, 0, 0, 0.5), n_nodes/2), rep(rgb(0, 1, 0, 0.5), n_nodes/2))) 
  })
  observeEvent(input$remove_vertices, {
    # removeNodes(id = "grapher", node_ids = c(1, 2)) 
  })
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
