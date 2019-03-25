library(shiny)
library(shinythemes)

# Define UI for app
ui <- fluidPage( theme = shinytheme("sandstone"),
  
   # Application title
   div(style="margin-bottom:10px;",titlePanel("Sponsor Network Explorer")),
   hr(),
   HTML("Select network type and product or condition name to plot.  <b>Note:</b> Some larger data sets may take time to render."),
   hr(),
   fluidRow(
     div(style="display: inline-block;vertical-align:top; width: 220px; margin-left: 10px;", 
         selectInput("networktype", label = "Network Type:", choices = list("Drug Product Network"=1, "Disease/ Condition Network"=2))
        ),
     div(style="display: inline-block;vertical-align:top; width: 220px; margin-left: 10px;",uiOutput("idquery")),
     div(style="display: inline-block;vertical-align:middle; margin-left: 20px; margin-top: 25px;",actionButton("go", "Plot Network")),
     div(style="clear:both; margin-left: 250px; margin-top: -10px;",HTML("<em>type to search or use dropdown</em><br />")),
     hr()
   ),
   
   sidebarLayout(
      sidebarPanel(width = 4,
        selectInput("layout", label = "Select Layout:", choices = list("Smart Layout"=1, "Fruchterman-Reingold"=2, "Kamada-Kawai"=3,"Ciricle"=4, "Large Graph Layout"=5)),
        div(style="margin-top: -10px;",HTML("<a href='http://conflictmetrics.com/layouts' target='_blank'>Learn more about layout types</a>")),
        uiOutput("nodeslide"),
        checkboxInput("labels","Show Labels",value = FALSE)
        ),
      mainPanel(
        div(id="plot", style="margin-bottom:100px;",plotOutput("network")),
        tags$style(type="text/css",
            ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error:before { visibility: visible; content: 'Loading...'; }"
            )
      )
   ),
   
  fluidRow(
    tabsetPanel(
      tabPanel("Trial Sponsors", dataTableOutput("topsponsors")),
      tabPanel("Registered Trials", dataTableOutput("trials"))
    )
  )
)


# Define server
library(stringr)
library(igraph)
library(data.table)
name_index <- fread("http://conflictmetrics.com/data/name_lookup.csv")
disease_lookup <-fread("http://conflictmetrics.com/data/disease_lookup.csv")

server <- function(input, output, session) {
  output$idquery <- renderUI({
    selectizeInput("pname", label="Name Search:", choices = NULL)
  })
  
  nettypeInput <- reactive({
    if(input$networktype == "1"){name_index$name}else{disease_lookup$name}
  })
  
  observe({
    updateSelectizeInput(session, 'pname', label="Name Search:", choices = nettypeInput(), server = TRUE)
  })

  pidInput <- eventReactive(input$go, {
    if(input$networktype == "1"){
           proid <- name_index$pid[name_index$name == input$pname]}else{
           proid <- disease_lookup$cid[disease_lookup$name == input$pname]}
  })
  
  maxInput <- eventReactive(input$go, {
    if(input$networktype == "1"){
        max <- name_index$max[name_index$name == input$pname]}else{
        max <- disease_lookup$max[disease_lookup$name == input$pname]}
  })
  
  edgesInput <- reactive({
    if(input$networktype == "1"){
      edges <- fread(paste0("http://conflictmetrics.com/data/edges/edges",pidInput(),".csv"))}else{
      edges <- fread(paste0("http://conflictmetrics.com/data/dedges/edges_",pidInput(),".csv"))  
      }
    freq <- as.data.frame(table(edges$from))
    colnames(freq) <- c("id","freq")
    freq <- subset(freq, freq >= input$nodedeg)
    edges <- edges[edges$from %in% freq$id, ]
  })
  
  nodesInput <- reactive({
    edges <- edgesInput()
    if(input$networktype == "1"){
        rpnodes <- fread(paste0("http://conflictmetrics.com/data/nodes/nodes_rct_",pidInput(),".csv"))
        spnodes <- fread(paste0("http://conflictmetrics.com/data/nodes/nodes_spo_",pidInput(),".csv"))
        }else{
        rpnodes <- fread(paste0("http://conflictmetrics.com/data/dnodes/nodes_rct_",pidInput(),".csv"))
        spnodes <- fread(paste0("http://conflictmetrics.com/data/dnodes/nodes_spo_",pidInput(),".csv"))
        }
    rpnodes <- rpnodes[rpnodes$id %in% edges$to, ]
    spnodes <- spnodes[spnodes$id %in% edges$from, ]
    nodes <- rbind(spnodes,rpnodes)
  })
    
  netInput <- reactive ({
    nodes <- nodesInput()
    edges <- edgesInput()
    net <- graph_from_data_frame(d= edges, vertices = nodes, directed = T)
  })
  
  output$network <- renderPlot({
    nodes <- nodesInput()
    edges <- edgesInput()
    net <- netInput()
    nodes$deg <- degree(net, v =V(net), mode = c("out"),normalized = TRUE)
    #nodes$tdeg  <- degree(net, v =V(net), mode = c("out"),normalized = FALSE)
    nodes$label <- strtrim(nodes$label,30)
    edge.start <- ends(net, es=E(net), names=F)[,1]
    edge.col <- nodes$color[edge.start]
    par(oma=c(0,0,0.0,0))
    par(mar=c(0,0,0,0))
    plot(net, 
         layout = 
           ifelse(input$layout == "1", layout_nicely,ifelse(input$layout == "2", layout_with_fr, ifelse(input$layout == "3", layout_with_kk, ifelse(input$layout == "4", layout_in_circle, layout_with_lgl)))), 
         edge.arrow.size=0, 
         vertex.size= ifelse(nodes$type == "RCT", 2, nodes$deg*100), 
         edge.curved=T, 
         edge.color=edge.col, 
         edge.width=2, 
         vertex.color=nodes$color, 
         vertex.frame.color=nodes$color, 
         vertex.label.color="black", 
         vertex.label.cex= 1.5, 
         vertex.label= ifelse(input$labels == TRUE & nodes$type != "RCT", nodes$label, NA)
         )
   colorcode <- c("black","firebrick1","gold","cornflowerblue")
   legend(x="topleft", c("RCTs","Industry", "US Fed", "Other"), pch=21, col="#777777", pt.bg=colorcode, pt.cex=2, cex=.8, bty="n", ncol=1, horiz = TRUE)
      }, height = 500, width = 600)
 
 
  output$nodeslide <- renderUI({
    sliderInput("nodedeg", "Filter Nodes by Out-Degree", min=0, max=maxInput(), value=.1*maxInput()[1], round=TRUE)
  })
  

  output$topsponsors <- renderDataTable({
    nodes<-nodesInput()
    net<- netInput()
    nodes$tdeg  <- degree(net, v =V(net), mode = c("out"),normalized = FALSE)
    nodes <- subset(nodes, nodes$type != "RCT")
    nodes$label <- str_replace_all(nodes$label,"[^a-zA-Z\\s]", "")
    nodes$color <- NULL
    nodes$type <- NULL
    nodes$id <- NULL
    colnames(nodes) <- c("Sponsors", "Trials Supported") 
    nodes
  })
  
  output$trials <- renderDataTable({
    nodes<-nodesInput()
    net<- netInput()
    nodes <- subset(nodes, nodes$type == "RCT")
    nodes$label <- str_replace_all(nodes$label,"[^a-zA-Z\\s]", "")
    nodes$color <- NULL
    nodes$type <- NULL
    colnames(nodes) <- c("Registry ID", "Study Title") 
    nodes
  },
  options=list(
    autoWidth = TRUE
    #columnDefs = list(list(width = '10px', targets=c(1)))
  )
)
}
# Run the application 
shinyApp(ui = ui, server = server)

