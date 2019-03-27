require(shiny)
library(shinythemes)

require(visNetwork)
library(RSQLite)
library(dplyr)
library(jsonlite)
library(reutils)

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
       div(style="clear:both; margin-left: 250px; margin-top: -10px;",HTML("<em>Type to search (ex: leukemia) </em><br />")),
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
         div(id="plot", style="margin-bottom:100px;"
              , visNetworkOutput("network_proxy")
             #, verbatimTextOutput("outval")

             )
        , tags$style(type="text/css",
                    ".shiny-output-error { visibility: hidden; }",
                    ".shiny-output-error:before { visibility: visible; content: 'Loading...'; }"
         )
       )
    )
                 
)

server <- function(input, output, session) {

  #setwd("/Users/pedroseguel/RProjects/PharmModel")
  # connect to the sqlite file
  sqlite    <- dbDriver("SQLite")
  connection <- dbConnect(sqlite,"t2v_test.sqlite")

  output$idquery <- renderUI({
    textInput("pname", label="Name Search:", value = "")
  })

  observeEvent(input$go, {
    message("go button was pressed")
    
    updateTextInput(session, 'pname', label="Name Search:", value = input$pname)
  })
  
  
  
  pidInput <- eventReactive(input$go, {
    temp <- esearch(input$pname,db="pubmed",retmode = "json", retstart = 0, retmax = "100000", datetype = "pdat",mindate = "2015",maxdate = "2018")
  })
  
  
  query <- reactive( {
    #Get pmid from  API
    pmidlist <- list()
    #Alirocumab leukemia Lukemia 
    temp <- pidInput()
    tempdata <- fromJSON(content(temp))
    pmidlist <- append(pmidlist,tempdata$esearchresult$idlist)
    #tempdata$esearchresult$idlist
    
    
    # Index was created prior to query
    #dbExecute(exampledb, "CREATE INDEX tag_pmid ON edges_master (pmid)")
    #dbExecute(exampledb, "CREATE INDEX tag_cid ON edges_master (cid)")
    #dbExecute(exampledb, "CREATE INDEX tag_aid ON edges_master (aid)")
    #dbListFields(connection, "edges_master")
    #dbGetQuery(connection,"select * from edges_master where pmid >0 and cid>0 and aid>0 LIMIT 50")
    #dbGetQuery(connection,"select * from edges_master where industry_type <> 'pharmaceuticals'")
    
    # Build query with pmid list 
    query_pmid_list <- paste(pmidlist,collapse = "','") 
    query_pmid_list_where <- paste("'",query_pmid_list,"'" )
    #cat(file=stderr(), query_pmid_list, "\n")
    query <- paste('select pmid as "target" 
                  , co_name  as "source"
                  , sum(weight) as value  
                  , max(industry_type), coi_type
                  from edges_master where pmid IN (',query_pmid_list_where,')'
                  , '   group by co_name, pmid'
                  )
  
    })  

  #output$outval <- renderText({ query() })  

  
  
  output$network_proxy  <- renderVisNetwork({
    
    #cat(file=stderr(), query(), "\n")
    message(query())
    # Execute Query
    edges_df <- dbGetQuery(connection,query())
    #aa= edges_df[edges_df$industry_type != 'pharmaceuticals', ]
    #summary(edges_df)
    #edges_df$to
    #head(edges_df)
    
    nodes_from <- as.data.frame(table(title =edges_df$source ))
    nodes_to <- as.data.frame(table(title =edges_df$target))
    #nodes_from <- as.data.frame(table(name=edges$from, nName=edges$co_name, Group=edges$coi_type ))
    #nodes_to <- as.data.frame(table(name=edges$to, nName=edges$author_name))
    #nodes_to["Group"]="Authors"
    nodes_to <- nodes_to %>% 
      mutate(group = "Article")  
    
    nodes_from <- nodes_from %>% 
      mutate(group = "Industry") 
    
    nodes_df <- rbind(nodes_from , nodes_to )
    #drop rows that freq is 0
    nodes_df <- nodes_df[nodes_df$Freq != 0,]
    
    #idx create
    nodes <- nodes_df %>% 
      mutate(id = row_number()) 
    
    #create from, to in edges
    edges <- edges_df %>% 
      left_join(nodes %>% rename(from = id), by=c('source' = 'title')) %>% 
      left_join(nodes %>% rename(to = id), by=c('target' = 'title'))
    
        visNetwork(nodes, edges, height = "600px", width = "100%") %>%
          visLegend()  %>%
          visGroups(groupname = "Authors", color = "lightblue") %>%
          visGroups(groupname = "Industry" , color = "red") %>%
          #visOptions(highlightNearest = list(enabled =TRUE, degree = 2, hover = T)) %>%
          addFontAwesome() %>%
          visPhysics(stabilization = FALSE) %>%
          visIgraphLayout(layout = ifelse(input$layout == "1", "layout_nicely",ifelse(input$layout == "2", "layout_with_fr", ifelse(input$layout == "3", "layout_with_kk", ifelse(input$layout == "4", "layout_in_circle", "layout_with_lgl")))) )  %>%
          visEdges(smooth = FALSE) %>%
          visNodes(color = list(background = "lightblue"
                                , hover = list(background = "green", border = "red")
          )
          ,shadow = list(enabled = TRUE, size = 10)
          ) %>%
          #  visInteraction(hover = TRUE)  %>%
          visLayout(randomSeed = 123)   %>%
          visEdges ( arrows = 'to' )
  })  
  
}


shinyApp(ui = ui, server = server)
