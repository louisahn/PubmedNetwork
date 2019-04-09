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
                 HTML("Select a level of granularity and a condition name to plot.  <b>Note:</b> Some larger data sets may take time to render."),
                 hr(),
                 fluidRow(
                   div(style="display: inline-block;vertical-align:top; width: 220px; margin-left: 10px;", 
                       selectInput("granularitylvl", label = "Granularity Level:", choices = list("Article Level"=1, "Journal Level"=2))
                   ),
                   div(style="display: inline-block;vertical-align:top; width: 220px; margin-left: 10px;",uiOutput("idquery")),
                   div(style="display: inline-block;vertical-align:middle;  margin-top: 25px;",actionButton("go", "Plot Network")),
                   div(style="display: inline-block;vertical-align:top; width: 220px; margin-left: 10px;", 
                       selectInput("layout", label = "Select Layout:", choices = list("Smart Layout"=1, "Fruchterman-Reingold"=2, "Kamada-Kawai"=3,"Ciricle"=4, "Large Graph Layout"=5))
                       ,div(style="margin-top: -10px;",HTML("<a href='http://conflictmetrics.com/layouts' target='_blank'>&nbsp;Learn more about layout types</a>"))
                        
                   ),
                   div(style="clear:both; margin-left: 250px; margin-top: -10px;",HTML("<em>Type to search (ex: leukemia) </em><br />")),

                 
                  hr()
                 ),
                 
                 fluidRow(
                    div(style="display: inline-block;vertical-align:top;margin-left: 10px;"
                        , textOutput("text_citation")
                      ),
                     div(id="plot", style="height:400px;"
                         , visNetworkOutput("network")
                         #, verbatimTextOutput("outval")
                         
                         #, tags$style(type="text/css",
                         #             ".shiny-output-error { visibility: hidden; }",
                         #              ".shiny-output-error:before { visibility: visible; content: 'Loading...'; }")                        
                     )
                    ),
                 fluidRow(
                          dataTableOutput("topsponsors")
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

  counter = 0
  query <- reactive( {
    #Get pmid from  API
    pmidlist <- list()
    #Alirocumab leukemia Lukemia 
    temp <- pidInput()
    tempdata <- fromJSON(content(temp))
    pmidlist <- append(pmidlist,tempdata$esearchresult$idlist)
    
    counter = length(pmidlist)
    message(counter)
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
    query_pmid_list_where <- paste0("'",query_pmid_list,"'" )
    #cat(file=stderr(), query_pmid_list, "\n")
    query = paste('select edges_master.pmid as "target" 
              , co_name  as "source"
                  , sum(weight) as value  
                  , max(article_meta_data.title) as article_title
                  from edges_master 
                  join article_meta_data
                  on edges_master.pmid = article_meta_data.pmid
                  where edges_master.pmid IN (',query_pmid_list_where,')'
                  , 'group by edges_master.pmid, co_name'
    )
    
  })  
  
  #output$outval <- renderText({ query() })  
  
    output$network <- renderVisNetwork({
    
    #cat(file=stderr(), query(), "\n")
    # Execute Query
    edges_df <- dbGetQuery(connection,query())
    #aa= edges_df[edges_df$industry_type != 'pharmaceuticals', ]
    #summary(edges_df)
    #edges_df$to
    #edges_df[grepl( "Novartis", edges_df$source),]
    #edges_df[edges_df$source == "Novartis",]
    
    nodes_from <- as.data.frame(table(title =edges_df$source ))
    nodes_to <- as.data.frame(table(title =edges_df$target))
    #nodes_from <- as.data.frame(table(name=edges$from, nName=edges$co_name, Group=edges$coi_type ))
    #nodes_to <- as.data.frame(table(name=edges$to, nName=edges$author_name))
    #nodes_to["Group"]="Authors"
    nodes_to$Freq = 1
    nodes_to <- nodes_to %>% 
      mutate(group = "Article") %>% 
      mutate(value = Freq)   
    
    nodes_from <- nodes_from %>% 
      mutate(group = "Industry")  %>% 
      mutate(value = Freq*300)  
    
    nodes_df <- rbind(nodes_from , nodes_to )
    #drop rows that freq is 0
    nodes_df <- nodes_df[nodes_df$Freq != 0,]
    
    #INCOMPLETE, frequency of out-degree as an input value
    #freq <- subset(freq, freq >= input$nodedeg)
    
    #nodes_df[grepl( "Novartis", nodes_df$title),]
    query_pmid_list_where
    #idx create
    nodes <- nodes_df %>% 
      mutate(id = row_number()) 
    #edges_df[grepl( "Merck", edges_df$source),]
        
    #create from, to in edges
    edges <- edges_df %>% 
      left_join(select(nodes, id, title) %>% rename(from = id), by=c('source' = 'title')) %>% 
      left_join(select(nodes, id, title) %>% rename(to = id), by=c('target' = 'title'))

    output$network <- renderVisNetwork(
      {
        visNetwork(nodes, edges, height = "400px", width = "100%") %>%
          #visLegend()  %>%
          visGroups(groupname = "Authors", color = "lightblue") %>%
          visGroups(groupname = "Industry" , color = "red") %>%
          addFontAwesome() %>%
          visPhysics(stabilization = FALSE) %>%
          visIgraphLayout(layout = ifelse(input$layout == "1", "layout_nicely",ifelse(input$layout == "2", "layout_with_fr", ifelse(input$layout == "3", "layout_with_kk", ifelse(input$layout == "4", "layout_in_circle", "layout_with_lgl")))) )  %>%
          visEdges(smooth = FALSE) %>%
          visNodes(color = list(background = "lightblue"
                                , hover = list(background = "green", border = "red")
          )
          ,shadow = list(enabled = TRUE, size = 10)
          ) %>%
          visOptions(highlightNearest = TRUE) %>% #Optional highligh option
          #visOptions(selectedBy = "group") %>% #Optional selection by group in the graph
          visInteraction(hover = TRUE)  %>%
          visLayout(randomSeed = 123)   %>%
          visEdges ( arrows = 'to' )
      })
    #Slider to fifer by Out-Degree
    output$nodeslide <- renderUI({
      sliderInput("nodedeg", "Filter Nodes by Out-Degree", min=0, max=maxInput(), value=.1*maxInput()[1], round=TRUE)
    })
    
    output$topsponsors <- renderDataTable({
      topsponsors<-subset(nodes, select=c("title", "group", "Freq"))
      topsponsors
    })
    
    #Text display with citations len(pmidlist)  vs len(unique(edges:df$pmid)) , input$var2
      output$text_citation <- renderText({ 
        paste("  PubMed Citations Matching Search for ", input$pname, ": ", length(edges_df$target), " Citations with COI Data: ",   length(nodes_from$title) )
      })

  })  
  
}


shinyApp(ui = ui, server = server)
