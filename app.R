#Lvl 2 final.

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
                       selectInput("granularitylvl", label = "Granularity Level:", choices = list("Article Level"=1, "Journal Level"=2, "Author Level"=3))
                   )
                   ,div(style="display: inline-block;vertical-align:top; width: 250px; margin-left: 10px;",uiOutput("idquery"))
                   ,div(style="display: inline-block;vertical-align:middle;width: 150px;  margin-top: 25px;",actionButton("go", "Plot Network"))
                   ,div(style="clear:both; margin-left: 250px; margin-top: -10px;",HTML("<em>Type to search (ex: leukemia) </em><br />")),
                   hr()
                 ),
                 fluidRow(
                   div(style="display: inline-block;vertical-align:top;margin-left: 10px;margin-bottom:10px;"
                       , htmlOutput("text_citation")
                   )
                   , tags$head(tags$style(type="text/css", "
                                          #loadmessage {
                                          position: fixed;
                                          top: 0px;
                                          left: 0px;
                                          width: 100%;
                                          padding: 5px 0px 5px 0px;
                                          text-align: center;
                                          font-weight: bold;
                                          font-size: 100%;
                                          color: #000000;
                                          background-color: #335EFF;
                                          z-index: 105;
                                          }
                                          "))                        
                   ,conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                     tags$div("Loading...",id="loadmessage"))
                   ),
                 sidebarLayout(
                   sidebarPanel(width = 3,
                                selectInput("layout", label = "Select Layout:", choices = list("Smart Layout"=1, "Fruchterman-Reingold"=2, "Kamada-Kawai"=3,"Ciricle"=4, "Large Graph Layout"=5))
                                ,div(style="margin-top: -10px;",HTML("<a href='http://conflictmetrics.com/layouts' target='_blank'>Learn more about layout types</a>"))
                                ,uiOutput("nodeslide")
                   ),
                   mainPanel(
                     div(id="plot", style="height:400px;"
                         , visNetworkOutput("network_proxy")
                         #, verbatimTextOutput("outval")
                     )                     
                   )
                 ),
                 fluidRow(
                   dataTableOutput("topsponsors")
                 )
                 )

server <- function(input, output, session) {
  
  #Set directory in case dataset does not run correctly: setwd("/Users/pedroseguel/RProjects/PharmModel")
  # connect to the sqlite file
  sqlite    <- dbDriver("SQLite")
  connection <- dbConnect(sqlite,"t2v_test.sqlite")
  
  output$idquery <- renderUI({
    textInput("pname", label="Name Search:", value = "")
  })
  
  observeEvent(input$go, {
    updateTextInput(session, 'pname', label="Name Search:", value = input$pname)
    
  })
  
  pidInput <- eventReactive(input$go, {
    temp <- esearch(input$pname,db="pubmed",retmode = "json", retstart = 0, retmax = "100000", datetype = "pdat",mindate = "2015",maxdate = "2018")
    #temp <- esearch(input$pname,db="pubmed",retmode = "json", retstart = 0, retmax = "100000")
  })

  countvals <- reactiveValues(coi= 0, nrow=0, ji= 0, au = 0)
  vals <- reactiveValues(pmidcount = 0) #set vals$pmidcount
  vals <- reactiveValues(nfv = 0)  #set vals$nfv
  vals <- reactiveValues(dbcount = 0)  #set vals$dbcount
  
  query <- reactive( {
    #Get pmid from  API
    pmidlist <- list()
    #Alirocumab leukemia Lukemia 
    temp <- pidInput()
    tempdata <- fromJSON(content(temp))
    pmidlist <- append(pmidlist,tempdata$esearchresult$idlist)
    
    vals$pmidcount = length(pmidlist)
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
    
    query = paste('select co_name  as "source"
                  , author_name  as "target_author" 
                  , edges_master.pmid as "target_article" 
                  , jid as target_journal
                  , weight
                  from edges_master where pmid IN (',query_pmid_list_where,')'
                  )

  })  
  output$nodeslide <- renderUI({
    sliderInput("nodedeg", "Filter Nodes by Out-Degree", min=0, max=vals$dbcount, value=.1*vals$dbcount, round=TRUE)
  })
  
  output$network_proxy <- renderVisNetwork({
    #cat(file=stderr(), query(), "\n")
    # Execute Query
    edges_df_db <- dbGetQuery(connection,query())

    countvals$nrow = nrow(edges_df_db)
    countvals$coi = length(unique(edges_df_db$target_article))
    countvals$ji = length(unique(edges_df_db$target_journal))
    countvals$au = length(unique(edges_df_db$target_author))                               
    #aa= edges_df[edges_df$industry_type != 'pharmaceuticals', ]
    #summary(edges_df)
    #edges_df$to
    #edges_df[grepl( "Novartis", edges_df$source),]
    #edges_df[edges_df$source == "Novartis",]
    

    #Check data - this's not working
    #validate(
    #    need( nrow (nodes_from)>0, "No data has returned")
    #  ) 
    nodes_from <- as.data.frame(table(Title =edges_df_db$source ))
    if(nrow(nodes_from) == 0)
    {
      output$text_citation <- renderText({ 
        HTML(paste("No data has returned" ))
      })
      return()
    }else
    {
      #PubMed Citations Matching Search for _____: N; Citations with COI Data: N; Total Conflicts N; Journals Involved: N; Authors Involved: N
      output$text_citation <- renderText({ 
        HTML(paste("PubMed Citations Matching Search for <b>", input$pname, "</b>: ", vals$pmidcount
                   , "; &nbsp;Citations with COI Data: ",  countvals$coi 
                   , "; &nbsp;Total Conflicts: ",   countvals$nrow
                   , "; &nbsp;Journals Involved: ",   countvals$ji 
                   , "; &nbsp;Authors Involved: ",   countvals$au                   
                   )
            )
      })
      
    }

    edges_df <- edges_df_db
    groupName=""
    if(input$granularitylvl == "1" ){
      groupName="Article"
      edges_df <- edges_df %>% 
        mutate(target = target_article) 
    }
    else if(input$granularitylvl == "2" ){
      groupName="Journals"
      edges_df <- edges_df %>% 
        mutate(target = target_journal) 
    }
    else{
      groupName="Authors"
      edges_df <- edges_df %>% 
        mutate(target = target_author) 
    }
    edges_df <- edges_df %>% 
      group_by(source, target) %>% 
      summarise(value = sum(as.numeric(weight), na.rm=TRUE))
    
    #Note_to
    nodes_to <- as.data.frame(table(Title =edges_df$target))
    
    vals$dbcount = max(nodes_from$Freq)
    message(paste("dbcount: ", vals$dbcount) )
    
    #nodes_from <- as.data.frame(table(name=edges$from, nName=edges$co_name, Group=edges$coi_type ))
    #nodes_to <- as.data.frame(table(name=edges$to, nName=edges$author_name))
    #nodes_to["Group"]="Authors"
    nodes_to$Freq = 1 # fix author&article's node size
    

    nodes_to <- nodes_to %>% 
      mutate(group = groupName) %>% 
      mutate(value = Freq)   %>% 
      mutate(label = Title)   
    
    #nfv match count in db
    vals$nfv <- sum(nodes_to$Freq)
    
    nodes_from <- nodes_from %>% 
      mutate(group = "Industry")  %>% 
      mutate(value = Freq)    %>% 
      mutate(label = Title)   
    
    #filter nodes by out-degree
    nodes_from <- nodes_from[nodes_from$Freq > input$nodedeg,]
    
    nodes_df <- rbind(nodes_from , nodes_to )
    #drop rows that freq is 0
    nodes_df <- nodes_df[nodes_df$Freq != 0,]
    
    #INCOMPLETE, frequency of out-degree as an input value
    #freq <- subset(freq, freq >= input$nodedeg)
    

    #nodes_df[grepl( "Novartis", nodes_df$title),]
    #query_pmid_list_where
    #idx create
    nodes <- nodes_df %>% 
      mutate(id = row_number()) 
    #edges_df[grepl( "Merck", edges_df$source),]
    
    #create from, to in edges
    edges <- edges_df %>% 
      left_join(select(nodes, id, Title) %>% rename(from = id), by=c('source' = 'Title')) %>% 
      left_join(select(nodes, id, Title) %>% rename(to = id), by=c('target' = 'Title'))


    #fliter edges table by out-degree 
    #edges <- subset(edges,edges$to %in% nodes$id | edges$from %in% nodes$id)
    edges <- edges[edges$from %in% nodes$id, ]
    nodes <- nodes[(nodes$id %in% edges$to | nodes$id %in% edges$from), ]


    #Table display
    output$topsponsors <- renderDataTable({
      topsponsors<-subset(nodes, select=c("Title", "group", "Freq"))
      topsponsors
    })

    
    #Interactive Network display, see the documentation here: https://datastorm-open.github.io/visNetwork/
    visNetwork(nodes, edges, height = "400px", width = "100%") %>%  #Control size of the network display
      visLegend(width = 0.1, position = "left", main = "")  %>%   #Display or mute Legend based on groups, using visLegend. Use options to add a title, adjust size or choose the position from the labels
      visGroups(groupname = groupName, color = "lightblue") %>% #Customize name and color selection based on level selection
      visGroups(groupname = "Industry" , color = "red") %>% #Customize name and color selection for on industry
      addFontAwesome() %>%
      visPhysics(stabilization = FALSE) %>% #Using visPhysics() function, you can play with the physics of the network
      #Use igraph layout: With visIgraphLayout(), you can use all available layouts in igraph 
      visIgraphLayout(layout = ifelse(input$layout == "1", "layout_nicely",ifelse(input$layout == "2", "layout_with_fr", ifelse(input$layout == "3", "layout_with_kk", ifelse(input$layout == "4", "layout_in_circle", "layout_with_lgl")))) )  %>%
      visEdges(smooth = FALSE) %>%
      visNodes(scaling = list(min = 20, max = 100), color = list(background = "lightblue"
                                                                 , hover = list(background = "green", border = "red")
      )
      ,shadow = list(enabled = TRUE, size = 10)
      ) %>%
      #Custom options are available using visOptions(), such as Highlight nearest, Select by node id, Select by a column, for more info: https://datastorm-open.github.io/visNetwork/options.html
      visOptions(highlightNearest = TRUE) %>% #Optional highligh option by node selection
      #visOptions(selectedBy = "Title") %>% #Optional selection by column to allow search by title or author name on the graph
      #visOptions(selectedBy = "group") %>% #Optional selection by group in the graph
      
      #Control the interactions (i.e. dragNodes, hover, zoom, keyboard functions) of the network with visInteraction():
      visInteraction(dragNodes = TRUE, #nable or not the selection and movement of nodes (click on a node, and move your mouse)
                     dragView = TRUE, #enable or not the movement of the full network (click everywhere except node, and move your mouse) 
                     zoomView = TRUE, #enable or not the zoom (use mouse scroll)
                     hover = TRUE, #hover and hoverConnectedEdges : control hover
                     #hoverConnectedEdges = TRUE,
                     #keyboard = TRUE, tooltipDelay = 0, # enable keyboard manipulation rather than mouse (click on network before)
                     navigationButtons = TRUE)  %>% #display navigation Buttons on the graph display.
      visLayout(randomSeed = 123)   %>%
      visEdges ( arrows = 'to' )
    
  })  
  #output$outval <- renderText({ query() })  
  #Slider to fifer by Out-Degree
  
  #Text display with number of citations matching search from pmidlist, and citation using the data for the graph. 
  #output$text_citation <- renderText({ 
  #  HTML(paste("PubMed citations matching search for <b>", input$pname, "</b>: ", vals$pmidcount, "; &nbsp;   &nbsp;  Citations with COI data: ",   vals$nfv ))
  #})
  
  
  }


shinyApp(ui = ui, server = server)