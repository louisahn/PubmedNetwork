
install.packages("visNetwork")
library(visNetwork)


setwd("D:/RWork/PubmedNetwork")
# connect to the sqlite file
sqlite    <- dbDriver("SQLite")
connection <- dbConnect(sqlite,"t2v_test.sqlite")

#Get pmid from  API
pmidlist = list()
#Alirocumab leukemia Lukemia 
temp <- esearch("leukemia",db="pubmed",retmode = "json", retstart = 0, retmax = "100000", datetype = "pdat",mindate = "2015",maxdate = "2018")
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
query_pmid_list = paste(pmidlist,collapse = "','") 
query_pmid_list_where = paste("'", query_pmid_list , "'" )
query = paste('select pmid, cid, aid
              , author_name  as "target" 
              , co_name  as "source"
              , coi_type, co_name
              , weight as Weight
              , industry_type
              from edges_master where pmid IN (',query_pmid_list_where,')')

# Execute Query
edges_df = dbGetQuery(connection,query)
#aa= edges_df[edges_df$industry_type != 'pharmaceuticals', ]
summary(edges_df)
#edges_df$to
#head(edges_df)

nodes_from <- as.data.frame(table(title =edges_df$source ))
nodes_to <- as.data.frame(table(title =edges_df$target))
#nodes_from <- as.data.frame(table(name=edges$from, nName=edges$co_name, Group=edges$coi_type ))
#nodes_to <- as.data.frame(table(name=edges$to, nName=edges$author_name))
#nodes_to["Group"]="Authors"
nodes_to = nodes_to %>% 
  mutate(group = "Authors")  

nodes_from = nodes_from %>% 
  mutate(group = "Industry") 

nodes_df <- rbind(nodes_from , nodes_to )
#drop rows that freq is 0
nodes_df = nodes_df[nodes_df$Freq != 0,]

#idx create
nodes = nodes_df %>% 
  mutate(id = row_number()) %>% 
  mutate(value = Freq)  

#create from, to in edges
edges = edges_df %>% 
  left_join(nodes %>% rename(from = id), by=c('source' = 'title')) %>% 
  left_join(nodes %>% rename(to = id), by=c('target' = 'title'))

visNetwork(nodes, edges, height = "500px", width = "100%") %>%
  visLegend()  %>%
  visGroups(groupname = "Authors", color = "darkblue") %>%
  visGroups(groupname = "Industry" , color = "red") %>%
  #visOptions(highlightNearest = list(enabled =TRUE, degree = 2, hover = T)) %>%
  addFontAwesome() %>%
  visPhysics(stabilization = FALSE) %>%
  visIgraphLayout(layout = "layout_with_lgl") %>%
  # layout_nicely,ifelse(input$layout == "2", layout_with_fr, ifelse(input$layout == "3", layout_with_kk, ifelse(input$layout == "4", layout_in_circle, layout_with_lgl)))), 
  visEdges(smooth = FALSE) %>%
  visNodes(color = list(background = "lightblue"
                        , hover = list(background = "green", border = "red")
                        )
           ,shadow = list(enabled = TRUE, size = 10)
  ) %>%
#  visInteraction(hover = TRUE)  %>%
  visLayout(randomSeed = 123)









