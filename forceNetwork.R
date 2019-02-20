library(networkD3)
library(RSQLite)
library(reutils)
library(jsonlite)
library(dplyr)
library(data.table)
library(stringr)
library(igraph)


setwd("D:/RWork/PubmedNetwork")
# connect to the sqlite file
sqlite    <- dbDriver("SQLite")
connection <- dbConnect(sqlite,"t2v_test.sqlite")

#Get pmid from  API
pmidlist = list()
#Alirocumab leukemia Lukemia 
temp <- esearch("Alirocumab",db="pubmed",retmode = "json", retstart = 0, retmax = "100000", datetype = "pdat",mindate = "2015",maxdate = "2018")
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
              , author_name  as "to" 
              , co_name  as "from"
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

nodes_from <- as.data.frame(table(name=edges_df$from ))
nodes_to <- as.data.frame(table(name=edges_df$to))
#nodes_from <- as.data.frame(table(name=edges$from, nName=edges$co_name, Group=edges$coi_type ))
#nodes_to <- as.data.frame(table(name=edges$to, nName=edges$author_name))
#nodes_to["Group"]="Authors"
nodes_df <- rbind(nodes_from , nodes_to )
#drop rows that freq is 0
nodes_df = nodes_df[nodes_df$Freq != 0,]

#idx create
nodes = nodes_df %>% 
  mutate(idx = row_number()-1) 

#create source_idx, target_idx in edges
edges = edges_df %>% 
  left_join(nodes %>% rename(source_idx = idx), by=c('from' = 'name')) %>% 
  left_join(nodes %>% rename(target_idx = idx), by=c('to' = 'name'))
head(edges)

networkD3::forceNetwork(Links = edges, Nodes = nodes
                             , Source = "source_idx", Target = "target_idx"
             , Value = "Weight"  # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
             , NodeID = "name" # value from the node list (data frame) that contains node description we want to use (e.g., node name)
             , Nodesize   = 'Freq' 
             , Group = 'name'
             , opacity = 1
             , fontSize = 20
             , zoom = TRUE
             )








#Not using now. save for later
forceNetwork(Links = MisLinks, Nodes = MisNodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",
             Group = "group"
             ,fontSize = 20)
Nodesize = "nodeBetweenness",  # value from the node list (data frame) that contains value we want to use for a node size
Group = "nodeDegree",  # value from the node list (data frame) that contains value we want to use for node color
#height = 500, # Size of the plot (vertical)
#width = 1000,  # Size of the plot (horizontal)
fontSize = 20, # Font size
linkDistance = networkD3::JS("function(d) { return 10*d.value; }"), # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
linkWidth = networkD3::JS("function(d) { return d.value/5; }"),# Function to determine link/edge thickness, uses variables already defined in forceNetwork function (not variables from a data frame)
opacity = 0.85, # opacity
zoom = TRUE, # ability to zoom when click on the node
opacityNoHover = 0.1, # opacity of labels when static
linkColour = edges_col) # edge colors



# forceNetwork(Links = edges, Nodes = nodes_new, Source = "from",
#              Target = "to", 
#               NodeID = "id",
#              Group = 'Freq'
#              , opacity = 0.7,
#              colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"))
#net <- graph_from_data_frame(d= edges, vertices = nodes, directed = T)
net <- graph.data.frame(edges, directed = F) 
#nodes$Nodesize <- degree(net, v =V(net), mode = c("out"),normalized = TRUE)
degree(net)

nodes
degree(net) 

set.seed(1)
degree(net)
plot(net, vertex.size=degree(net)*0.1, vertex.label.dist=0.3
     , edge.arrow.size=0
     , edge.curved=T 
     , edge.width=2 
     , vertex.label= NA
)
forceNetwork(reponet, Nodes = nodesdf, Source = "from", Target = "to", 
             NodeID = "name", Group = "group", Nodesize = "nodesize")
#unique(edges$to)

# Check Nodes and edges
#c(edges[,2], edges[,3]) %in% nodes$name
#Network Graph
#table(name=edges$from )
#table(name=edges$to )
