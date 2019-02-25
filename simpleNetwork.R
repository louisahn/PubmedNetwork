library(networkD3)
library(RSQLite)
library(reutils)
library(jsonlite)
library(dplyr)
library(data.table)

setwd("D:/RWork/PubmedNetwork")


#Get pmid from  API
pmidlist = list()
temp <- esearch("leukemia",db="pubmed",retmode = "json", retstart = 0, retmax = "100000", datetype = "pdat",mindate = "2010",maxdate = "2017")
tempdata <- fromJSON(content(temp))
pmidlist <- append(pmidlist,tempdata$esearchresult$idlist)

# connect to the sqlite file
sqlite    <- dbDriver("SQLite")
exampledb <- dbConnect(sqlite,"t2v_test.sqlite")
edges_master = dbGetQuery(exampledb, 'select pmid, cid as "from", aid as "to" from edges_master')

#filter edges with pmid
edges <- edges_master[edges_master$pmid %in% tempdata$esearchresult$idlist,]

#Network Graph
src <- edges$from
target <- edges$to
networkData <- data.frame(src, target)
simpleNetwork(networkData)


dbListTables(exampledb) 
pmiddf = data.frame(pmidlist, c='pmid')
dbWriteTable(exampledb, "pmidtable", pmidlist) 

edges_master = dbGetQuery(exampledb, 'select pmid, cid as "from", aid as "to" from edges_master where pmid in :a'
                          , list(a = pmidlist)   )

paste(pmidlist,collapse = ",")

a = dbGetQuery(exampledb,'select pmid, cid as "from", aid as "to" from edges_master where pmid IN (',
        paste(pmidlist,collapse = ','  ), ')'  )


dbExecute(exampledb, "CREATE INDEX tag_pmid ON edges_master (pmid)")

a = paste(pmidlist,collapse = "','") 
b = paste("'", a , "'" )
query = paste('select pmid, cid as "from", aid as "to" from edges_master where pmid IN (',b,')')
a = dbGetQuery(exampledb,query)




#Not using now. save for later
# nodes2 <- as.data.frame(table(edges$from))
# nodes3 <- as.data.frame(table(edges$to))
# 
# v <- as.data.frame (unique(c(edges[,2], edges[,3])) )
# colnames(v) <- c("id")
# 
# colnames(nodes2) <- c("id", 'Freq')
# colnames(nodes3) <- c("id", 'Freq')
# 
# rpnodes <- edges[edges$from %in% nodes2$id, ]
# spnodes <- edges[edges$to %in% nodes3$id, ]
# 
# nodes_new <- rbind(nodes2 , nodes3 )
# 
# forceNetwork(Links = edges, Nodes = nodes_new, Source = "from",
#              Target = "to", 
#               NodeID = "id",
#              Group = 'Freq'
#              , opacity = 0.7,
#              colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"))
# 
# 
# nodes_new[!duplicated(nodes_new), ]
# 
# c(e[,1], e[,2]) %in% nodes_new
# 
# 
# nodes_new
# 
# net <- graph_from_data_frame(d= edges, vertices = v, directed = T)
# 
# c(edges[,3], edges[,2]) %in% v$id
# 
# write.csv(edges, "edges.csv")
# write.csv(v, "vertices.csv")
