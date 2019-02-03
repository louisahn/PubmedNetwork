edges_active


edges_master = dbGetQuery(exampledb, 'select pmid, cid as "from", aid as "to" from edges_master')
edges <- edges_master[edges_master$pmid %in% tempdata$esearchresult$idlist,]

install.packages('networkD3')
library(networkD3)

src <- edges$from
target <- edges$to
networkData <- data.frame(src, target)
simpleNetwork(networkData)



edges$from
as.data.frame(table(edges$from))

nodes2 <- as.data.frame(table(edges$from))
nodes3 <- as.data.frame(table(edges$to))

v <- as.data.frame (unique(c(edges[,2], edges[,3])) )
colnames(v) <- c("id")

colnames(nodes2) <- c("id", 'Freq')
colnames(nodes3) <- c("id", 'Freq')

rpnodes <- edges[edges$from %in% nodes2$id, ]
spnodes <- edges[edges$to %in% nodes3$id, ]

nodes_new <- rbind(nodes2 , nodes3 )

forceNetwork(Links = edges, Nodes = nodes_new, Source = "from",
             Target = "to", 
              NodeID = "id",
             Group = 'Freq'
             , opacity = 0.7,
             colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"))


nodes_new[!duplicated(nodes_new), ]

c(e[,1], e[,2]) %in% nodes_new


nodes_new

net <- graph_from_data_frame(d= edges, vertices = v, directed = T)

c(edges[,3], edges[,2]) %in% v$id



write.csv(edges, "edges.csv")
write.csv(v, "vertices.csv")
