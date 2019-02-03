library(stringr)
library(igraph)

edges <- fread(paste0("http://conflictmetrics.com/data/edges/edges",'113',".csv"))    

rpnodes <- fread(paste0("http://conflictmetrics.com/data/nodes/nodes_rct_",'113',".csv"))
spnodes <- fread(paste0("http://conflictmetrics.com/data/nodes/nodes_spo_",'113',".csv"))
rpnodes <- rpnodes[rpnodes$id %in% edges$to, ]
spnodes <- spnodes[spnodes$id %in% edges$from, ]
nodes <- rbind(spnodes,rpnodes)

net <- graph_from_data_frame(d= edges, vertices = nodes, directed = T)

nodes$deg <- degree(net, v =V(net), mode = c("out"),normalized = TRUE)
#nodes$tdeg  <- degree(net, v =V(net), mode = c("out"),normalized = FALSE)
nodes$label <- strtrim(nodes$label,30)
edge.start <- ends(net, es=E(net), names=F)[,1]
edge.col <- nodes$color[edge.start]
par(oma=c(0,0,0.0,0))
par(mar=c(0,0,0,0))
plot(net, 
     layout = layout_nicely, 
     edge.arrow.size=0, 
     vertex.size= ifelse(nodes$type == "RCT", 2, nodes$deg*100), 
     edge.curved=T, 
     edge.color=edge.col, 
     edge.width=2, 
     vertex.color=nodes$color, 
     vertex.frame.color=nodes$color, 
     vertex.label.color="black", 
     vertex.label.cex= 1.5, 
     vertex.label= NA
)
colorcode <- c("black","firebrick1","gold","cornflowerblue")
legend(x="topleft", c("RCTs","Industry", "US Fed", "Other"), pch=21, col="#777777", pt.bg=colorcode, pt.cex=2, cex=.8, bty="n", ncol=1, horiz = TRUE)
}, height = 500, width = 600)
