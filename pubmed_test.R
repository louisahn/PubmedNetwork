
install.packages('reutils')
install.packages('jsonlite')
install.packages('dplyr')
install.packages("data.table")


#PubMed API Search
#This code can be used to send text string queries to pubmed and return a pmid list. That list can be used to filter the database. 

#load libraries 
library(reutils)
library(jsonlite)
library(dplyr)
library(data.table)

### RUN QUERY PUBMED, GET LIST OF PMIDs
#create pmid list
pmidlist = list()

#get total number of records (this is required for managing api limits) 
n_pubmed <- esearch("leukemia",db="pubmed",retmode = "json", rettype = "count", datetype = "pdat"
                    ,mindate = "2010",maxdate = "2017")
n_pubmed[1]
batches <- ceiling(n_pubmed[1]/100000)
batches
#use API esearch get UID list 


sink("output.txt")

for (i in 0:(batches-1)){ 
  # "leukemia below can be repaced with input variable in R Shiny
  temp <- esearch("leukemia",db="pubmed",retmode = "json", retstart = i, retmax = "100000", datetype = "pdat",mindate = "2010",maxdate = "2017")
  tempdata <- fromJSON(content(temp))
  pmidlist <- append(pmidlist,tempdata$esearchresult$idlist)
  print(tempdata$esearchresult$idlist)
  
  }
sink()

write.table(as.data.frame(pmidlist),file="leukemia.txt", quote=F,sep=",",row.names=F)
cat(capture.output(print(pmidlist), file="test.txt"))
cat(capture.output(summary(pmidlist), file = "My New File.txt"))

bb = as.data.frame(pmidlist)
summary(bb)

print(pmidlist)    

#Can be used to filter database on master_edges  
edges_active <- master_edges[master_edges$pmid %in% pmidlist,]

temp <- esearch("leukemia",db="pubmed",retmode = "json", retstart = 0, retmax = "100000", datetype = "pdat",mindate = "2010",maxdate = "2017")
tempdata <- fromJSON(content(temp))
pmidlist <- append(pmidlist,tempdata$esearchresult$idlist)
tempdata$esearchresult$idlist

install.packages("RSQLite") 
library("RSQLite")
setwd("E:/RWork")

# connect to the sqlite file
sqlite    <- dbDriver("SQLite")
exampledb <- dbConnect(sqlite,"t2v_test.sqlite")
dbListTables(exampledb)
dbGetQuery(exampledb, 'PRAGMA table_info(edges_master) ')
edges_master = dbGetQuery(exampledb, 'select pmid from edges_master')
dbGetQuery(exampledb, 'select * from edges_master where pmid="15024423"')
edges_active <- edges_master[edges_master$pmid %in% tempdata$esearchresult$idlist,]
edges_active
write.csv(edges_master, "edges_master.csv")
