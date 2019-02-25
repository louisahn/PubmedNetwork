
install.packages("httr")

#Require the package so you can use it

install.packages("jsonlite")

#Require the package so you can use it
require("jsonlite")
require("httr")

base = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?"
default="db=pubmed&retmax=1000&retmode=json&term="
term=URLencode("breast cancer")


call1 <- paste(base,default,term, sep="" )

get_prices <- GET(call1)
get_prices_text <- content(get_prices, "text")
get_prices_json <- fromJSON(get_prices_text, flatten = TRUE)


get_prices_json$esearchresult$idlist


get_prices_df <- as.data.frame(get_prices_json$esearchresult$idlist)
View(get_prices_df)


