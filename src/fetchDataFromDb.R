
library('RNeo4j')

neo4jDevUrl <- "http://dev.acuo.com:7474/db/data"
neo4jLocalUrl = "http://localhost:7474/db/data/"

portfolioInfoByPortfolioIdCypherPath <- "https://raw.githubusercontent.com/AcuoFS/acuo-optimization/master/src/Cypher/portfolioInfo.cql"

ExecuteCypher <- function(path,...){
  params <- list(...)
  query = paste(readLines(path), collapse="\n")
  #graph = startGraph(neo4jUrl)
  graph = startGraph(neo4jDevUrl)
  #graph = startGraph(neo4jLocalUrl,username='neo4j',password='neo4j')
  cypher(graph,query,params)
}

PortfolioInfo <- function(portfolioIds){
  portfolioIds <- c(portfolioIds,'nonexist')
  ExecuteCypher(path=portfolioInfoByPortfolioIdCypherPath,portfolioIds=portfolioIds)
}
