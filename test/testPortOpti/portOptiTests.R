source('src/portOpti.R')

# example 1
portfolioIds1 <- c('p31','p32','p33','p34','p35','p36','p37','p38','p39','p40')
portfolio1 <- portfolioInfo(portfolioIds)

set.seed(0)
portfolio1$DV01 <- round(rnorm(length(portfolio[,1]))*10000)
portfolio1$tradeNum <- rep(1,length(portfolio[,1]))
IOAll1 <- inputConstruct(portfolio)


testPortOptiWithMultiCurrencyAndTradeType <- function(){
  IOAll <- IOAll1
  IOAll <- portOpti(IOAll)
  
  result <- IOAll[[1]]$output
  
  checkEquals(result$DV01,c(3300,0,5100))
  checkEquals(result$tradesNum,c(27,0,13))
  checkEquals(result$portfolioIds,c('p31','p31','p33'))
  checkEquals(result$movements,13)
  
  result <- IOAll[[12]]$output
  
  checkEquals(result$DV01,c(0,0,0,7900,0,9800))
  checkEquals(result$tradesNum,c(0,0,0,43,0,21))
  checkEquals(result$portfolioIds,c('p37','p37','p35','p37','p40','p40'))
  checkEquals(result$movements,58)
}
