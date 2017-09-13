source('src/portOpti_moveEntirePort.R')
source('src/inputConstruct.R')
source('src/fetchDataFromDb.R')

# example 1
portfolioIds1 <- c('p31','p32','p33','p34','p35','p36','p37','p38','p39','p40')
portfolio1 <- PortfolioInfo(portfolioIds1)


tempDV01 <- ((1:length(portfolio1[,1]))^1.5-3000)*2
tempIdx <- (1:floor(length(portfolio1[,1])/2))*2
tempX <- tempDV01[tempIdx]
tempDV01 <- tempDV01[-tempIdx]
tempDV01[(length(tempDV01)+1):(length(tempDV01)+length(tempX))] <- tempX

portfolio1$DV01 <- round(tempDV01)
portfolio1$tradeNum <- rep(1,length(portfolio1[,1]))
IOAll1 <- InputConstruct(portfolio1)


testPortOptiWithMultiCurrencyAndTradeType <- function(){
  IOAll <- IOAll1
  IOAll <- PortOpti(IOAll)
  
  result <- IOAll[[1]]$output
  
  checkEquals(result$DV01,c(0,27176,56664))
  checkEquals(result$tradesNum,c(0,27,13))
  checkEquals(result$portfolioIds,c('p32','p32','p33'))
  checkEquals(result$movements,14)
  
  result <- IOAll[[12]]$output
  
  checkEquals(result$DV01,c(0,1352,21595,33914,79085,212943))
  checkEquals(result$tradesNum,c(0,15,15,7,7,20))
  checkEquals(result$portfolioIds,c( "p40","p35","p36","p37","p38","p40"))
  checkEquals(result$movements,6)
}
