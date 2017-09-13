source('src/portOpti_moveEntirePort.R')

DV01_1 <- c(80,-50,-20,40,70,-70,50,-70)
tradeNum1 <- c(1,2,4,1,3,1,2,1)
portfolioIds1 <- c('p1','p2','p3','p4','p5','p6','p7','p8')


testPortOptiCoreWithForSingleCurrencyAndTradeType <- function(){
  DV01 <- DV01_1
  tradeNum <- tradeNum1
  portfolioIds <- portfolioIds1
  
  result <- OptiMoveEntirePort(DV01,tradeNum,portfolioIds)
  
  checkEquals(result$DV01,c(10,0,0,20,0,0,0,0))
  checkEquals(result$tradeNum,c(2,4,0,5,4,0,0,0))
  checkEquals(result$portfolioIds,c('p1','p2','p4','p4','p5','p1','p2','p5'))
  checkEquals(result$movements,8)
}