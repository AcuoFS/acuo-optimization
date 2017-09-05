source('src/portOpti.R')

accounts1 <- c(80,-50,-20,40,70,-70,50,-70)
tradesNum1 <- c(1,2,4,1,3,1,2,1)


testPortOptiWithMultiTradesInEachAccount <- function(){
  accounts <- accounts1
  tradesNum <- tradesNum1
  result <- portOpti(accounts,tradesNum)
  
  checkEquals(result$accounts,c(10,0,0,20,0,0,0,0))
  checkEquals(result$tradesNum,c(2,2,0,5,4,0,0,0))
  checkEquals(result$accountsIdx,c(1,2,4,4,5,1,2,5))
}