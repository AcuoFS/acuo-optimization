source('src/portOpti.R')

DV01_1 <- c(80,-50,-20,40,70,-70,50,-70)
tradesNum1 <- c(1,2,4,1,3,1,2,1)
portfoliosId1 <- c('p1','p2','p3','p4','p5','p6','p7','p8')


testPortOptiWithMultiTradesInEachAccount <- function(){
  DV01 <- DV01_1
  tradesNum <- tradesNum1
  portfoliosId <- portfoliosId1
  
  result <- portOpti(DV01,tradesNum,portfoliosId)
  
  checkEquals(result$DV01,c(10,0,0,20,0,0,0,0))
  checkEquals(result$tradesNum,c(2,2,0,5,4,0,0,0))
  checkEquals(result$portfoliosId,c('p1','p2','p4','p4','p5','p1','p2','p5'))
}