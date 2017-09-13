source('src/portOpti.R')

DV01_1 <- c(-20481,38566,27919)
tradeNum1 <- c(6,7,7)
portfolioIds1 <- c('p31','p32','p33')
tradeIds1 <- list()
tradeDV01s1 <- list()
tradeIds1[['p31']] <-  c("455519","455504","455489","455534","455459","455474")
tradeDV01s1[['p31']] <- c(-4148,-4030,-3724,-3469,-3272,-1838)

tradeIds1[['p32']] <-  c("455544","455514","455499","455529","455454","455484","455469")
tradeDV01s1[['p32']] <- c(3623,3928,4549,5181,5502,7491,8292)

tradeIds1[['p33']] <-  c("455524","455509","455494","455539","455139","455479","455464")
tradeDV01s1[['p33']] <- c(16160,16562,17103,-5971,-5937,-5376,-4622)

moveLimit1 <- 10

testPortOptiCoreWithForSingleCurrencyAndTradeType <- function(){
  DV01 <- DV01_1
  tradeNum <- tradeNum1
  portfolioIds <- portfolioIds1
  tradeIds <- tradeIds1
  tradeDV01s <- tradeDV01s1
  moveLimti <- moveLimit1
  
  result <- OptiMoveOneTradeEachTime(DV01,tradeNum,portfolioIds,tradeIds,tradeDV01s,moveLimit)
  
  checkEquals(result$DV01,c(0,23392,22612))
  checkEquals(result$tradeNum,c(0,11,9))
  checkEquals(result$portfolioIds,c('p31','p32','p33'))
  checkEquals(result$movements,6)
}