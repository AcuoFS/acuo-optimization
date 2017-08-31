
source('src/portOpti.R')
accounts <- c(80,-50,-20,40,70,-70,50,-70)
tradesNum <- c(1,2,4,1,3,1,2,1)
result <- portOpti(accounts,tradesNum)
