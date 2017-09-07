
setwd('E:/ACUO/projects/acuo-optimization/')

source('src/portOpti.R')
source('src/inputConstruct.R')
source('src/generalFunctions.R')
source('src/fetchDataFromDb.R')


#### PORTFOLIO INPUT FROM JAVA ###############
portfolioIds <- c('p31','p32','p33','p34','p35','p36','p37','p38','p39','p40')
portfolio <- portfolioInfo(portfolioIds)

# generate randome numbers between -10,000 and +10,000 as DV01
portfolio$DV01 <- sample(-100:100,length(portfolio[,1]),replace = TRUE)*100
portfolio$tradeNum <- rep(1,length(portfolio[,1]))
#### PORTFOLIO INPUT FROM JAVA END ############

#### INPUT AND OUTPUT ###########
IOAll <- inputConstruct(portfolio)

IOAll <- portOpti(IOAll)
#### INPUT AND OUTPUT END ########

# Instructions for each category 
printInstructions(c(1:18))
