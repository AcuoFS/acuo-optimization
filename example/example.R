
setwd('E:/ACUO/projects/acuo-optimization/')

source('src/portOpti.R')
source('src/inputConstruct.R')
source('src/generalFunctions.R')
source('src/fetchDataFromDb.R')


#### PORTFOLIO INPUT FROM JAVA ###############
portfolioIds <- c('p2','p4','p6','p31','p32','p33')
portfolio <- PortfolioInfo(portfolioIds)

# generate randome numbers between -10,000 and +10,000 as DV01
portfolio$DV01 <- sample(-100:100,length(portfolio[,1]),replace = TRUE)*100
portfolio$tradeNum <- rep(1,length(portfolio[,1]))
bi2clear <- rep(FALSE,length(portfolio[,1]))
eliTradeType <- c('IRS','FRA')
bi2clear[which(portfolio$agreementType=='bilateral' & portfolio$tradeType %in% eliTradeType)] <- TRUE
portfolio$bi2clear <- bi2clear
#### PORTFOLIO INPUT FROM JAVA END ############

#### INPUT AND OUTPUT ###########
IOAll <- InputConstruct(portfolio)

# run Optimizer
IOAll <- PortOpti(IOAll)
#### INPUT AND OUTPUT END ########

# Instructions for each category 
PrintInstructions(c(1:10))



