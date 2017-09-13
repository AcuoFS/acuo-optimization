
#### INPUT CONSTRUCT FUNCTION #######
InputConstruct <- function(portfolio){
  P <- portfolio
  
  all <- list()
  
  # group by trade types
  # aggregate the entire data fram
  # to get all of the currency-tradeType combinations
  types <- aggregate(cbind(P$DV01,P$tradeNum),by = list(P$currency,P$tradeType),FUN='sum')
  colnames(types) <- c('currency','tradeType','DV01','tradeNum')
  
  aggregation <- aggregate(cbind(P$DV01,P$tradeNum),by = list(P$currency,P$tradeType,P$portfolioId),FUN='sum')
  colnames(aggregation) <- c('currency','tradeType','portfolio','DV01','tradeNum')
  
  aggregateTrades <- aggregate(cbind(P$tradeId,P$DV01),by = list(P$currency,P$tradeType,P$portfolioId),FUN='paste')
  colnames(aggregateTrades) <- c('currency','tradeType','portfolio','tradeIds','DV01s')
  names(aggregateTrades$tradeIds) <- aggregateTrades$portfolio
  names(aggregateTrades$DV01s) <- aggregateTrades$portfolio
  # convert the DV01s to numeric
  for(k in 1:length(aggregateTrades$tradeIds)){
    aggregateTrades$DV01s[[k]] <- as.numeric(aggregateTrades$DV01s[[k]])
  }
  
  
  for(i in 1:length(types$currency)){
    currency <- types$currency[i]
    tradeType <- types$tradeType[i]
    
    idx <- which(aggregation$currency==currency & aggregation$tradeType==tradeType)
    tempPortfolioIds <- aggregation$portfolio[idx]
    tempTradeNum <- aggregation$tradeNum[idx]
    tempDV01 <- aggregation$DV01[idx]
    
    idxTrades <- which(aggregateTrades$currency==currency & aggregateTrades$tradeType==tradeType)
    tradeIds <- aggregateTrades$tradeIds[idxTrades]
    tradeDV01s <- aggregateTrades$DV01s[idxTrades]
    
    all[[i]] <- list()
    all[[i]]$category <- list(currency=currency,tradeType=tradeType)
    all[[i]]$input <- list(portfolioIds=tempPortfolioIds,tradeNum=tempTradeNum,DV01=tempDV01,
                           tradeIds=tradeIds,tradeDV01s=tradeDV01s)
  }
  
  return(all)
}

