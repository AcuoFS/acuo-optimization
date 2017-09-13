PortOpti <- function(IOAll){
  for(i in 1:length(IOAll)){
    input <- IOAll[[i]]$input
    
    tradeNum <- input$tradeNum
    DV01 <- input$DV01
    portfolioIds <- input$portfolioIds
    tradeIds <- input$tradeIds
    tradeDV01s <- input$tradeDV01s
    
    moveLimit <- 10
    result <- OptiMoveOneTradeEachTime(DV01,tradeNum,portfolioIds,tradeIds,tradeDV01s,moveLimit)
    IOAll[[i]]$output <- result
  }
  return(IOAll)
}

# Execute the trades reorganizing
OptiMoveOneTradeEachTime <- function(DV01,tradeNum,portfolioIds,tradeIds,tradeDV01s,moveLimit){
  # DV01: DV01 of each account
  # tradeNum: number of trades in each portfolio(account)
  # portfolioIds: portfolio Id for trades
  # tradeIds
  # tradeDV01s
  # movements: record the movements number of the trades
  # N: number of portfolios
  
  N <- length(portfolioIds)
  movements <- 0
  totalDV01 <- sum(DV01)
  
  instructions <- list()
  
  while(sum(DV01[which(DV01>0)])>0 & sum(DV01[which(DV01<0)])<0 & movements<moveLimit){

    DV01Pos <- rep(0,N)
    DV01Neg <- rep(0,N)
    
    idxPos <- which(DV01>0)
    idxNeg <- which(DV01<0)
    
    DV01Pos[idxPos] <- DV01[idxPos]  
    DV01Neg[idxNeg] <- DV01[idxNeg]
    
    
    # DECISION
    toMove <- TradeMoveDecision(N,portfolioIds,DV01,tradeDV01s,tradeIds)

    # EXECUTION
    if(toMove$reduc>0){
      # instructions
      instructions[[movements+1]] <- toMove
      
      # extract the trade from the decision
      moveTradeId <- toMove$tradeId
      idxFromPort <- which(portfolioIds==toMove$moveFrom)
      idxToPort <- which(portfolioIds==toMove$moveTo)
      idxTrade <- which(tradeIds[[idxFromPort]]==moveTradeId)
      moveTradeDV01 <- tradeDV01s[[idxFromPort]][idxTrade]
      
      # remove from moveFrom portfolio
      tradeIds[[idxFromPort]] <- tradeIds[[idxFromPort]][-idxTrade]
      tradeDV01s[[idxFromPort]] <- tradeDV01s[[idxFromPort]][-idxTrade]
      DV01[idxFromPort] <- sum(tradeDV01s[[idxFromPort]])
      tradeNum[idxFromPort] <- tradeNum[idxFromPort]-1
      
      # add to moveTo portfolio
      tradeIds[[idxToPort]][length(tradeIds[[idxToPort]])+1] <- moveTradeId
      tradeDV01s[[idxToPort]][length(tradeDV01s[[idxToPort]])+1] <- moveTradeDV01
      DV01[idxToPort] <- sum(tradeDV01s[[idxToPort]])
      tradeNum[idxToPort] <- tradeNum[idxToPort]+1
      
      movements <- movements+1
    } else{
      break
    }
  }

  result <- list(DV01=DV01,tradeNum=tradeNum,portfolioIds=portfolioIds,
                 tradeIds=tradeIds,tradeDV01s=tradeDV01s,
                 movements=movements,instructions=instructions)
  return(result)
}

# Decision function: find the maximum IM reduction
TradeMoveDecision <- function(N,portfolioIds,DV01,tradeDV01s,tradeIds){
  DV01Pos <- rep(0,N)
  DV01Neg <- rep(0,N)
  
  idxPos <- which(DV01>0)
  idxNeg <- which(DV01<0)
  
  DV01Pos[idxPos] <- DV01[idxPos]  
  DV01Neg[idxNeg] <- DV01[idxNeg]
  
  reduc <- rep(0,N)
  moveFrom <- portfolioIds
  moveTrade <- rep('',N)
  moveTo <- rep('',N)
  for(k in 1:N){
    isPos <- k %in% idxPos
    if(isPos){
      maxIdxNeg <- which.max(abs(DV01Neg)) # the maximum -DV01 portfolio index
      maxNeg <- DV01Neg[maxIdxNeg]
      compare <-abs(DV01[k])-abs(DV01[k]-tradeDV01s[[k]])+abs(maxNeg)-abs(maxNeg-tradeDV01s[[k]])
      if(length(which(compare>0))>0){
        idx <- which.max(compare)
        reduc[k] <- compare[idx]
        moveTo[k] <- portfolioIds[maxIdxNeg]
        moveTrade[k] <- tradeIds[[k]][idx]
      }
    } else{
      maxIdxPos <- which.max(DV01Pos)  # the maximum +DV01 portfolio index
      maxPos <- DV01Pos[maxIdxPos]
      compare <- abs(DV01[k])-abs(DV01[k]-tradeDV01s[[k]])+maxPos-abs(maxPos+tradeDV01s[[k]])
      if(length(which(compare>0))>0){
        idx <- which.max(compare)
        reduc[k] <- compare[idx]
        moveTo[k] <- portfolioIds[maxIdxPos]
        moveTrade[k] <- tradeIds[[k]][idx]
      }
    }
  }
  idxMax <- which.max(reduc)
  return(list(moveFrom=moveFrom[idxMax],moveTo=moveTo[idxMax],tradeId=moveTrade[idxMax],reduc=reduc[idxMax]))
}

