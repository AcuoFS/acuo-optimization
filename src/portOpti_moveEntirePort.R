PortOpti <- function(IOAll){
  for(i in 1:length(IOAll)){
    input <- IOAll[[i]]$input
    tradeNum <- input$tradeNum
    DV01 <- input$DV01
    portfolioIds <- input$portfolioIds
    IOAll[[i]]$output <- OptiMoveEntirePort(DV01,tradeNum,portfolioIds)
  }
  return(IOAll)
}

OptiMoveEntirePort <- function(DV01,tradeNum,portfolioIds){
  # DV01: DV01 of each account, initially 1 trade in each portfolio(account)
  # N: number of DV01
  # tradeNum: number of trades in each portfolio(account)
  # portfolioIds: portfolio Id for trades
  # DV01Idx: portfolio index for trades
  # movements: record the movements number of the trades
  
  N <- length(DV01)
  DV01Idx <- 1:N
  movements <- 0
  
  DV01Pos <- rep(0,N)
  DV01Neg <- rep(0,N)
  DV01Pos[which(DV01>0)] <- DV01[which(DV01>0)]  
  DV01Neg[which(DV01<0)] <- DV01[which(DV01<0)]
  
  while(sum(DV01Pos)>0 && sum(DV01Neg)<0){
    maxIdxPos <- which.max(DV01Pos) # the maximum DV01 account index
    maxPos <- DV01Pos[maxIdxPos]
    maxIdxNeg <- which.max(abs(DV01Neg))
    maxNeg <- DV01Neg[maxIdxNeg]
    
    # compare the trades in the two opposite account
    # move the smaller absolute DV01 to the larger DV01 account
    # if same, move the account with less trades
    if(maxPos > abs(maxNeg)){
      DV01Pos[maxIdxPos] <- maxPos + maxNeg
      DV01Neg[maxIdxNeg] <- 0
      movements <- movements + tradeNum[maxIdxNeg]
      tradeNum[maxIdxPos] <- tradeNum[maxIdxPos]+tradeNum[maxIdxNeg]
      tradeNum[maxIdxNeg] <- 0
      DV01Idx[maxIdxNeg] <- maxIdxPos
      
      
    } else if(maxPos < abs(maxNeg)){
      DV01Pos[maxIdxPos] <- 0
      DV01Neg[maxIdxNeg] <- maxPos + maxNeg
      movements <- movements + tradeNum[maxIdxPos]
      tradeNum[maxIdxNeg] <- tradeNum[maxIdxNeg]+tradeNum[maxIdxPos]
      tradeNum[maxIdxPos] <- 0
      DV01Idx[maxIdxPos] <- maxIdxNeg
      
      
    } else{
      if(tradeNum[maxIdxPos] > tradeNum[maxIdxNeg]){
        DV01Pos[maxIdxPos] <- maxPos + maxNeg
        DV01Neg[maxIdxNeg] <- 0
        movements <- movements + tradeNum[maxIdxNeg]
        tradeNum[maxIdxPos] <- tradeNum[maxIdxPos]+tradeNum[maxIdxNeg]
        tradeNum[maxIdxNeg] <- 0
        DV01Idx[maxIdxNeg] <- maxIdxPos
        
      } else{
        DV01Pos[maxIdxPos] <- 0
        DV01Neg[maxIdxNeg] <- maxPos + maxNeg
        movements <- movements + tradeNum[maxIdxPos]
        tradeNum[maxIdxNeg] <- tradeNum[maxIdxNeg]+tradeNum[maxIdxPos]
        tradeNum[maxIdxPos] <- 0
        DV01Idx[maxIdxPos] <- maxIdxNeg
        
      }
    }
    DV01 <- DV01Pos+DV01Neg
  }
  portfolioIds <- portfolioIds[DV01Idx]
  
  #cat("DV01 of each account:",DV01,'\n')
  #cat("Number of trades in each account",tradeNum,'\n')
  #cat("Original account of each trade",DV01Idx,'\n')
  #cat("Total movements:",movements,'\n')
  
  # instructions
  for(k in 1:N){
    if(DV01Idx[k]!=k){
      #cat('Move trade(s) from portfolio [', k, '] to portfolio [', portfolioIds[k],'].','\n')
    }
  }
  result <- list(DV01=DV01,tradeNum=tradeNum,portfolioIds=portfolioIds,movements=movements)
}

