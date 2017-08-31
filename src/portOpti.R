
portOpti <- function(accounts,tradesNum){
  # accounts: DV01 of each account, initially 1 trade in each account
  # N: number of accounts
  # tradesNum: number of trades in each account
  # accountsIdx: account index for trade
  # movements: record the movements number of the trades
  
  N <- length(accounts)
  accountsIdx <- 1:N
  movements <- 0
  
  accountsPos <- rep(0,N)
  accountsNeg <- rep(0,N)
  accountsPos[which(accounts>0)] <- accounts[which(accounts>0)]  
  accountsNeg[which(accounts<0)] <- accounts[which(accounts<0)]

  while(sum(accountsPos)>0 && sum(accountsNeg)<0){
    maxIdxPos <- which.max(accountsPos) # the maximum DV01 account index
    maxPos <- accountsPos[maxIdxPos]
    maxIdxNeg <- which.max(abs(accountsNeg))
    maxNeg <- accountsNeg[maxIdxNeg]
    
    # compare the trades in the two opposite account
    # move the smaller absolute DV01 to the larger DV01 account
    # if same, move the account with less trades
    if(maxPos > abs(maxNeg)){
      accountsPos[maxIdxPos] <- maxPos + maxNeg
      accountsNeg[maxIdxNeg] <- 0
      movements <- movements + tradesNum[maxIdxNeg]
      tradesNum[maxIdxPos] <- tradesNum[maxIdxPos]+tradesNum[maxIdxNeg]
      tradesNum[maxIdxNeg] <- 0
      accountsIdx[maxIdxNeg] <- maxIdxPos
      
      
    } else if(maxPos < abs(maxNeg)){
      accountsPos[maxIdxPos] <- 0
      accountsNeg[maxIdxNeg] <- maxPos + maxNeg
      movements <- movements + tradesNum[maxIdxPos]
      tradesNum[maxIdxPos] <- 0
      tradesNum[maxIdxNeg] <- tradesNum[maxIdxNeg]+tradesNum[maxIdxPos]
      accountsIdx[maxIdxPos] <- maxIdxNeg
      
      
    } else{
      if(tradesNum[maxIdxPos] > tradesNum[maxIdxNeg]){
        accountsPos[maxIdxPos] <- maxPos + maxNeg
        accountsNeg[maxIdxNeg] <- 0
        movements <- movements + tradesNum[maxIdxNeg]
        tradesNum[maxIdxPos] <- tradesNum[maxIdxPos]+tradesNum[maxIdxNeg]
        tradesNum[maxIdxNeg] <- 0
        accountsIdx[maxIdxNeg] <- maxIdxPos
        
      } else{
        accountsPos[maxIdxPos] <- 0
        accountsNeg[maxIdxNeg] <- maxPos + maxNeg
        movements <- movements + tradesNum[maxIdxPos]
        tradesNum[maxIdxPos] <- 0
        tradesNum[maxIdxNeg] <- tradesNum[maxIdxNeg]+tradesNum[maxIdxPos]
        accountsIdx[maxIdxPos] <- maxIdxNeg
        
      }
    }
    accounts <- accountsPos+accountsNeg
  }
  cat("DV01 of each account:",accounts,'\n')
  cat("Number of trades in each account",tradesNum,'\n')
  cat("Original account of each trade",accountsIdx,'\n')
  cat("Total movements:",movements,'\n')
  
  # instructions
  for(k in 1:N){
    if(accountsIdx[k]!=k){
      cat('Move trade(s) from account [', k, '] to account [', accountsIdx[k],'].','\n')
    }
  }
  result <- list(accounts=accounts,tradesNum=tradesNum,accountsIdx=accountsIdx)
}


