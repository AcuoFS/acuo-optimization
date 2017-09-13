PrintInstructions <- function(idx){
  for(j in idx){
    category <- IOAll[[j]]$category
    input <- IOAll[[j]]$input
    output <- IOAll[[j]]$output
    
    cat(j,'. Category of trades with currency(',category$currency, ') tradeType(',category$tradeType,'):\n\n')
    cat("Portfolios:",output$portfolioIds,'\n')
    cat("DV01 before optimization:",input$DV01,'\n')
    cat("DV01 after optimization:",output$DV01,'\n')
    cat("Number of trades before optimization:",input$tradeNum,'\n')
    cat("Number of trades after optimization:",output$tradeNum,'\n')
    cat("Total movements:",output$movements,'\n')
    if(output$movements>0){
      cat("Instructions:",'\n')
      for(k in 1:output$movements){
        instruction <- output$instructions[[k]]
        cat('Trade',instruction$tradeId,'move from',instruction$moveFrom,
            'to',instruction$moveTo,', IM reduction:',instruction$reduc,'\n')
      }
    }
    cat('\n')
  }
}
