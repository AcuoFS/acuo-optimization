printInstructions <- function(idx){
  for(j in idx){
    category <- IOAll[[j]]$category
    input <- IOAll[[j]]$input
    output <- IOAll[[j]]$output
    
    cat('Category of trades with currency(',category$currency, ') tradeType(',category$tradeType,'):\n\n')
    cat("DV01 of each portfolio:",output$DV01,'\n')
    cat("Number of trades in each portfolio:",output$tradesNum,'\n')
    cat("Original portfolio of each trade:",output$portfolioIds,'\n')
    cat("Total movements:",output$movements,'\n')
    for(k in 1:length(input$portfolioIds)){
      if(output$portfolioIds[k]!=input$portfolioIds[k]){
        tradeIds <- paste(input$tradeIds[[portfolioIds[k]]],collapse=',')
        cat('Move trade(s) from portfolio [', input$portfolioIds[k],'(',tradeIds, ')] to portfolio [', output$portfolioIds[k],'].','\n')
      } 
    }
    cat('\n')
  }
}
