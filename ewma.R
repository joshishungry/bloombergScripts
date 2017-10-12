rm(list=ls())

gc()

library(Rblpapi)

library(zoo)



  # --------------------------------------------------------------------------------
  ##### SECTION:  Key inputs

startDate <- as.Date("2016-03-31")


lamda <- 0.97

ticker <- c("NKY Index", "TWSE Index", "SIMSCI Index", " XIN9I Index", 
            "NIFTY Index", "FSSTI Index", "ISIX62IU Index")


# add ticker name to this vector if it has 2 day liquidation period (eg. iron, steel, coal)
twoDayLiquidation <-c( "ISIX62IU Index")


field <- "PX_LAST"



    # --------------------------------------------------------------------------------
    ##### SECTION:  Download data from bloomberg - used only on bloomberg terminal

blpConnect()

bloombergData <- bdh(ticker, field, start.date = as.Date("2012-09-28"))



    # --------------------------------------------------------------------------------
    ##### SECTION:  Additional field - true if ticker has two day liquidation period (eg. iron, steel, coal)


liquidationIndex <- match(twoDayLiquidation,names(bloombergData))

for (i in 1:length(bloombergData)) {
  bloombergData[[i]]$twoDayLiquidation =ifelse(any(i == liquidationIndex), T,  F)
}



    # --------------------------------------------------------------------------------
    ##### SECTION:  calculate EWMA and Historical 


list_results <- lapply(bloombergData, function(x){

    filtered = tail(x[x$date<=startDate,],246)
    
    returns = ifelse(filtered$twoDayLiquidation==T,
                   c(0,0, head( log(filtered$PX_LAST[-c(1,2)]/filtered$PX_LAST[-c(nrow(filtered)-1,nrow(filtered))]),244)),
                   c(0,  head( log(filtered$PX_LAST[-1]/filtered$PX_LAST[-nrow(filtered)]),245)) )


    results = cbind(filtered,"returns" = c(returns))

    results$weight <-  lamda^(246:1-1)
    
    results$sqr_weight <- sqrt(results$weight)
    
    weighted_sum <- rollsum(results$weight, 121 )
   
    results <- cbind(results,"weight_sum"=c(rep(0,120),weighted_sum))

    results$weighted_returns <- results$returns * results$sqr_weight
    
    matrixMultiply <- tail(rollapply(data =  results$weighted_returns, 
                                     width = 121, partial = F, function(x) {x %*% x} ) ,124)
    
    results <- cbind(results, "matrixMultiply" = c(rep(0,122),matrixMultiply))
    
    results$ewma <- 7 * ifelse(!results$weight_sum, 0, sqrt((1/results$weight_sum)*results$matrixMultiply))
    
    historical <- 7 * rollapply(results$returns, width = 121, sd)
    
    results <- cbind( results, historical = c(rep(0,120),historical))
    
plot(tail(results$historical,124), 
     type = "s" , 
     col = "blue", 
     ylim = c(min(tail(results$ewma,124),tail(results$historical,124))-0.1,max(results$ewma,results$historical)+0.1))

lines(tail(results$ewma,124), col = "red")


return (results[,c(1,10,11)])
  }) 




View(list_results)
