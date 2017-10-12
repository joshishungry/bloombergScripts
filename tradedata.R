options(stringsAsFactors = FALSE)

#rm(list=ls())

gc()

library(reshape2)
library(Rblpapi)


# --------------------------------------------------------------------------------
##### SECTION:  Key inputs

ticker <- c("SIA SP Equity","KEP SP Equity")

fields <- c("TRADE")

analysisTime <- strptime("17/05/2016 09:00:00", 
                         format = "%d/%m/%Y %H:%M:%OS",
                         tz = "Asia/Kuala_Lumpur")


# --------------------------------------------------------------------------------
##### SECTION: Create empty list for storing results


list_results <- vector("list",length(ticker))

 

# --------------------------------------------------------------------------------
##### SECTION: Function to get data from bloomberg

for (i in 1:length(list_results)) {
  blpConnect()
  list_results[[i]] <- getMultipleTicks(ticker[i], eventType = fields, 
                               startTime = analysisTime,
                               endTime = Sys.time(), 
                               returnAs = getOption("blpType","matrix"), 
                               tz = "Asia/Kuala_Lumpur"
                                )
  }

x<- list_results

# --------------------------------------------------------------------------------
##### SECTION: Calculations

names(list_results) <- ticker

list_results <- lapply(list_results,function(x){
  
  ### Write code for calculations here ###
  
  
  return(x)
})

# --------------------------------------------------------------------------------
##### SECTION: Collapse list into dataframe


df_results <- melt(data = list_results, do.call(unique,(lapply(list_results,colnames))))

head(df_results)
