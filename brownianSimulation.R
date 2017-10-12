rm(list=ls())

gc()

library(yuima)

set.seed(1)


sample<-100000
tradingDays <-250


m1=setModel(drift="mu*s",diffusion="sigma*s", state.var="s",time.var="t",solve.var="s",xinit=100)

simulationfunc<-function(x){
 
    simulation=simulate(x,sampling=setSampling(delta=1/tradingDays,n=tradingDays),
                        true.param=list(mu=0.03,sigma=runif(1,.2,.8)))
    
    dataList= as.data.frame(get.zoo.data(simulation),row.names=NULL)
    data = dataList$Series.1
    
  return(data)
}

#Simulate into matrix of 100,000 columns with 250 rows (1 column represents each year)
simulationResults<-replicate(sample,simulationfunc(m1))

#calculate annual returns
yearly_returns<-simulationResults[nrow(simulationResults),] / simulationResults[1,] -1 

#calculate daily returns
daily_returns <- simulationResults[-1, ] / simulationResults[ -nrow(simulationResults),] -1 

#calculate most negative daily returns for each year
minimum_shock<-apply(daily_returns,2,min)

#form buckets for results
year_return_set <- seq(-0.55,-0.30,length=6)#seq(-0.5,0,length=50)
samples <- NULL
day_return_qant <- data.frame(NULL)
quantileBuckets <- c (.05, .5)
q95_idx = 1 
q50_idx = 2


buckets <- seq(0,20,length=10)

for(i in year_return_set)
{
  data_idx_range = yearly_returns <(i+.05) &
    yearly_returns > (i-.05) &
    !is.na(yearly_returns)
  
  daily_move =   minimum_shock[data_idx_range] 
  var =  quantile(unique(daily_move), quantileBuckets, na.rm = T)
  samples <- c(samples,length(unique(daily_move)))
  
  day_return_qant <- rbind(day_return_qant, var )
}

f_line_fit_y <- function( x, y, w ) {
  data = data.frame( x = x,  y = y )
  fity = lm(y~x,data=data,weights=w)
  predict_y = predict(fity,newdata =  data)
  return( predict_y )
}
line_q50 <-  f_line_fit_y(x = year_return_set,  y = day_return_qant[[q50_idx]], w=samples     )
line_q95 <-  f_line_fit_y(x = year_return_set,  y = day_return_qant[[q95_idx]], w=samples     )


matplot(year_return_set, day_return_qant[,], pch=1, lwd = 1)
points(year_return_set,  day_return_qant[,q50_idx] ,col="red", lwd = 2)
points(year_return_set,  day_return_qant[,q95_idx] ,col="black", lwd = 2)
lines(year_return_set,  line_q50 ,col="blue", lwd = 3)
lines(year_return_set,  line_q95 ,col="blue", lwd = 3)

names(line_q95)<-year_return_set
line_q95

#matplot(year_return_set, day_return_qant[,], type="l")

