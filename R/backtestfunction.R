backtestfunction <-
function(prices, date, ..., f, percentile, freq, holdingperiod, long = TRUE , short = FALSE, revert = FALSE){ 
  re<-f( date = date, prices = prices, freq= freq, holdingperiod = holdingperiod, ...)
  resul<-re[[1]]
  classifica<- clas(resul)
  if( revert == T ){ classifica<- apply(classifica, 2, rev)}
  rets<-retcalc(prices,classifica,percentile,holdingperiod,freq,datestrategy =  re$date,date)
  r<-as.vector(rets$`Vector of rend perc`)
  gra<-multi_plot(date[-(1:freq)][-1],r)
  a<-list(rets$`List of daily cumulative return per trade`, r,date[-(1:freq)][-1], cumprod(r+1), summary(r),ValueatRisk(rets$`Vector of rend perc`),gra)
  names(a)<-c('Daily cumulative returns by trade','vector of returns','Dates of returns ','daily cumulative returns','empirical distribution statistics','VaR','graphics')
  return(a)
}
