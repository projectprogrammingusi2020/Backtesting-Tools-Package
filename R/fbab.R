fbab <-
function(date,prices,freq,holdingperiod,mkt){ 
  retper<-f.rendperc(prices)
  rmkt<-mkt[-length(mkt)]/mkt[-1] -1
  r<- estimateextractor(retper,holdingperiod,freq)
  m<- estimateextractor(rmkt,holdingperiod,freq)
  d<- estimateextractor(date[-1],holdingperiod,freq)
  da<-d[[1]][length(d[[1]])]
  beta<-var(cbind(m[[1]],r[[1]]))[,1][-1]/var(cbind(m[[1]],r[[1]]))[,1][1]
  for (i in 2:length(d)) {
    da<-c(da,d[[i]][length(d[[i]])])
    beta<-rbind(beta,var(cbind(m[[i]],r[[i]]))[,1][-1]/var(cbind(m[[i]],r[[i]]))[,1][1])
  }
  l<-list(beta,da)
  names(l)<-c("mtx","date")
  return(l)
}
