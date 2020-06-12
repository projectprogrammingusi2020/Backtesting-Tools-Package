fmomentum <-
function(date,prices,freq,holdingperiod){ 
  logret<-f.rendlog(prices)
  r<- estimateextractor(logret,holdingperiod,freq)
  d<- estimateextractor(date[-1],holdingperiod,freq)
  da<-d[[1]][length(d[[1]])]
  cumret <- vectocolmtxf(fu,r[[1]],freq)
  for (i in 2:length(d)) {
    da<-c(da,d[[i]][length(d[[i]])])
    cumret<-rbind(cumret,vectocolmtxf(fu,r[[i]],freq))
  }
  l<-list(cumret,da)
  names(l)<-c('mtx','date')
  return(l)
}
