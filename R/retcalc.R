retcalc <-
function(prices,classifica,percentile,holdingperiod,freq,datestrategy,date, long = TRUE , short = FALSE ){
  r<-f.rendperc(prices)
  ret<-xts(x=r, order.by = date[-1]) 
  
  if(long == T){
    portafoglio<- classifica[,(ncol(classifica)- ceiling(ncol(classifica)*percentile) +1)  : ncol(classifica) ] 
  }else{portafoglio<-matrix(rep(0,nrow(classifica)*ceiling(ncol(classifica)*percentile)),ncol = ceiling(ncol(classifica)*percentile) )}
  if(short == T){
    portafoglio<-cbind(portafoglio, classifica[,1:ceiling(ncol(classifica)*percentile) ])
  } 
  
  if( datestrategy[length(datestrategy)] == index(last(ret)) ){
    dateperend<- datestrategy
  }else dateperend<-c(datestrategy,index(last(ret)) )
  
  cmpr<- list(NA)
  rnet<- list(NA)
  
  for (i in 1:(length(dateperend)-1)) {
    foglia<-ret[index(ret)[ (which(dateperend[i] == (index(ret)) )+1) :(which(dateperend[(i+1)] == index(ret) ))] ]
    d<-foglia[,portafoglio[i,]]
    if(long == T & short == T){
      x<-rowMeans(cbind(rowMeans(d[,1:(ncol(d)/2)]),rowMeans(-d[,-(1:(ncol(d)/2))]))) 
    }else{
      if(long == F){ x<- rowMeans(-d) }
      if(short == F){ x<- rowMeans(d) }
    }
    cmpr[[i]]<- cumprod((1+x))
    rnet[[i]]<-x
  }
  
  a<-as.vector(unlist(rnet,use.names=FALSE))
  names(cmpr)<-dateperend[-1]
  l<-list(a,cmpr)
  names(l)<-c("Vector of rend perc","List of daily cumulative return per trade")
  return(l)
}
