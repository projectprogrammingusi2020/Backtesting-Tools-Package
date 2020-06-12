estimateextractor <-
function(x,holdingperiod,freq){
  n<-ifelse(test = (is.matrix(x) == T), yes = nrow(x), length(x) )
  se<-seq(1:(ceiling(n/(holdingperiod*freq)) -1) )*holdingperiod*freq + freq
  if((n > se[length(se)])){ n<-se[length(se)]}
  if(is.matrix(x) == T){
    quanto<- ifelse(test = (length(se) > 1), yes =  (length(se)-1), 1 )
    a<-list(x[1:freq,])
    for (i in 1: quanto) {
      a[[i+1]]<-x[(se[i]-freq+1):(se[i]),]
    }
    a[[length(se)+1]]<- x[(se[length(se)]-freq+1) : n, ]
    
  } else{
    a<-list(x[1:freq])
    for (i in 1:length(se)) {
      a[[i+1]]<-x[(se[i]-freq+1):(se[i])]
    }
    a[[length(se)+1]]<- x[(se[length(se)]-freq+1) : n]
  }
  return(a)
}
