colmatrixdalistconv <-
function(Lista){
  names<-colnames(Lista)
  ps<-Lista[[1]]
  for (i in 2:length(Lista)) {
    ps<-cbind(ps,Lista[[i]])
  }
  names(ps)<-names
  return(ps)
}
