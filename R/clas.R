clas <-
function(resul){
  classifica<-sort(resul[1,],index.return=T)$ix
  for (i in 2:nrow(resul)) {
    classifica<-rbind(classifica,sort(resul[i,],index.return=T)$ix)
  }
  return(classifica)
}
