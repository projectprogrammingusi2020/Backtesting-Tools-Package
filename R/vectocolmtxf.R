vectocolmtxf <-
function(f,matri,...){
  nomi <- colnames(matri)
  matr2<- f(x = matri[,1],...)
  for (i in 2:ncol(matri)) {
    x<-matri[,i]
    matr2<-cbind(matr2, f(x=matri[,i],... ) )
  }
  colnames(matr2)<-nomi
  return(matr2)
}
