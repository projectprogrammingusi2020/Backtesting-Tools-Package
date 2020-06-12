ValueatRisk <-
function(x){
  a<-quantile(x,c(0.01,0.05))
  names(a)<-c("VaR(99%,1)","VaR(95%,1)")
  return(a)
}
