multi_plot <-
function(zdate,k){
  w<-rep(NA,length(k))
  w[which((k>0) == T)]<-"Positive"
  w[which(is.na(w) == T)] <- "Negative"
  df <- data.frame("Date" = zdate, "Cumulative_Return" = cumprod(k+1), "Ret" = k, "Returns" = w)
  
  y <- ggplot(df, aes(Date, Cumulative_Return)) +
    geom_line() + 
    ylab("Cumulative Return per unit invested")
  
  z <- ggplot(df, aes(k, fill = Returns)) +
    geom_histogram(bins = ceiling(nrow(df))/10) +
    xlab("Returns") +
    ggtitle("Empirical distribution of returns") +
    scale_color_identity()
  l<-list(y,z)
  names(l)<-c("graph of cumulative returns per unit invested","empirical distribution of returns")
  return(l)
}
