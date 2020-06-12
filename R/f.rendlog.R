f.rendlog <-
function(prices) log(prices[-nrow(prices),]) - log(prices[-1,])
