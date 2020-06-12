f.rendperc <-
function(prices) prices[-nrow(prices),]/prices[-1,] - 1
