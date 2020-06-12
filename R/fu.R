fu <-
function(x,freq) as.vector(tapply( x, (seq_along(x)-1) %/% freq, sum))
