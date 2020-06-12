\name{retcalc}
\alias{retcalc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
retcalc(prices, classifica, percentile, holdingperiod, freq, datestrategy, date, long = TRUE, short = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prices}{
%%     ~~Describe \code{prices} here~~
}
  \item{classifica}{
%%     ~~Describe \code{classifica} here~~
}
  \item{percentile}{
%%     ~~Describe \code{percentile} here~~
}
  \item{holdingperiod}{
%%     ~~Describe \code{holdingperiod} here~~
}
  \item{freq}{
%%     ~~Describe \code{freq} here~~
}
  \item{datestrategy}{
%%     ~~Describe \code{datestrategy} here~~
}
  \item{date}{
%%     ~~Describe \code{date} here~~
}
  \item{long}{
%%     ~~Describe \code{long} here~~
}
  \item{short}{
%%     ~~Describe \code{short} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (prices, classifica, percentile, holdingperiod, freq, 
    datestrategy, date, long = TRUE, short = FALSE) 
{
    r <- f.rendperc(prices)
    ret <- xts(x = r, order.by = date[-1])
    if (long == T) {
        portafoglio <- classifica[, (ncol(classifica) - ceiling(ncol(classifica) * 
            percentile) + 1):ncol(classifica)]
    }
    else {
        portafoglio <- matrix(rep(0, nrow(classifica) * ceiling(ncol(classifica) * 
            percentile)), ncol = ceiling(ncol(classifica) * percentile))
    }
    if (short == T) {
        portafoglio <- cbind(portafoglio, classifica[, 1:ceiling(ncol(classifica) * 
            percentile)])
    }
    if (datestrategy[length(datestrategy)] == index(last(ret))) {
        dateperend <- datestrategy
    }
    else dateperend <- c(datestrategy, index(last(ret)))
    cmpr <- list(NA)
    rnet <- list(NA)
    for (i in 1:(length(dateperend) - 1)) {
        foglia <- ret[index(ret)[(which(dateperend[i] == (index(ret))) + 
            1):(which(dateperend[(i + 1)] == index(ret)))]]
        d <- foglia[, portafoglio[i, ]]
        if (long == T & short == T) {
            x <- rowMeans(cbind(rowMeans(d[, 1:(ncol(d)/2)]), 
                rowMeans(-d[, -(1:(ncol(d)/2))])))
        }
        else {
            if (long == F) {
                x <- rowMeans(-d)
            }
            if (short == F) {
                x <- rowMeans(d)
            }
        }
        cmpr[[i]] <- cumprod((1 + x))
        rnet[[i]] <- x
    }
    a <- as.vector(unlist(rnet, use.names = FALSE))
    names(cmpr) <- dateperend[-1]
    l <- list(a, cmpr)
    names(l) <- c("Vector of rend perc", "List of daily cumulative return per trade")
    return(l)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
