\name{backtestfunction}
\alias{backtestfunction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
backtestfunction(prices, date, ..., f, percentile, freq, holdingperiod, long = TRUE, short = FALSE, revert = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prices}{
%%     ~~Describe \code{prices} here~~
}
  \item{date}{
%%     ~~Describe \code{date} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{f}{
%%     ~~Describe \code{f} here~~
}
  \item{percentile}{
%%     ~~Describe \code{percentile} here~~
}
  \item{freq}{
%%     ~~Describe \code{freq} here~~
}
  \item{holdingperiod}{
%%     ~~Describe \code{holdingperiod} here~~
}
  \item{long}{
%%     ~~Describe \code{long} here~~
}
  \item{short}{
%%     ~~Describe \code{short} here~~
}
  \item{revert}{
%%     ~~Describe \code{revert} here~~
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
function (prices, date, ..., f, percentile, freq, holdingperiod, 
    long = TRUE, short = FALSE, revert = FALSE) 
{
    re <- f(date = date, prices = prices, freq = freq, holdingperiod = holdingperiod, 
        ...)
    resul <- re[[1]]
    classifica <- clas(resul)
    if (revert == T) {
        classifica <- apply(classifica, 2, rev)
    }
    rets <- retcalc(prices, classifica, percentile, holdingperiod, 
        freq, datestrategy = re$date, date)
    r <- as.vector(rets$`Vector of rend perc`)
    gra <- multi_plot(date[-(1:freq)][-1], r)
    a <- list(rets$`List of daily cumulative return per trade`, 
        r, date[-(1:freq)][-1], cumprod(r + 1), summary(r), ValueatRisk(rets$`Vector of rend perc`), 
        gra)
    names(a) <- c("Daily cumulative returns by trade", "vector of returns", 
        "Dates of returns ", "daily cumulative returns", "empirical distribution statistics", 
        "VaR", "graphics")
    return(a)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
