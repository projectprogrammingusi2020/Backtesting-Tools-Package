\name{fmomentum}
\alias{fmomentum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fmomentum(date, prices, freq, holdingperiod)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{date}{
%%     ~~Describe \code{date} here~~
}
  \item{prices}{
%%     ~~Describe \code{prices} here~~
}
  \item{freq}{
%%     ~~Describe \code{freq} here~~
}
  \item{holdingperiod}{
%%     ~~Describe \code{holdingperiod} here~~
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
function (date, prices, freq, holdingperiod) 
{
    logret <- f.rendlog(prices)
    r <- estimateextractor(logret, holdingperiod, freq)
    d <- estimateextractor(date[-1], holdingperiod, freq)
    da <- d[[1]][length(d[[1]])]
    cumret <- vectocolmtxf(fu, r[[1]], freq)
    for (i in 2:length(d)) {
        da <- c(da, d[[i]][length(d[[i]])])
        cumret <- rbind(cumret, vectocolmtxf(fu, r[[i]], freq))
    }
    l <- list(cumret, da)
    names(l) <- c("mtx", "date")
    return(l)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
