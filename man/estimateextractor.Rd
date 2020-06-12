\name{estimateextractor}
\alias{estimateextractor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
estimateextractor(x, holdingperiod, freq)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{holdingperiod}{
%%     ~~Describe \code{holdingperiod} here~~
}
  \item{freq}{
%%     ~~Describe \code{freq} here~~
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
function (x, holdingperiod, freq) 
{
    n <- ifelse(test = (is.matrix(x) == T), yes = nrow(x), length(x))
    se <- seq(1:(ceiling(n/(holdingperiod * freq)) - 1)) * holdingperiod * 
        freq + freq
    if ((n > se[length(se)])) {
        n <- se[length(se)]
    }
    if (is.matrix(x) == T) {
        quanto <- ifelse(test = (length(se) > 1), yes = (length(se) - 
            1), 1)
        a <- list(x[1:freq, ])
        for (i in 1:quanto) {
            a[[i + 1]] <- x[(se[i] - freq + 1):(se[i]), ]
        }
        a[[length(se) + 1]] <- x[(se[length(se)] - freq + 1):n, 
            ]
    }
    else {
        a <- list(x[1:freq])
        for (i in 1:length(se)) {
            a[[i + 1]] <- x[(se[i] - freq + 1):(se[i])]
        }
        a[[length(se) + 1]] <- x[(se[length(se)] - freq + 1):n]
    }
    return(a)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
