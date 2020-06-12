\name{multi_plot}
\alias{multi_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
multi_plot(zdate, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{zdate}{
%%     ~~Describe \code{zdate} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
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
function (zdate, k) 
{
    w <- rep(NA, length(k))
    w[which((k > 0) == T)] <- "Positive"
    w[which(is.na(w) == T)] <- "Negative"
    df <- data.frame(Date = zdate, Cumulative_Return = cumprod(k + 
        1), Ret = k, Returns = w)
    y <- ggplot(df, aes(Date, Cumulative_Return)) + geom_line() + 
        ylab("Cumulative Return per unit invested")
    z <- ggplot(df, aes(k, fill = Returns)) + geom_histogram(bins = ceiling(nrow(df))/10) + 
        xlab("Returns") + ggtitle("Empirical distribution of returns") + 
        scale_color_identity()
    l <- list(y, z)
    names(l) <- c("graph of cumulative returns per unit invested", 
        "empirical distribution of returns")
    return(l)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
