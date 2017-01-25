\name{cutTrend}
\alias{cutTrend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Actually do the cutting of trends, not simply setting it
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cutTrend(inizio, fine)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inizio}{
%%     ~~Describe \code{inizio} here~~
}
  \item{fine}{
%%     ~~Describe \code{fine} here~~
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
function (inizio, fine)
{
    notSoFlat <- yieldPrev$flatYield
    preflat <- subset(notSoFlat, notSoFlat$YEAR >= inizio & notSoFlat$YEAR <=
        fine)
    flatLin <- lm(OFFICIAL_YIELD ~ YEAR, data = preflat)
    cutEnv <- new.env()
    cutEnv$modello <- flatLin
    cutEnv$flatting <- preflat
    if (any(names(yieldPrev) == "safeTrend)")) {
        cutEnv$trendCorr <- (flatLin$coefficients[2] - yieldPrev$safeTrend)
    }
    else {
        cutEnv$trendCorr <- flatLin$coefficients[2]
    }
    smootherer <- function(num) {
        model <- cutEnv$modello
        flat <- (model$model[num, 1]) - (model$model[num, 2] -
            model$model[1, 2]) * cutEnv$trendCorr
        cutEnv$flatting[num, 2] <- flat
    }
    lapply(X = seq(1, length(preflat$OFFICIAL_YIELD)), FUN = smootherer)
    preflat <- cutEnv$flatting
    if (any(names(yieldPrev) == "breakPoint"))
        yieldPrev$breakPoint <- rbind(yieldPrev$breakPoint, c(inizio,
            fine, as.numeric(flatLin$coefficients[2])))
    else yieldPrev$breakPoint <- data.frame(begin = inizio, finish = fine,
        trend = as.numeric(flatLin$coefficients[2]))
    postFlat <- subset(notSoFlat, notSoFlat$YEAR < inizio | notSoFlat$YEAR >
        fine)
    flatFlat <- rbind(preflat, postFlat)
    yieldPrev$flatYield <- flatFlat[order(flatFlat$YEAR), ]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
