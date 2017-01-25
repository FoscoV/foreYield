\name{responseYield}
\alias{responseYield}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Some adjustment and mainly prints the results
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
responseYield()
}
%- maybe also 'usage' for other objects documented here.
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
function ()
{
    expYield <- yieldPrev$expYield
    knoTime <- yieldPrev$breakPoint
    trendMissing <- (knoTime$finish - knoTime$begin) * knoTime$trend
    cat(c(" \n \n \n RESPONSE \n \n ", "As it is, the forecasted yield for year",
        yieldPrev$currentYear, "is", round(expYield$fit[1], 2),
        "+/-", round(expYield$fit[1] - expYield$fit[2], 2), "."),
        fill = TRUE)
    cat(c("Confidence = 95\% \n\n\t\n \n CROSS-VALIDATION returned ",
        round(yieldPrev$CVmsRes[1], 2), "as mean square error\n\n \n\nDue to the marked trends, the forecasted has, ",
        round(trendMissing, 2), " resulting in ", round(expYield$fit[1] +
            trendMissing, 2), ". \n\n\n\t\n \n\n\tTimeSeries statistical analysis over OFFICIAL_YIELD would bet on ",
        round(forecast(ets(yieldPrev$actualYield[, 2]), h = 1)$mean[1],
            2), " +/- ", round((forecast(ets(yieldPrev$actualYield[,
            2]), h = 1)$upper[2] - forecast(ets(yieldPrev$actualYield[,
            2]), h = 1)$mean[1]), 2)), fill = TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
