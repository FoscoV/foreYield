\name{breakTrends}
\alias{breakTrends}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
select where to break the trends and plot usefull graphs
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
breakTrends()
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
    attach(yieldPrev)
    flatPlot <- ggplot(yieldPrev$flatYield) + geom_point(color = "black",
        aes(x = YEAR, y = OFFICIAL_YIELD)) + geom_line(color = "black",
        aes(x = YEAR, y = OFFICIAL_YIELD)) + geom_smooth(method = "lm",
        color = "brown", se = FALSE, aes(x = YEAR, y = OFFICIAL_YIELD)) +
        geom_smooth(method = "loess", color = "red", se = FALSE,
            aes(x = YEAR, y = OFFICIAL_YIELD)) + geom_smooth(method = "lm",
        formula = y ~ splines::bs(x, 3), color = "orange", se = FALSE,
        aes(x = YEAR, y = OFFICIAL_YIELD))
    if (any(names(yieldPrev) == "breakPoint"))
        flatPlot <- flatPlot + geom_rect(data = yieldPrev$breakPoint,
            aes(xmin = begin, xmax = finish, ymin = -Inf, ymax = +Inf),
            fill = "yellow", alpha = 0.3)
    plot(flatPlot)
    cat(c("Time series starts in", unique(min(flatYield$YEAR)),
        "and ends in ", unique(max(flatYield$YEAR))), fill = TRUE)
    cat(c("Point the trend's edges by year\n"), fill = TRUE)
    trendEdge <- scan(, nmax = 2)
    tempLimit <- data.frame(begin = min(trendEdge), finish = max(trendEdge))
    cutPlot <- flatPlot + geom_rect(data = tempLimit, aes(xmin = begin,
        xmax = finish, ymin = -Inf, ymax = +Inf), fill = "pink",
        alpha = 0.5)
    plot(cutPlot)
    sewTrends(min(trendEdge), max(trendEdge))
    detach(yieldPrev)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
