\name{checkTrends}
\alias{checkTrends}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot usefull graph to check the existance of trends
}
\description{
Moreover it asks for cutting or no. In virgilio there is no other way to stop cutting than answer something ese than "y"
}
\usage{
checkTrends()
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
    if (any(names(yieldPrev) == "flatYield")) {
        cat(c("Official yields have a ", (adf.test(flatYield$OFFICIAL_YIELD)$p.value) *
            100, "\% of chances to have a trend."), fill = TRUE)
    }
    else {
        cat(c("Official yields have a ", (adf.test(actualYield$OFFICIAL_YIELD)$p.value) *
            100, "\% of chances to have a trend."), fill = TRUE)
    }
    cat(c("Look the chart for the visual assessment. \n Plotted: \n BLACK:actual data \n BROWN:linear model \n RED:LOcal regrESSion \n ORANGE:spline regression"),
        fill = TRUE)
    if (any(names(yieldPrev) == "flatYield")) {
        offiPlot <- ggplot(flatYield, aes(x = YEAR, y = OFFICIAL_YIELD)) +
            geom_point(color = "black") + geom_line(color = "black") +
            geom_smooth(method = "lm", color = "brown", se = FALSE) +
            geom_smooth(method = "loess", color = "red", se = FALSE) +
            geom_smooth(method = "lm", formula = y ~ splines::bs(x,
                3), color = "orange", se = FALSE)
    }
    else {
        offiPlot <- ggplot(actualYield, aes(x = YEAR, y = OFFICIAL_YIELD)) +
            geom_point(color = "black") + geom_line(color = "black") +
            geom_smooth(method = "lm", color = "brown", se = FALSE) +
            geom_smooth(method = "loess", color = "red", se = FALSE) +
            geom_smooth(method = "lm", formula = y ~ splines::bs(x,
                3), color = "orange", se = FALSE)
    }
    plot(offiPlot)
    cat(c("Do you see a trend in the data?\n\ty \n remove trend \n \n\tn \n "),
        fill = TRUE)
    yieldPrev$flattyn <- scan(, what = "text", nmax = 1)
    if (any(names(yieldPrev) == "flatYield")) {
    }
    else {
        flatYield <- actualYield
        yieldPrev$flatYield <- actualYield
    }
    if (length(c(setdiff(seq(min(flatYield$YEAR), max(flatYield$YEAR)),
        flatYield$YEAR), flatYield$YEAR[duplicated(flatYield$YEAR)])) ==
        0)
        cat("")
    else cat(c("By the way there are \n MISSING:", setdiff(seq(min(flatYield$YEAR),
        max(flatYield$YEAR)), flatYield$YEAR), " \n REPLICATED: ",
        flatYield$YEAR[duplicated(flatYield$YEAR)]))
    detach(yieldPrev)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
