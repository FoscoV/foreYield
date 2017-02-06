\name{valiTrend}
\alias{valiTrend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Some statistical checks on the printed results
}
\description{
It performs two validation. That the data provided for the regression are free from noticeable trends (which is very likely to be). Mainly it checks that the trend marked doesn't cause the data to have a worse fitting (MSE) than without any treatment. If it were such, then it notify this and prompts for a reset of Trends as marked previously.
}
\usage{
valiTrend()
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
    train <- subset(yieldPrev$due2trend, yieldPrev$due2trend$trended ==
        0)
    train <- merge(train, yieldPrev$actualYield, by = "YEAR")
    train$trended <- NULL
    train <- merge(train, yieldPrev$relatedModel, by = "YEAR")
    noTrendMod <- lm(as.formula(yieldPrev$model_formula), data = train)
    test <- subset(yieldPrev$due2trend, yieldPrev$due2trend$trended !=
        0)
    test <- merge(test, yieldPrev$relatedModel, by = "YEAR")
    test$trended <- NULL
    solution <- subset(yieldPrev$due2trend, yieldPrev$due2trend$trended !=
        0)
    solution <- merge(solution, yieldPrev$flatYield, by = "YEAR")
    solution$trended <- NULL
    woTrend <- predict(noTrendMod, newdata = test, se.fit = TRUE,
        type = "response", level = 0.95, interval = "prediction")
    predCfg <- merge(solution, yieldPrev$omniYield, by = "YEAR")
    predCfg$clean <- woTrend$fit[, 1]
    DnoTREND <- predCfg$YIELD - predCfg$pred
    DwTREND <- predCfg$OFFICIAL_YIELD - predCfg$clean
    sigNO <- sqrt(mean((DnoTREND - mean(DnoTREND))^2))
    sigW <- sqrt(mean((DwTREND - mean(DwTREND))^2))
    if (sigNO < sigW) {
        cat(c("NOTE that the pointed Trends are afflicted by some kind of problem, a BETTER FITting model can be obtained WITHout any TREND removal. \n"))
    }
    asimErr <- skewness(woTrend$fit[, 1] - solution$OFFICIAL_YIELD)
    predError <- woTrend$fit[, 1] - solution$OFFICIAL_YIELD
    sigma <- sqrt(mean((predError - mean(predError))^2))
    danger <- asimErr/sigma
    if (danger >= 2.6) {
        cat(c("\n ADVICE: \n The marked trend related dynamics don't fit with the data! \n "))
    }
    if (sigNO < sigW | danger >= 2.6) {
        cat(c("Do you want to reset the trend marked and proceed again? (y/n) \n"),
            fill = T)
        reBea <- scan(, what = "text", nmax = 1)
        while (reBea != "y" & reBea != "n") {
            cat("answer y or n")
            reBea <- scan(, what = "text", nmax = 1)
        }
        if (reBea == "y") {
            rm(list = c("breakPoint", "flatYield", "due2trend",
                "friendShip", "flattyn", "safeTrend", "yieldTrend",
                "flatOff", "tableXregression", "model_formula",
                "CVmsRes", "expYield", "omniYield", "modelLM",
                "PCmodel"), envir = yieldPrev)
            virgilio()
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
