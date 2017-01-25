\name{sewTrends}
\alias{sewTrends}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tuning systemic trends
}
\description{
If yield experience a trend the models don't grasp you have to remove it to get the lm. But some part of it may, anyway get grasped by the models... Here is the sewing
}
\usage{
#breakTrends() call this
sewTrends(inizio, fine)
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
    tempLimit <- data.frame(begin = inizio, finish = fine)
    attach(yieldPrev)
    flatOff <- merge(flatYield, relatedModel, by = "YEAR")
    yieldPrev$flatOff <- flatOff
    normalizingTrend <- function(campo) {
        yieldPrev$flatOff[, campo] <- (flatOff[, campo]/mean(flatOff[,
            campo])) * 100
    }
    lapply(X = seq(2, length(flatOff)), FUN = normalizingTrend)
    flatOff <- yieldPrev$flatOff
    yieldPrev$flatOff <- flatOff[sapply(flatOff, function(flatOff) !any(is.na(flatOff)))]
    flatOff <- yieldPrev$flatOff
    trendAN <- new.env()
    trendAN$normPlot <- ggplot(yieldPrev$flatOff) + geom_line(aes(x = YEAR,
        y = OFFICIAL_YIELD), color = "red") + geom_smooth(method = "loess",
        color = "red", aes(x = YEAR, y = OFFICIAL_YIELD), se = FALSE)
    trendPlot <- function(yvar) {
        trendAN$normPlot <- trendAN$normPlot + geom_smooth(data = yieldPrev$flatOff,
            method = "loess", color = "cyan", aes_(x = ~YEAR,
                y = as.name(yvar), label = yvar), se = FALSE)
    }
    lapply(names(yieldPrev$flatOff[, c(-1)]), FUN = trendPlot)
    trendAN$PlotNormA <- trendAN$normPlot + geom_line(aes(x = YEAR,
        y = OFFICIAL_YIELD), color = "red", size = 1.5) + geom_smooth(method = "loess",
        color = "orange", aes(x = YEAR, y = OFFICIAL_YIELD),
        se = FALSE, size = 1.5) + labs(x = "YEARS", y = "TREND") +
        geom_rect(data = tempLimit, aes(xmin = begin, xmax = finish,
            ymin = -Inf, ymax = +Inf), fill = "pink", alpha = 0.5)
    detach(yieldPrev)
    plot(trendAN$PlotNormA)
    flatOff1 <- yieldPrev$flatOff
    flatOff2 <- subset(flatOff1, flatOff1$YEAR >= inizio & flatOff1$YEAR <=
        fine)
    yieldPrev$flatOff <- flatOff2
    flatOff <- flatOff2
    yieldPrev$friendShip <- data.frame(param = as.character(names(flatOff)[(names(flatOff) ==
        "OFFICIAL_YIELD")]), trendCoef = as.numeric(lm(formula = OFFICIAL_YIELD ~
        YEAR, data = flatOff)$coefficients[2]))
    plot(trendAN$PlotNormA + stat_smooth(data = flatOff, method = "lm",
        color = "black", aes(x = YEAR, y = OFFICIAL_YIELD), fullrange = FALSE,
        se = FALSE, size = 1))
    mayTrend <- names(flatOff)[(names(flatOff) != "YEAR" & names(flatOff) !=
        "OFFICIAL_YIELD")]
    friendTest <- function(mate) {
        allIn <- yieldPrev$flatOff
        formulFriend <- as.formula(paste(as.name(mate), " ~ YEAR",
            sep = ""))
        linMod <- lm(formula = formulFriend, data = allIn)
        yieldPrev$friendShip <- rbind(yieldPrev$friendShip, data.frame(param = paste(c(as.character(mate)),
            sep = ""), trendCoef = as.numeric(linMod$coefficients[2])),
            deparse.level = 1)
    }
    sapply(X = mayTrend, FUN = friendTest, USE.NAMES = TRUE)
    trendShip <- yieldPrev$friendShip
    YieldTrend <- trendShip[1, 2]
    yieldPrev$yieldTrend <- YieldTrend
    cat(c("The found trend for Official_Yield is ", round((trendShip[1,
        2]), 2), "\%. \n The following are the similar trends available between the predictors: \n ->Absolute values \n -> Sorted by difference with Yield trend \n \n "),
        fill = TRUE)
    trendShip$trendDiff <- abs(trendShip[, 2] - trendShip[1,
        2])
    trendMates <- trendShip[c(-1), ]
    trendMates <- (trendMates[order(trendMates$trendDiff), ])
    trendMates$diff_perC <- round(trendMates$trendDiff/YieldTrend,
        2)
    trendMates$ID <- seq(1, length(trendMates[, 1]))
    print(trendMates)
    cat(c("Do you want to continue removing the trend in official yields"),
        fill = TRUE)
    continueToCut <- scan(, what = "text", nmax = 1)
    if (continueToCut == "y") {
        cat(c("Does any of the predictors explain some of the Official's Trend? \n If yes, point which one(s) by ID (multiple answers allowed) \n If none of them does, return blank: \n"),
            fill = TRUE)
        mateList <- scan(, nmax = length(trendMates[, 1]))
        yieldPrev$safeTrend <- mean(trendMates$trendCoef[mateList])
        cutTrend(inizio, fine)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
