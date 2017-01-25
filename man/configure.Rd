\name{configure}
\alias{configure}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
import all the data needed
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
configure()
#never tested:
configure(depth=advanced)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{depth}{
%%     ~~Describe \code{depth} here~~
}
}
\details{
In normal mode, you ha only to follow the screen instruction. If you are trying the advanced version, it is supposed to allow importing dataset different than the ones used since now. Not yet tested!
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
function (depth = "base")
{
    cat("If you created your csv databases using MS Office, write <1> here, else <0>",
        fill = TRUE)
    msoffice <- scan(, nmax = 1)
    cat("Provide OFFICIAL yield database", fill = TRUE)
    if (msoffice == 1)
        eurostat <- read.table(file.choose(), header = T, sep = ";")
    else eurostat <- read.csv(file.choose())
    if (depth == "advanced" && depth == "adv-offi") {
        freakoffi <- eurostat
        cat(c("Here are the columns in your file. Which one represents the crop code? \n ",
            names(freakoffi), " \n Write it here: \n "), fill = TRUE)
        scn <- scan(, what = "text", nmax = 1)
        cat(c("Here are the columns in your file. Which one represents the Nation (is going to be drop)? \n ",
            names(freakoffi), " \n Write it here: \n "), fill = TRUE)
        nut <- scan(, what = "text", nmax = 1)
        cat(c("Here are the columns in your file. Which one represents the Years? \n ",
            names(freakoffi), " \n Write it here: \n "), fill = TRUE)
        epoch <- scan(, what = "text", nmax = 1)
        cat(c("Here are the columns in your file. Which one represents the Official Yield? \n ",
            names(freakoffi), " \n Write it here: \n "), fill = TRUE)
        harv <- scan(, what = "text", nmax = 1)
        eurostat$STAT_CROP_NO <- freakoffi$as.name(scn)
        eurostat$NUTS_CODE <- freakoffi$as.name(nut)
        eurostat$YEAR <- freakoffi$as.name(epoch)
        eurostat$OFFICIAL_YIELD <- freakoffi$as.name(harv)
    }
    cat("Provide SIMULATE yield database", fill = TRUE)
    if (msoffice == 1)
        prev <- read.table(file.choose(), header = T, sep = ";")
    else prev <- read.csv(file.choose())
    if (depth == "advanced" && depth == "adv-simul") {
        freakprev <- prev
        cat(c("Here are the columns in your file. Which one represents the crop code? \n ",
            names(freakprev), " \n Write it here: \n "), fill = TRUE)
        scn <- scan(, what = "text", nmax = 1)
        cat(c("Here are the columns in your file. Which one represents the Nation (is going to be drop)? \n ",
            names(freakprev), " \n Write it here: \n "), fill = TRUE)
        nut <- scan(, what = "text", nmax = 1)
        cat(c("Here are the columns in your file. Which one represents the Years? \n ",
            names(freakprev), " \n Write it here: \n "), fill = TRUE)
        epoch <- scan(, what = "text", nmax = 1)
        cat(c("Here are the columns in your file. Which one represents the decade (phase of the year, if you like: it doesn't matter)? \n ",
            names(freakprev), " \n Write it here: \n "), fill = TRUE)
        decad <- scan(, what = "text", nmax = 1)
        prev$CROP_NO <- freakprev$as.name(scn)
        prev$NUTS_CODE <- freakpre$as.name(nut)
        prev$YEAR <- freakoffi$pre(epoch)
        prev$DECADE <- freakoffi$as.name(decad)
    }
    cat(c("OFFICIAL yield data contains information for the following CROPs:\n",
        unique(eurostat$STAT_CROP_NO), "\n Choose one:", fill = TRUE))
    cropO <- scan(, nmax = 1)
    cat(c("SIMULATED yield data contains information for the following CROPs:\n",
        unique(prev$CROP_NO), "\n Choose one:", fill = TRUE))
    cropS <- scan(, nmax = 1)
    cat(c("Following countries are provided by the DataBases:\n",
        "OFFICIAL:", levels(eurostat$NUTS_CODE), "\n SIMULATION:",
        levels(prev$NUTS_CODE), "(case sensitive\n"), fill = TRUE)
    cat(" OFFICIAL COUNTRY:")
    countryO <- scan(, what = "text", nmax = 1)
    cat("\n SIMULATION COUNTRY")
    countryS <- scan(, what = "text", nmax = 1)
    actualYield <- subset(eurostat, eurostat$STAT_CROP_NO ==
        cropO & eurostat$NUTS_CODE == countryO)[, c(which(names(eurostat) ==
        "YEAR"), which(names(eurostat) == "OFFICIAL_YIELD"))]
    yieldPrev$actualYield <- actualYield[order(actualYield$YEAR),
        ]
    relatedModel <- subset(prev, prev$CROP_NO == cropS & prev$NUTS_CODE ==
        countryS)
    currentYear <- max(unique(relatedModel$YEAR))
    yieldPrev$currentYear <- currentYear
    currentDecade <- max(subset(relatedModel, relatedModel$YEAR ==
        currentYear)$DECADE)
    cat(c("It seems forecasting the year", currentYear, "with data till the ",
        currentDecade, "th decade"), fill = TRUE)
    yieldPrev$relatedModel <- subset(relatedModel, relatedModel$DECADE ==
        currentDecade)[, c(-which(names(prev) == "CROP_NO"),
        -which(names(prev) == "DECADE"), -which(names(prev) ==
            "NUTS_CODE"))]
    yieldPrev$saveCrop <- cropS
    yieldPrev$saveCountry <- countryS
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
