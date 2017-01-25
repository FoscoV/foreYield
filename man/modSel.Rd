\name{modSel}
\alias{modSel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interactive choice of the model you like more
}
\description{
the main statistical stuff is here
}
\usage{
modSel()
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
    tableXregression <- merge(yieldPrev$flatYield, yieldPrev$relatedModel,
        by = "YEAR")
    allSign <- regsubsets(OFFICIAL_YIELD ~ .^2 + ., data = tableXregression[,
        c(-1)], nbest = 2, method = "exhaustive", nvmax = 4,
        really.big = TRUE)
    summaSign <- summaryHH(allSign, names = seq(1, length(allSign$xnames)),
        statistics = "adjr2")
    plot(summaSign, col = "green", cex = 0.8)
    print(summaSign)
    cat("Note: one of the accounted parameter is (Intercept) \n \n Select a model")
    modId <- scan(, nmax = 1)
    yieldPrev$model_formula <- c("OFFICIAL_YIELD ~ ")
    compleFormula <- function(parametro) {
        yieldPrev$model_formula <- paste(yieldPrev$model_formula,
            names(coef(allSign, id = modId))[parametro], if (parametro ==
                length(names(coef(allSign, id = modId))))
                sep = " "
            else sep = " +")
    }
    lapply(X = seq(2, length(names(coef(allSign, id = modId)))),
        FUN = compleFormula)
    regrSW <- lm(as.formula(yieldPrev$model_formula), data = tableXregression)
    print(regrSW)
    relatedModel <- yieldPrev$relatedModel
    expYield <- predict(regrSW, newdata = subset(relatedModel,
        relatedModel$YEAR == yieldPrev$currentYear), se.fit = TRUE,
        type = "response", level = 0.95, interval = "prediction")
    yieldPrev$expYield <- expYield
    yieldPrev$modelLM <- regrSW
    yieldPrev$tableXregression <- tableXregression
    attach(yieldPrev)
    validC <- CV(yieldPrev$modelLM)
    detach(yieldPrev)
    yieldPrev$CVmsRes <- c(validC[1], validC[5])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
