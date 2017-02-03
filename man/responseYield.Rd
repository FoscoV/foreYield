\name{responseYield}
\alias{responseYield}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Some adjustment and mainly prints the results
}
\description{
At this stage, the work is done and the results are printed.
You can read information about the prediction, the Cross Validation results (plotted in detail).
At this stage a PCA regression is processed (by itself) and it's results are printed.

So that you have  for comparison:prediction,CV,PCR,TimeSeriesForecast.

The results are based on the data as the came out of the un-trending process so that have to be added for the trend effects. This is done (explicitly) for the predicted one only. All the others have to summed to the current trend by hand (if you prefer them to your own result after the comparison)
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
