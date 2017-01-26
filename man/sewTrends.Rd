\name{sewTrends}
\alias{sewTrends}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tuning systemic trends
}
\description{
If yield experience a trend the models don't grasp you have to remove it to get the right lm. But some part of it may, anyway get explained by the models... Here is the sewing
it provides graphs to understand if at least part of the trend you find with checkTrends() is external to simulated parameters.
Prompts interactively for the choice of the best parameters describing the trend in the time lapse you selected (from inizio to fine). It also plot a graph with the parameters and yields "normalized" so that matching the trends is easier.
Normalization happens around the mean each parameter given in percentage.
}
\usage{
#breakTrends() call this
sewTrends(inizio, fine)
%- maybe also 'usage' for other objects documented here.
}
\arguments{
  \item{inizio}{
 is the lower year (included) of the trended lapse
}
  \item{fine}{
 is the upper year (included) of the trended lapse
}
}
\details{
checkTrends() call include both "inizio" and "fine",
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
#Designed to be called by breakTrends() (which itself is called by checkTrends())


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
