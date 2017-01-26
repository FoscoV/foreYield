\name{cutTrend}
\alias{cutTrend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Actually do the cutting of trends, not simply setting it
}
\description{
Two input parameters: It receives the edges to cut at.

}
\usage{
#called by sewTrends()
cutTrend(inizio, fine)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inizio}{
 is the lower year (included) of the trended lapse
}
  \item{fine}{
 is the upper year (included) of the trended lapse
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
The lower year included in the trend comes out as it get in.
Considering that the trand is, at that time, experiencing the year of its start no changes actually occur.


Mathematically, the trend is accounted to be linear, and so described by the following:

$yield_{n~unTrended}= yield_{n~Offical} - ( year_{n} - year_{startTrend} ) * Trend_{coefficient}  $

So that is clear that for n = startTrend (aka "inizio") it is that the trend and the unTrended versions of yield are the same.
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
