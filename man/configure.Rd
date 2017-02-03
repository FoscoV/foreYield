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
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{depth}{
%%     ~~Describe \code{depth} here~~
}
}
\details{
In normal mode, you ha only to follow the screen instruction.
Here are supported two database structures.

Wide table split in two files: an official one and a simulated one.

Long table where all the data are stored in one single file.

More combination of Long-Wide and single-two(-more?) may get  supported in future releases.

Please note that some column name are required (and case sensitive):
YEAR,DECADE,NUTS_CODE.

OFFICIAL_YIELD can be coerced to OFFICIAL_YIELD from an amount of writings (cases, underscore alternatives).

It supports only the .csv input format, please note that it get rid of several kind of csv-like standards. Anyway it will fail if , (comma) is used as decimal separator; I strongly suggest to adopt the standard . (dot) .
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
