\name{virgilio}
\alias{virgilio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{guide you all along
}
\description{
It just provides a supervised path across the story. If you want to use other function step by step and not this one, the results are the same.
}
\usage{
virgilio()
}
%- maybe also 'usage' for other objects documented here.
\details{
Actually it doesn't do anything, but sort the other function (it doesn't save nor load) from earlier to later stages of the proceedings.
It's something similar tto a scout or a tutor managing that everything works fine (NB: that's the aim).
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
No argument are supposed when calling the function
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
configure()
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function ()
{
    if (any(ls() == "yieldPrev")) {
    }
    else {
        yieldPrev <- new.env()
        yieldPrev$.conflicts.OK <- c()
    }
    configure()
    checkTrends()
    while (yieldPrev$flattyn == "y") {
        breakTrends()
        checkTrends()
    }
    modSel()
    responseYield()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
