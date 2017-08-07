\name{bartolomeoMod}
\alias{bartolomeoMod}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{wrapper for bartMachine
}
\description{
Supervisor of Bayesian Additive Regression Tree for yield previsions
}
\usage{
bartolomeoMod(cpus,depthing=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\details{
\itemize{
\item cpus	bartMachine which handles the computations allow for heavy parallelization. You can here provide your number of cpus to speed up the processing.
\item depthing	on default doeas the CrossValidated version (what you are probably looking for: MY SUGGESTION IS NOT TO DO ANYTHING)
}
Please note that due to its own structure this function will work after the successful fullfillment of the data structure and collection granted by virgilio's guide.
}
\value{
bartolomeoMod return a printed text and nothing else
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Adam Kapelner, Justin Bleich (2016). bartMachine: Machine Learning
  with Bayesian Additive Regression Trees. Journal of Statistical
  Software, 70(4), 1-40. doi:10.18637/jss.v070.i04
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
#to run the cross validated fit of the BART model on a quad(4) core cpus:
bartolomeoMod(4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
