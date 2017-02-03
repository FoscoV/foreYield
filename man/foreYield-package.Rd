\name{foreYield-package}
\alias{foreYield-package}
\alias{foreYield}
\docType{package}
\title{foreYield
\packageTitle{foreYield}
}
\description{
\packageDescription{foreYield}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{foreYield}
\packageIndices{foreYield}
The packages is designed to easily apply at forecasting yield. It only requires a couple of data as input: previous yields (Officials) and predictors coming from SIMULATIONs.
virgilio() provides a textual user interface (cli) so that no more knowledge about R is required (but to remember the brackets in "virgilio()").

It's based on other packages automatically installed and loaded as dependencies.




By default (using configure(), which is the  same virgilio() does) those are assumed to come from .csv files but you can provide them by yourself having care them to be homologous to the supposed data.frame structure. If not screening the sources, yuo can check the required structure in configure().

}
\author{
\packageAuthor{foreYield}

Maintainer: \packageMaintainer{foreYield}
}
\references{
~~ Literature or other references for background information ~~
}
\keyword{ package }
\seealso{
}
\examples{
#This should allow you to just follow the instructions on screen
virgilio()
}
