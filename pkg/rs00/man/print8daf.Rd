\name{print8daf}
\alias{print8daf}
\title{
(00) prints a daf object
}
\description{
This function prints in a daf object.
}
\usage{
print8daf(x, whi=1:10, quoi="nr")
}
\examples{
 rs003k("RESET"); # needed only for R checking, to be forgotten
 print(new("daf", des=new("des", name="rbsb", 
 orig=paste("Created by rebastaba"), 
 time=date(), 
 defi="prototype", 
 role=character(0), 
 comm=character(0)), 
 what="d", 
 valu="rbsb.dfr0"));
}
\arguments{
\item{x}{The daf object.}
\item{quoi}{(="nr"), which part of the description to print (see
print.ds).}
\item{whi}{(=1:10), which rows to print?\cr when -1: none is
printed,\cr when 0: all are printed,\cr when a vector: indicates the
numbers of rows to print (default = the first ten),\cr when a value:
the percentage to be printed (50 = half of the rows) }
}
\value{
returns nothing but a printing is performed
}
\keyword{print}
