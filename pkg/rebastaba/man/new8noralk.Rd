\name{new8noralk}
\alias{new8noralk}
\title{
very simplified version of new8alk
}
\description{
(ba) In this simplified version of new8alk, the user has no more than
giving the name and the parents of the node and the regression
coefficients. The distribution will be automatically put to 'normal'
and the parameter 'mu' to the linear combinations of the parents.
}
\usage{
new8noralk(des, parent=character(0), coef=0:length(parent), sigma=1)
}
\examples{
 rebastaba3k("RESET"); # needed only for R checking, to be forgotten
 print(new8noralk(char2des("some /alk/"), parent=c("A", "AA")));
}
\arguments{
\item{des}{either the name or a description ('ds' object).}
\item{parent}{(\code{character(0)}) Character vector with the names
of the parents. \code{character(0)} means that there is no parents.}
\item{coef}{ coefficient to define mu. If there are two parents, then
mu will be
'coef[1]+coef[2]*\{\{parent[1]\}\}+coef[3]*\{\{parent[2]\}\}'.}
\item{sigma}{ standard deviation.}
}
\details{
length(coef) must equals 1 + length(parent); sigma must be numeric.
}
\value{
an object of class "bn" with an additional node
}
