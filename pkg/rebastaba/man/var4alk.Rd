\name{var4alk}
\alias{var4alk}
\title{
finds the variable names for an alk.
}
\description{
(ba) They can be already given, in that case they are not modified
}
\usage{
var4alk(alk, nom, nbrep)
}
\arguments{
\item{alk}{alk object}
\item{nom}{nom of the bn where the alk must inserted.}
\item{nbrep}{repetition number already found by nbrep4alk.}
}
\details{
In case of repetition, the compatibility of their number is given.
}
\value{
the names of the alk variables (without the node name)
}
