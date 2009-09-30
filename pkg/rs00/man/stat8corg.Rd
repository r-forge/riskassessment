\name{stat8corg}
\alias{stat8corg}
\title{
(f1) computes correlations between two variables
}
\description{
The two common correlation coefficients are computed for two
variables. TO BE DONE: the natures of the random variables are taken
into account to accept non valued variables.
}
\usage{
stat8corg(xy, nat)
}
\arguments{
\item{xy}{matrix with two columns, containing the two variables}
\item{nat}{the two natures of the two variables}
}
\value{
A vector of two values: (1) the correlation coefficient of Pearson
(on the values) \& (2) the correlation coefficient of Spearman (on
the ranks)
}
