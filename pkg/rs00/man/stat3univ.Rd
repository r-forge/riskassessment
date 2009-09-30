\name{stat3univ}
\alias{stat3univ}
\title{
(f1) computes univariates statistics from a data matrix
}
\description{
data is a matrix: repetition by rows, variables by columns
}
\usage{
stat3univ(data, nbmin=30)
}
\arguments{
\item{data}{Matrix with the data. NA values are possible.}
\item{nbmin}{(=30) Minimum number of observations required to compute
the statistics.}
}
\value{
a matrix having a column for each variables and different statistics
in rows. Dimnames are sufficient to understand what it is.
}
