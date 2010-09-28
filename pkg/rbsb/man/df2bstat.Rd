\name{df2bstat}
\alias{df2bstat}
\title{
computes bivariate statistics from a data frame
}
\description{
Data being supposed to be a data frame: observation by rows,
variables by columns being numeric or factor; standard bivariate
statistics are computed for each couple of variables.
}
\usage{
df2bstat(data, nbmin=80)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 set.seed(1234);
 don <- as.data.frame(matrix(round(runif(100)*10), 20));
 names(don) <- LETTERS[1:5];
 don[[4]] <- as.factor(don[[4]]);
 don[[5]] <- as.factor(don[[5]]);
 print(df2bstat(don, 5));
}
\arguments{
\item{data}{Date frame containing the data set. NA values are
accepted.}
\item{nbmin}{(\code{numeric(1)}) Minimum number of observations
required to compute the statistics for each variable.}
}
\value{
a symmetric matrix having rows and columns associated to the
variables. When the couple is numeric/numeric: the Pearson
correlation; categoric/categoric: the first eigen values of
\{n\_ij/(n\_i+.n\_+j)\}; numeric/categoric: the Pearson correlation
giving to the categoric the values of mean of the numeric.
}
