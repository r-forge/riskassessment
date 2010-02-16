\name{df2ustat}
\alias{df2ustat}
\title{
(00) computes univariate statistics from a data frame
}
\description{
Data being supposed to be a data frame: observation by rows,
variables by columns being numeric or factor; standard univariate
statistics are computed for each variable.
}
\usage{
df2ustat(data, nbmin=30)
}
\examples{
 rs003k("RESET"); # needed only for R checking, to be forgotten
 set.seed(1234);
 don <- matrix(round(runif(100)*100), 20);
 dimnames(don)[[2]] <- LETTERS[1:5];
 print(df2ustat(as.data.frame(don), 5));
 set.seed(1234);
 don <- matrix(letters[1+round(runif(100)*10)], 20); 
 dimnames(don)[[2]] <- LETTERS[1:5];
 print(df2ustat(as.data.frame(don), 5));
}
\arguments{
\item{data}{Date frame containing the data set. NA values are
accepted.}
\item{nbmin}{(\code{numeric(1)}) Minimum number of observations
required to compute the statistics for each variable.}
}
\value{
a list with as many components as variables (getting the variable
names). For numeric variables it is a named vector with standard
statistics. For categoric variables it is a matrix having a column
for each category sorted according to the frequencies and four rows:
frequencies, rounded percentages, rounded cumulated percentages in
both directions.
}
