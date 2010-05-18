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
df2ustat(data, quant=c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99),
	  nbmin=30)
}
\examples{
 rs003k("RESET"); # needed only for R checking, to be forgotten
 set.seed(1234);
 don <- matrix(round(runif(100)*100), 20);
 dimnames(don)[[2]] <- LETTERS[1:5];
 print(df2ustat(as.data.frame(don), quant=c(0, 0.5, 1), nbmin=5));
 set.seed(1234);
 don <- matrix(letters[1+round(runif(100)*10)], 20); 
 dimnames(don)[[2]] <- LETTERS[1:5];
 print(df2ustat(as.data.frame(don), nbmin=5));
}
\arguments{
\item{data}{Date frame containing the data set. NA values are
accepted.}
\item{quant}{(\code{numeric}) The desired quantiles to be computed
for the continuous variables.}
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
