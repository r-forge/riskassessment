\name{df2bn}
\alias{df2bn}
\title{
initializes a bn object from a data frame
}
\description{
(bn) just creating a bn without any link and constant distribution
numbering the nodes from the variables of the data frame.\cr When a
variable is a factor, then the distribution is taken as categoric
with uniform distribution.
}
\usage{
df2bn(df, description=new("des", name="created.by.df2bn"))
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 print(df2bn(data.frame(A=1:3, B=LETTERS[1:3], C=11:13)));
}
\arguments{
\item{df}{the data frame}
\item{description}{ the descripion to give to the bn.}
}
\value{
a bn object
}
