\name{draw8edcg}
\alias{draw8edcg}
\title{
simulates random draws for a numcat distribution
}
\description{
(dn) This function simulates a categoric distribution. Similar to the
multinomial distribution but the size parameter is one and it is
considered as a univariate distribution. Values are the numbers of
drawn [\code{lev}] categories. If necessary this function could be
replaced by some C code... Possible conditional distribution are
introduced if !is.null(parents) using their order and unex
convention. See the description provided in the function
\code{help8ltype} for \code{numcat}.
}
\usage{
draw8edcg(X, proba, lev=NULL, parents=NULL, parentslength=NULL)
}
\examples{
 rebastaba3k("RESET"); # For R checking
 X <- matrix(NA, 12, 0);
 proba <- matrix(1:5, 1);
 draw8edcg(X, proba);
 UU <- sample(LETTERS[1:4], 20, TRUE);
 X <- as.data.frame(UU);
 proba <- matrix(1:12, 4);
 draw8edcg(X, proba);
 draw8edcg(X, proba, lev=letters[1:12]);
}
\arguments{
\item{X}{already generated parents. If no one, must be a matrix nx0
to indicate the number of draws to perform.}
\item{proba}{Matrix [p,k] of the probabilities of the multinomials.
When there are no parents, p=1 because the distribution is the same
for all the draws. When there are parents, it is the product of the
numbers of categories of the parents. By the way, the parents must be
of nature \code{categ}.}
\item{lev}{ When null a distribution is drawn with values from
\code{1} to \code{ncol(proba)}. When numeric transformed
\code{as.character(round(lev))}. When character, a categoric
distributions is drawn with the categories given by \code{lev}, a
character vector. (If not null the length of lev must be equal to the
column number of the \code{proba} matrix.)}
\item{parents}{ When null no parents. If not, the parent names as
variables names if any. Be careful that their order is significant
for the interpretation of the proba matrix.}
\item{parentslength}{ Only when there are parents. Then The vector of
lengths of levels of the parents. This cannot be deduced from the
simulated values because some of the levels can be missing.}
}
\value{
a vector [n] of the draws is returned (it is a factor).
}
