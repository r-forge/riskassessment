\name{form3names}
\alias{form3names}
\title{
provides systematic names for nodes
}
\description{
Provides systematic names for nodes according the number of nodes
taking care of not duplicating former nodes.
}
\usage{
form3names(nbn, nom=character(0), prefix="", upca=TRUE, nume=14)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 form3names(2);
 form3names(2, nume=-3);
 form3names(2, prefix="rbsb.");
 form3names(2, upca=FALSE);
 form3names(2, "Z");
 form3names(30);
}
\arguments{
\item{nbn}{Number of new node names to generate}
\item{nom}{ Already present names (to avoid identical names).}
\item{prefix}{ Systematic prefix of all names to generate. Must
comprise the dot, if one wants such a separator between it and the
specific part of the name. Of course can be 'underscore' or whatever
else.}
\item{upca}{ Indicates whether the letters constituting the new names
must be uppercase or not.}
\item{nume}{ Its absolute value gives the number of the letter to use
when Letters are not sufficient. When negative, alphabet is not
considered as a first possibility.}
}
\details{
(see the code)
}
\value{
vector with nbn different strings associated to new names
}
\keyword{utilities}
