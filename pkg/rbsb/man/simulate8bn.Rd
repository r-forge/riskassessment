\name{simulate8bn}
\alias{simulate8bn}
\title{
(dn) simulates a /dn/ from a /bn/
}
\description{
returns a matrix having simu rows and as many columns as variables in
the bn, using the joint probability distribution implicitely defined
by the bn. \cr Some of the columns can be already computed, in that
case there are skipped from the general loop. This applies only for
the root nodes to save the consistency with the /bn/ structure.
}
\usage{
simulate8bn(bn, simu=100)
}
\examples{
 rsdn3k("RESET"); # For R convenience when checking
 simulate8bn(rbsb.bn1, 10);
 simulate8bn(rbsb.bn2, 10);
 simulate8bn(rbsb.bn3, 10);
 simulate8bn(rbsb.bn4, 10);
 simulate8bn(rbsb.bn5, 10);
}
\arguments{
\item{bn}{the bn to simulate}
\item{simu}{(=100) either (1) a numeric indicating the desired number
of simulations or (2) a data.frame whose the number of rows indicates
the desired number of simulations, and where columns can comprises
already computed nodes.\cr Already computed nodes are accepted when
they are root node for the bn. If there are not root, they are
not.\cr Notice that extra columns not being a node can be
incorporated, they will be returned as they are.\cr To better tackle
the 'categ' variables, all results coming back from the call of a
generating function (associated to any node to simulate) are
considered to be a data frame. If this is not the case, they are
previously transformed in a data frame. Nevertheless the names of the
variable are always imposed because some of the generating functions
cannot do it.}
}
\details{
A compatible order with the definition of the conditional
probabilities is first computed by porder, then nodes are sucessively
generated according to this order using the generating functions
stored into @rlks (except for some specific cases like 'empidata',
'numcat'...\cr BE AWARE that if a multivariate root node is included
in the matrix simu, then ALL variables must be included and the fact
is not checked. \cr Notice that the function relies on the column
names of this matrix and also that the control of existing variables
in case of multivariate nodes is not made a priori but at the moment
of their use (then possibly generating an error message at an
unexpected point for the user).
}
\value{
the resulting data frame with values outside corresponding 'lpod's
put to NA)
}
