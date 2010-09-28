\name{nv2ion}
\alias{nv2ion}
\title{
returns the /ion/ of a series of nodes/variables
}
\description{
(ba) This is a central, basic and very general function for
programming with /rebastaba/, so not that easy to grasp. It is
suggested to read the detail section.\cr From a series of nodes (or
variables) indicated with 'x' returns their complete name/index
descriptions under a 'ion' object. Checking can be desactivated,
anyway it is of course conditional to \code{rbsb.mck}...\cr Be aware
that the proposed order is not respected, nodes/variables are sorted!
}
\usage{
nv2ion(x, nom, kwhat="n", check=TRUE)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 nv2ion(1, rbsb.nom2, "n");
 nv2ion(1, rbsb.nom2, "N");
 nv2ion(1, rbsb.nom2, "v");
 nv2ion(0, rbsb.nom2, "n");
 nv2ion("-", rbsb.nom2, "n");
 nv2ion("A", rbsb.nom2, "n");
 nv2ion("B", rbsb.nom2, "n");
 nv2ion("C", rbsb.nom3, "n");
}
\arguments{
\item{x}{ indicates one or several subsets of nodes/variables of the
second argument (\code{nom}). When they are several subsets,
\code{nv2ion} deals with the union of them. The indication can be
made (1) by names (\code{character} interpreted of nodes or variates
according to the third argument \code{kwhat}; (ii) a numeric matrix
of two rows giving its columns the [node number, variable number];
(iii) a numeric giving the index of nodes or variables according to
the third argument \code{kwhat}.\cr See the description section for
more insights.\cr Additional facilities are given by the extensions
of '-' (when \code{x} is a character), 0 and -1 (when \code{x} is
numeric). \code{"-"}, \code{matrix(c(0,-1),2)} and \code{0} are
equivalent. Notice that \code{matrix(c(0,0),2)} will keep on the
developped set of variable names (not using '-'). }
\item{nom}{nom object of reference.}
\item{kwhat}{ This argument is used in two different ways according
its values. When 'x' is a single numeric it indicates if the user
wants to specify a node ("n"/"N") or a variable ("v"). But when it is
a node (either numeric or character), it indicates if the node is
wanted ("n") of the set of variables of this n}
\item{check}{( when TRUE checking of the argument consistence is
performed if rbsb.mck is also TRUE.}
}
\details{
There are different ways to designate a subset of nodes/variables
belonging to a /nom/ object. Let A[a], A[b], A[c], B, C[1], C[2] be
the set of variables of /nom/ \code{uu} given in the example section.
Intuitively, we coud designate the first node in different ways: (1)
as the first node, (2) as the node of name 'A', (3) as the first
three variables of \code{uu}, (4) as the subset of variables 'A[a]',
'A[b]' and 'A[c]'... \code{nv2ion} using one of these ways (and more)
returns equivalent ways of defining them.\cr Said in a different
manner, it gives the simultaneous translation of one way in every
known ways. This could be quite useful for exchanges between user and
algorithms. Another properties is to give the subsets in a unique
way.\cr In fact \code{"-"} is \code{rbsb.all}.
}
\value{
A 'ion' object comprising names, indices and identifications.\cr 

\item{@nn}{The node names.}
\item{@vn}{The variable names.}
\item{@nvn}{The node[variable] names.}
\item{@ij}{The variable indices within each node.}
\item{@nk}{The indices at the node level.}
\item{@vk}{The indices at the variable level.}
\item{@iden}{The identification of the \code{x} inputs by a character
vector of the same length and containing either 'nn' (-\eqn{>} node
as node), or 'nv' (-\eqn{>} node as variable set), or 'v' (-\eqn{>}
variable level).}
}
\keyword{misc}
