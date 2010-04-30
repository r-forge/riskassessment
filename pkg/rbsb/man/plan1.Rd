\name{plan1}
\alias{plan1}
\title{
(bn) generates imposed values to start a simulation plan on a bn
}
\description{
A matrix of phi(N) values is extracted from (most often marginal)
nodes according to the so-called uniform way to serve starting
value(s) when simulating from a bn. It is what we called a simulation
plan.\cr The constraints about the nodes to plan are those of the
function plan0 which is called (with Nx or Nn) for each node. It is
supposed that Nx \eqn{>}= Nn since x is for maXimum and n for
miNimum. For the convenience of the explanation, let us denote Vx[i]
the set of Nx values associated to the node number ith and Vn[i] the
set of Nn values associated to the same node. If Nx==Nn, the two sets
are identical (resulting from the same call of plan0) but when Nn
\eqn{<} Nx, there is no obligation that Vn be a subset of Vx (see the
algorithm in plan0 to get the answer).\cr Now the plan is constructed
as the following union of combinations of these level sets:\cr
Union(for i varying over the nodes) of (Vx[i] times (product(for
j!=i) of the Vn[j])).\cr So a duplicate complete plan is produced
with Nx=Nn and a central composite plan with Nn=1. Intermediate plans
for 1\eqn{<}Nn\eqn{<}Nx.\cr When all involved nodes can return to
plan0 the Nx number of levels, the number of observation of the plan
is given by p.(Nx).(Nn)\eqn{\mbox{\textasciicircum}}{^}p where p is
the number of nodes.
}
\usage{
plan1(bn, ion, Nx=7, Nn=3, repl=0)
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 plan1(rbsb.bn2, 1);
 plan1(rbsb.bn2, 2);
}
\arguments{
\item{bn}{The original bn.}
\item{ion}{indicates the node(s) from which values must be fixed (iin
or name).}
\item{Nx}{(=7) number of values to extract for expanded nodes.}
\item{Nn}{(=3) number of values to extract for node in the central
part.}
\item{repl}{(=0) abs(repl) is the number of replications to provide
once the first replication has been done. When the number is
negative, TRUE repetition of the first replication are not
numerically removed. 0 is equivalent to -1.}
}
\details{
Notice that the result can be random, so it is advisable to monitor
the pseudo-random seed value.\cr Multivariate nodes are accepted and
their variables are considered as independent nodes.
}
\value{
The resulting data.frame of the values. Columns have got the node
names; categoric are factors.
}
