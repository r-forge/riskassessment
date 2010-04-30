\name{study8nd}
\alias{study8nd}
\title{
(bn) extracts sub-bn(s) with one parent one child from a bn
}
\description{
From a given bn and for one specified node (ion), the list of all
'one relevant root' versus 'this child' bn(s) is constructed. Non
considered parents are put at a fixed value.
}
\usage{
study8nd(bn, ion, unfixed=character(0))
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 study8nd(rnorbn(rbsb.gg1), "F");
}
\arguments{
\item{bn}{The original bn.}
\item{ion}{The definition of the target node (so-called child)}
\item{unfixed}{(=character(0)) The list of parents not to be fixed.
This list can comprise other thing that parent names, they will be
neglected.}
}
\details{
For the sake of simplicity, let us call here as "parent" a root of
the bn which has the studied node as descendant. The non varying
parents are fixed with Dirac distributions for the continuous
variables and single domain for categoric variables. The unique
varying parent is inherited from a bn2bn call.\cr It must be
underlined that the desired construction is not as straightforward as
one can think at first. The difficulty comes with the fact that the
parents are not necessarily independant. Let us consider the simple
case: (A-\eqn{>}C; B-\eqn{>}C). C has got two parents which are
independent and we probably be happy in studying the two
probabilistic relations: [C\eqn{|}A,B=b] and [C\eqn{|}A=a,B] for
respectively assess the influence of A over C; and of B over C. To do
so we use these conditional probability adding a uniform distribution
onto A and B\cr Now if we add a third arc: (A-\eqn{>}B), this is not
so clear because A and B are no longer independent! In that case, a
choice could be to study sum\_b([C\eqn{|}A,B=b]) and [C\eqn{|}B,A=a]
loosing the symetry between the two parents to follow the structure
of the graph.\cr But things are indeed much more complicated since
the joint probability of (A,B) can depend on non direct parents. Let
us add a new node (D) and the arc (D-\eqn{>}B)... Then natural
proposals are not so spontaneous.\cr This is why the retained choice
here, was to study the variation of one node for root ancestors. If
somebody wants to use directly the direct parents, s/he must breaks
the relationships between the parents down (assuming their
independence); this can be done with bn2bn function.
}
\value{
A list of length the number of generated bn, each component
comprising a bn. For each, description@comm[1] comprises the model
associated to the bn, description@comm[2] comprises the list of fixed
nodes.
}
