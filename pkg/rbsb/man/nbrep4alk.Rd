\name{nbrep4alk}
\alias{nbrep4alk}
\title{
(ba) determines the number of repetitions of an alk
}
\description{
determines the number of repetitions of an alk, taking care of its
possible parents.
}
\usage{
nbrep4alk(alk, nom)
}
\arguments{
\item{alk}{alk object}
\item{nom}{nom where the possible parents of the node can be found.}
}
\value{
the number of repetitions: 0 for non repeatable nodes; the dimension
for repeatable nodes
}
