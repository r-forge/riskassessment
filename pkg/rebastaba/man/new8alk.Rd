\name{new8alk}
\alias{new8alk}
\title{
generates a new alk from user specification
}
\description{
(ba) Just collecting specifications provided by the user to build an
alk object up taking into consideration the specificity of the ltype.
\cr Default values are consistent for not to be used arguments.
}
\usage{
new8alk(ldes, ltype, lpara=rbsb.lis0, lrep=0, lnat=rbsb.cha0,
	  lvar=rbsb.cha0, lparent=rbsb.cha0, lpod=rbsb.lis0,
	  lred=rbsb.lis0, lcod=rbsb.lis0, ltransfo="",
	  ldaf=rbsb.daf0, lwin=rebastaba.win0, lfunct=rbsb.fun0)
}
\arguments{
\item{ldes}{Human description of the potential node through either a
character for the name or a /des/ object.}
\item{ltype}{ type of link: a character string which must belong to
the \code{names(rebastaba.tlk)}. This list is returned by
\code{help8ltype("ls")}.}
\item{lpara}{ list of parameter values for standard distributions.}
\item{lrep}{ the number of repetitions; zero means no repetitions.
Not all node types can be repeated.}
\item{lnat}{ According to the ltype (see \code{help8ltype}) can be
any component of \code{rbsb.sna}. When rbsb.cha0, the standard nature
associated to \code{ltype}. When it is a multivariate node, it must
be a vector with the corresponding natures. For 'empidata' it is
given by the slot \code{@nat} of \code{lwin}.}
\item{lvar}{ This argument can be used either for multivariate nodes,
or for univariate nodes which are repeated (indeed it is the way to
indicate repetitions which are then possible only for univariate
distribution.\cr In both case, lvar provides the vector of variable
names of the node. Its length gives the number of variables within
the added node. Standard probability distributions are associated
with a number of variables and it will be checked. For 'empidata' it
is given by the names of slot \code{@nat} of \code{lwin}.}
\item{lparent}{ parent vector at the node level; in most cases these
are deduced from the other arguments.}
\item{lpod}{ When rbsb.lis0, possible domain(s) associated to ltype
is provided (see rebastaba.tlk).\cr This must be a list with as many
components as variables in the added node. Each component of the list
is associated with a variable of the node (in the same order as given
in slot lvar. When there is only one variable, can be a vector
instead of a list with a unique element which is a vector.\cr (*)
when lnat== 'cateo' or 'categ', as many character strings as the
number of categories. Be aware that the length of lpod provides also
the number of categories! (*) when lnat=="integ" the lower and upper
bounds of the variation of the integer variable (e.g. c(0,17)) (*)
when lnat=="conti" the strict lower and upper bounds of the variation
of the continuous variable (e.g. c(-I}
\item{lred}{ Idem of lpod but for representation domain instead of
possible domaine}
\item{lcod}{ Idem of lpod but for common domain instead of possible
domaine}
\item{ltransfo}{ a simple way to define a transformation of the drawn
values by some easyprogramming string. Also "[\eqn{|}2\eqn{|}]" means
rounding with 2 decimals and remind that the alias for the node under
creation is "*Y*".}
\item{ldaf}{ in the case of data.frame distributions provides the
associated /daf/.}
\item{lwin}{ /win/ object describing the way to use a data frame
based distribution. See the description of this object for more
details.}
\item{lfunct}{ can be a function to be used as simulating function
when ltype is \code{program}.}
}
\details{
The complete list of standard already programmed distributions is
given with \code{help8ltype} function.\cr
}
\value{
The generated 'alk'
}
