\name{new8alk}
\alias{new8alk}
\title{
(ba) defines a new alk from user specification
}
\description{
Just collecting specifications provided by the user to build an alk
object up taking into consideration the specificity of the ltype. \cr
The potential links can be defined according to different possible
way.
}
\usage{
new8alk(ldes, ltype, lpara=rbsb.lis0, lrep=0, lnat=rbsb.cha0,
	  lvar=rbsb.cha0, lparent=rbsb.cha0, lpod=rbsb.lis0,
	  lred=rbsb.lis0, lcod=rbsb.lis0, ltransfo="",
	  ldaf=rbsb.daf0, lwin=rbsb.win0, lfunct=rbsb.fun0)
}
\arguments{
\item{ldes}{Human description of the potential node through either a
character for the name or a 'ds' object.}
\item{ltype}{ type of link: a character string which must belong to
the "rbsb.tlk" names. Type help8ltype("ls") to get the list.}
\item{lpara}{(=rbsb.lis0) list of parameter values for standard
distributions.}
\item{lrep}{(=0) the number of repetitions; zero means no
repetitions. Not all node types can be repeated.}
\item{lnat}{(=rbsb.cha0) According to the ltype (see help8ltype())
can be any component of "rbsb.fun0". When rbsb.cha0, the standard
nature associated to ltype is provided (see help8ltype).\cr When it
is a multivariate node, it must be a vector with the corresponding
natures}
\item{lvar}{(=rbsb.cha0) This argument can be used either for
multivariate nodes, or for univariate nodes which are repeated
(indeed it is the way to indicate repetitions which are then possible
only for univariate distribution.\cr In both case, lvar provides the
vector of variable names of the node. Its length gives the number of
variables within the added node. Standard probability distributions
are associated with a number of variables and it will be checked. \cr
For multivariate nodes, rbsb.cha0 will imply lvar=1,2,... Due to R
flexibility the vector can be numerical.}
\item{lparent}{(=rbsb.cha0) parent vector at the node level; in most
cases these are deduced from the other arguments.}
\item{lpod}{(=rbsb.lis0) When rbsb.lis0, possible domain(s)
associated to ltype is provided (see rbsb.tlk).\cr This must be a
list with as many components as variables in the added node. Each
component of the list is associated with a variable of the node (in
the same order as given in slot lvar. When there is only one
variable, can be a vector instead of a list with a unique element
which is a vector.\cr (*) when lnat== 'cateo' or 'categ', as many
character strings as the number of categories. Be aware that the
length of lpod provides also the number of categories! (*) when
lnat=="integ" the lower and upper bounds of the variation of the
integer variable (e.g. c(0,17)) (*) when lnat=="conti" the strict
lower and upper bounds of the variation of the continuous variable
(e.g. c(-I}
\item{lred}{(=rbsb.lis0) Idem of lpod but for representation domain
instead of possible domaine}
\item{lcod}{(=rbsb.lis0) Idem of lpod but for common domain instead
of possible domaine}
\item{ltransfo}{(="") a simple way to define a transformation of the
drawn values. E.g. "[[2]]" means rounding with 2 decimals. Remind
also that the pseudo-nom for the node under creation is "*Y*".}
\item{ldaf}{(=rbsb.daf0) in the case of data.frame distributions
provides the associated /daf/.}
\item{lwin}{(=rbsb.win0) values for selecting a subset when drawing
in case of an empidata distribution. There is one value for each
parent, and its use depends on the nature of the parents. See
draw8empidata function for more details.}
\item{lfunct}{(=rbsb.fun0) can be a function to be used as simulating
function when ltype is program.}
}
\details{
The complete list of standard already programmed distributions is
given with help8ltype function.\cr
}
\value{
The generated 'alk'
}
