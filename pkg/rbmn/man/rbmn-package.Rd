\name{rbmn-package} 
\alias{rbmn-package} 
\alias{rbmn} 
\docType{package} 
\title{ 
Linear Gaussian Bayesian network manipulations 
} 
\description{ 
General functions to generate, transform, display general and 
particular linear Gaussian Bayesian networks [/nbn/] are provided.\cr 
Specific /nbn/ are chain and crossed /nbn/s. Focus is given in getting 
joint and conditional probability distributions of the set of 
nodes.\cr 
\emph{rbmn} stands for \emph{R'eseau Bay'esien MultiNormal}. 
} 
\details{ 
Some basic concepts: 
\itemize{ 
\item \emph{chained /nbn/s} are /nbn/s where all nodes are connected with two 
other nodes, except the two ending nodes of the chain having only one 
connection. (This is not the usual terminology in graphical models 
but I didn't find a more appropriate word: suggestions are welcome.) 
\item \emph{crossed /nbn/s} are /nbn/s having the node set defined as a 
Cartesian product of two series of items, and a DAG based on this 
structure. See the \code{crossed4nbn1nbn} function and/or Tian (2013) for 
details. 
\item An \emph{adjacency matrix} is a matrix equivalent to the DAG 
associated to a /nbn/. Its rows as well as its columns are associated 
to the set of nodes. The \code{(i,j)} cell is one when there is an arc going 
from node \code{i} to \code{j} and zero otherwise. 
} 
Three equivalent ways can be used to represent the joint probability 
distribution of a set of nodes respectively associated to the 
structures /mn/, /nbn/ and /gema/: 
\itemize{ 
\item /mn/ (for multivariate normal) is just the list of the 
expectation (\code{$mu}) and the variance matrix (\code{$gamma}) 
plus the regression coefficient matrix to encompass conditional 
distribution as well, 
\item /nbn/ (for normal Bayesian network) is a simple list, a 
component a node described with a list. The names are node names and 
each list associated to a node provides the conditional expectation 
and variance, the parent (if any) and the associated regression 
coefficients. 
\item /gema/ (for generating matrices) is a list of a vector 
(\code{mu}) and a matrix (\code{li}) such that the vector of the 
nodes can be defined by \code{X = mu + li\%*\%E} where \code{E} is a 
normal random vector with expectation zero and variance matrix 
unity. 
\item  It is planned to add a fourth one under the name of /gbn/, 
similar to /mn/ but where the individual conditional distributions 
of the nodes are given and not the joint distribution (see below). 
} 
To relieve the memory effort, most names of the functions have been 
given a two (or more) components structure separated with a 
digit see the documentation of package \pkg{documair} for details. 
} 
\section{Projected evolution of /rbmn/}{ 
\itemize{ 
\item Develop more functions about the topological orders of 
Bayesian networks. Possibly add an optional new component 
giving a topological order. 
\item Introduce a new object \code{gbn} for Gaussian Bayesian 
network similar to the list provided by 
function \code{nbn2rr}. 
} 
} 
\section{TO DO list}{ 
\itemize{ 
\item Understand why I was obliged to add \samp{check=TRUE} in the 
examples of \samp{adja4nbn} and other similar function for R to be 
happy... Is the fact that the function name starts with \samp{a,b} be 
the cause? 
\item Make the function \code{nbn4string7dag}. 
\item Make a super transformation function from an object associated 
to a Bayesian network to any other type, including itself. 
\item Correct the \code{ord} option in \code{order4chain}. 
\item Think about removing all \code{rmatrix} transformations to the 
benefit of the to-come \code{gbn} object. 
} 
} 
\author{ 
Jean-Baptiste Denis\cr 
MIAj - Inra - Jouy-en-Josas \cr 
F-78532 Jouy-en-Josas 
Maintainer: Jean-Baptiste Denis \email{Jean-Baptiste.Denis@Jouy.Inra.Fr} 
} 
\references{ 
\itemize{ 
\item A technical report presenting the concepts used in \pkg{rbmn} is 
under redaction; it can be obtained as it is if asked.) 
\item Scutari M (2010). "Learning Bayesian Networks with the bnlearn R 
Package". Journal of Statistical Software, \strong{35}(3), 
1-22. URL http://www.jstatsoft.org/v35/i03/. 
\item Tian S, Scutari M & Denis J-B (2014). "Predicting 
with Crossed Linear Gaussian Bayesian Networks". Journal de la 
Soci'et'e Fran'c'aise de Statistique. 
} 
} 
\examples{ 
library(rbmn); 
rbmn0 <- complete8rbmn0(rbmn0); 
data(boco); 
print(head(boco)); 
mod <- rbmn0$nbn5$v; 
est <- estimate8nbn(rbmn0$nbn5$v,boco); 
print8nbn(mod); 
print8nbn(est); 
nbnmod <- mod; 
print8nbn(nbnmod); 
mnmod <- nbn2mn(mod); 
print8mn(mnmod); 
gemamod <- nbn2gema(mod); 
print8gema(gemamod); 
} 
\keyword{ package } 
\section{Additional Information}{ 
  \itemize{ 
    \item This package was built with /documair/ package (version 0.6-0) 
       on 14_09_16 
    \item 
  There are 122 object(s) in total. 
76 are exported 
 and there exist 46 masked object(s):  
aux0, aux1, aux2, aux3, aux4, aux5, aux6, aux7, aux8, sssbc, sssbd, sssbelong9, sssbf, sssdipa, sssdisplay8k, ssserreur, sssexplore8list, sssfidi9, sssfile2list, sssfile2text, sssfilter8text, sssform3crop, sssform3join, sssform3justify, sssform3names, sssform3numbering, sssform3repeat, sssform3split, sssform3titre, sssform3titre, sssget8comp7list, sssinterv7belonging, ssslist2file, ssslist2text, sssnow, sssobject9, sssposi9, sssrbsa0, sssrbsa7list9, sssread8list, ssstext2file, ssstext2list, ssstext2vma, ssstext3preparation, sssvma2text, sssvoid9. 
   \item They were provided through 15 code file(s). 
    \item There were also 7 test file(s). 
  } 
} 
