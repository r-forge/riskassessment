% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{topo4nbn} 
% 
\alias{topo4nbn} 
% 
\title{topological order of a /nbn/} 
% 
\usage{ 
  topo4nbn(nbn,ord=NULL,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  names(rbmn0$nbn4$v)[topo4nbn(rbmn0$nbn4$v)]; 
} 
% 
\details{ 
   
  When \samp{!is.null(ord)} the order must be a permutation, if 
  not an error is issued. 
   
} 
% 
\description{ 
   
  Returns one of the orders of the nodes 
  such as the parents of any node are less ranked than 
  it when \samp{is.null(ord)}.\cr 
  If not check that the proposed order 
  is either a right permutation (\samp{is.numeric(ord)}) or 
  a vector of node names providing a topological order 
  (\samp{is.character(ord)}). 
   
} 
% 
\value{ 
   
  When no topological order, an error is issued! \cr 
  When \samp{is.null{ord}}, a \samp{integer} giving a permutation 
  of the nodes of the /nbn/ providing 
  a topological order.\cr 
  When \samp{!is.null{ord}}, \samp{TRUE} or \samp{FALSE} 
  to indicate that it is a topological order or not. 
   
} 
% 
 
\arguments{ 
    \item{nbn}{\samp{nbn} object for which the order must be computed or checked.} 
    \item{ord}{\samp{NULL} or an order to test as a permutation or a vector of names.} 
    \item{check}{ Must a check of the argument be performed?} 
} 
 
% 
\keyword{normal--Bayesian--network} 
\keyword{topological--order} 
