% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{arcs4nbn1nbn} 
% 
\alias{arcs4nbn1nbn} 
% 
\title{returns the list of 'parallel' arcs of a crossed-nbn} 
% 
\usage{ 
  arcs4nbn1nbn(nbn1,nbn2,type="a1",
               nona=as.vector(outer(names(nbn1),
               names(nbn2),paste,sep="_")),
               check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print(arcs4nbn1nbn(rbmn0$nbn1$v,rbmn0$nbn4$v,"a1",check=TRUE)); 
  print(arcs4nbn1nbn(rbmn0$nbn1$v,rbmn0$nbn4$v,"n1",check=TRUE)); 
} 
% 
\description{ 
   
  Returns a list of matrices with two columns (as needed by \samp{estimate8constrained7nbn}) 
  indicating corresponding arcs for each arcs/nodes of \samp{nbn1} (or \samp{nbn2}) of the 
  crossed /nbn/ obtained when crossing /nbn1/ and /nbn2/ with node names given by \samp{nona}. 
   
} 
% 
\value{ 
   
  The resulting named (after node names) list of matrices. 
   
} 
% 
 
\arguments{ 
    \item{nbn1}{ The first generating /nbn/.} 
    \item{nbn2}{ The second generating /nbn/.} 
    \item{type}{ \samp{"a1"} to indicate that the parallelism must be done for each arc of \samp{nbn1}; \samp{"a2"} for each arc of \samp{nbn2}; \samp{"n1"} for each node of \samp{nbn1}; \samp{"n2"} for each node of \samp{nbn2}.} 
    \item{nona}{ The node names to give to the crossed /nbn/, the nodes of the \samp{nbn1} varying first.} 
    \item{check}{Must the arguments be checked?} 
} 
 
% 
\keyword{arc} 
\keyword{normal--Bayesian--network} 
