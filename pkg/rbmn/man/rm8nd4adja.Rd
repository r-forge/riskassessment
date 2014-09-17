% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{rm8nd4adja} 
% 
\alias{rm8nd4adja} 
% 
\title{removes somes nodes from an adjacency matrix} 
% 
\usage{ 
  rm8nd4adja(adja,nodes) 
} 
% 
\examples{ 
  rm8nd4adja(rbmn0$adja4$v,"1.1"); 
} 
% 
\details{ 
   
  When a node is removed, all its parents become parents of all its children. 
   
} 
% 
\description{ 
   
  Eliminates from the adjacency matrix (\samp{adja})all \samp{nodes} 
  not breaking the existing links.\cr 
  Important: the node order in \samp{adja} must be topological. 
   
} 
% 
\value{ 
   
  The reduced adjacency matrix. 
   
} 
% 
 
\arguments{ 
    \item{adja}{The relation matrix to be consider (same format as those provided by the function \samp{adja4nbn}. Must be in topological order, roots first.} 
    \item{nodes}{Numeric or character vector providing the node numbers to use for the generation of the subset.} 
} 
 
% 
\keyword{adjacency--matrix} 
\keyword{node} 
\section{Future}{ 
remove the topological order constraint 
} 
