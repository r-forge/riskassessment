% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{estimate8arcs} 
% 
\alias{estimate8arcs} 
% 
\title{estimates a common regression coefficient to a set of arcs} 
% 
\usage{ 
  estimate8arcs(nbn,marc,data,check=rbmn0$check$v) 
} 
% 
\details{ 
   
  This is mainly an ancillary function of \samp{estimate8constrained7nbn}\cr 
  Two coefficient are updated: the regression coefficient and 
  the estimate of the standard deviation. 
   
} 
% 
\description{ 
   
  Supposing known all parameters but one in a /nbn/ object, the function 
  provides the esmate for the missing one which is supposed common to 
  arcs described in the argument \samp{marc}. Constant term is included 
  in the optimization. 
   
} 
% 
\value{ 
   
  the resulting /nbn/ object with the common parameter. 
   
} 
% 
 
\arguments{ 
    \item{nbn}{\samp{nbn} object.} 
    \item{marc}{Matrix with two columns indicating the tails (1rst column) and the heads (2d column) of the arcs having a common parameter. It is checked that these arcs are indeed included in \samp{nbn}. Nodes must be indicated by their names (not their number).} 
    \item{data}{Data frame to be used for the estimation. It must comprise all necessary nodes (not only those involved in \samp{marc} but also the remaining parents of \samp{marc[,2]}. Usually, all used variables are centred but this is not required.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{arc} 
\keyword{estimation} 
