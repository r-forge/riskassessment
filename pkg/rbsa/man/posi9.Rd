% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{posi9} 
% 
\alias{posi9} 
% 
\title{returns the positions of identifiers} 
% 
\usage{ 
  posi9(which,within,strict=TRUE,repe=TRUE,
        monitor=rbsa0$monitor$v) 
} 
% 
\examples{ 
  posi9(10:1,1:10); 
  posi9(c("Z","E","P","L","I","N"),LETTERS); 
  posi9(c("B","o","F","!"),LETTERS,strict=FALSE); 
  posi9(c("B","E","B","E"),LETTERS); 
  \dontrun{posi9(c("B","E","B","E"),LETTERS,repe=FALSE);} 
} 
% 
\description{ 
   
  Returns the positions of identifiers (an identifier is 
  \samp{character(1)} or \samp{integer(1)}). 
  From a reference set of 
  identifiers (a vector in fact) returns the positions of some of them. 
  If they don't belong to the list, an error is issued 
  according to \samp{strict}. If there are repetitions 
  an error is issued or not according to \samp{repe}. 
   
} 
% 
\value{ 
   
  A numeric vector of the positions. It is named with the identifiers. 
  When the identifier does not belong to the list and \samp{strict} is 
  FALSE, NA values are returned. 
   
} 
% 
 
\arguments{ 
    \item{which}{Identifiers, their position is looked for. Must be a character or a numeric.} 
    \item{within}{The ordered vector of all identifiers. Must be a character or a numeric of distinct values.} 
    \item{strict}{Must \samp{which} be a subset of \samp{within}.} 
    \item{repe}{Are repetitions allowed?} 
    \item{monitor}{List of constants indicating the monitoring choices, see the \samp{rbsa0$monitor$v} provided object as an example.} 
} 
 
% 
\keyword{miscellaneous} 
\keyword{position} 
