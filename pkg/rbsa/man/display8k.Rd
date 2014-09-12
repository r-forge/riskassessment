% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{display8k} 
% 
\alias{display8k} 
% 
\title{returns information about a /rbsa/ constant} 
% 
\usage{ 
  display8k(constants,what="exhibit") 
} 
% 
\examples{ 
  display8k(rbsa0) 
  display8k(rbsa0$monitor$v,"v") 
} 
% 
\details{ 
   
  A \samp{rbsa} type constant is a named list, each component being 
  a list of two components: \samp{$d} for the definition which must be 
  a character (concatenated if not a scalar) and \samp{$v} which 
  can take any value. 
   
} 
% 
\description{ 
   
  Exhibits a \samp{rbsa} constant 
  or returns a \samp{character} vector for the names and 
  the definitions of \samp{constants} or a \samp{list} for 
  the values. 
   
} 
% 
\value{ 
   
  Information is displayed and nothing is returned for exhibition 
  else a named \samp{character} or a named \samp{list}. 
   
} 
% 
 
\arguments{ 
    \item{constants}{ The \samp{rbsa} constant list (see the details for the description).} 
    \item{what}{a character(1) indicating what must be returned. The acceptable values are \samp{exhibit}, \samp{names}, \samp{definitions} and \samp{values}. For user convenience, the argument is lowercased before checked, and only the first character is taken into consideration.} 
} 
 
% 
\keyword{miscellaneous} 
\keyword{miscellaneous} 
\keyword{display} 
\keyword{constant} 
