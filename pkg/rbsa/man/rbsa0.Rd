% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{rbsa0} 
% 
\alias{rbsa0} 
% 
\title{list of the /rbsa/ constants} 
% 
\examples{ 
  print(display8k(rbsa0,"names")); 
  print(display8k(rbsa0)); 
  print(display8k(rbsa0$monitor$v,"names")); 
  print(display8k(rbsa0$monitor$v)); 
  str(rbsa0$monitor$v); 
} 
% 
\details{ 
   
  It is a named list, one component for each constant. 
  A named sublist is associated to each constant with two components: \samp{$d} 
  for the definition and \samp{$v} for the value. Be aware that the value 
  can be any object (vector, list, matrix, function,...) 
   
} 
% 
\description{ 
   
  Just a list of constants. Each constant is defined with a name, a definition 
  and a value. The initial values can be modified by the user. Some 
  specifies the way the \pkg{rbsa} calculations and displays are made. Values 
  definitions and names can be obtained with the function \samp{display8k}.\cr 
  An important role of the constant is to drive the behavior of many functions 
  through \samp{rbsa0$monitor$v} which is itself a \samp{rbsa} constant. 
   
} 
% 
\value{ 
   
  A list see the details for the description. 
   
} 
% 
\keyword{miscellaneous} 
\keyword{miscellaneous} 
\keyword{rbsa} 
\seealso{ 
\code{\link{rbsa7list9}} 
\code{\link{display8k}} 
} 
