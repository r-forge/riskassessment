% 
% created by /documair/ at 14_09_15@12:32 
% 
\name{documair0} 
% 
\alias{documair0} 
% 
\title{list of the /documair/ constants} 
% 
\examples{ 
  names(documair0); 
  print(documair0$text1); 
  print(documair0$text2$v); 
} 
% 
\details{ 
   
  It is a named list, one component for each constant. 
  A sublist is associated to each constant with two components: \samp{$d} 
  for the definition and \samp{$v} for the value. Be aware that the value 
  can be any object (vector, list, matrix, function,...) 
   
} 
% 
\description{ 
   
  Just a list of constants defined with a name, a definition 
  and a value. They can be modified by the user. 
  Of course the modification must be consistent with the role of 
  the constant, and only values should be modified.\cr 
   
} 
% 
\value{ 
   
  A list (it is a self-documented object). 
   
} 
% 
\keyword{tags} 
\keyword{helpful} 
\keyword{documair} 
