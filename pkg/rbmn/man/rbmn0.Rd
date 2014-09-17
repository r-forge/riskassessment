% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{rbmn0} 
% 
\alias{rbmn0} 
% 
\title{list of the /rbmn/ constants} 
% 
\examples{ 
  names(rbmn0); 
  rbmn0$check; 
  rbmn0$chain1; 
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
  and a value. They can be modified by the user. Values 
  definitions and names can be obtained with the function 
  \samp{sssdisplay8k} of /rbsa/ package. 
   
} 
% 
\value{ 
   
  A list see the description and details to be able to know its contents. 
  (it is a self-documented object.) 
   
} 
% 
\keyword{miscellaneous} 
\keyword{rbmn} 
