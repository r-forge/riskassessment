% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{form3display} 
% 
\alias{form3display} 
% 
\title{displays with its name any object} 
% 
\usage{ 
  form3display(x,pau=FALSE,cat=FALSE,...) 
} 
% 
\examples{ 
  uu <- "azerty"; 
  form3display(uu); 
} 
% 
\description{ 
   
  displays any object after giving the name of 
  the variable containing it. A pause can be introduced 
  to give the opportunity to scrutinize the result. 
   
} 
% 
\value{ 
   
  a print (or cat) is done and \samp{x} is returned 
   
} 
% 
 
\arguments{ 
    \item{x}{The object to print.} 
    \item{pau}{ Must a pause be performed after the display?} 
    \item{cat}{ Must the printing be done with 'cat' instead of print?} 
    \item{\dots}{possible arguments for the print function.} 
} 
 
% 
\keyword{print} 
\keyword{display} 
\keyword{format} 
