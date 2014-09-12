% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{pprint} 
% 
\alias{pprint} 
% 
\title{double print at the screen and into a text file} 
% 
\usage{ 
  pprint(x,fifi="",...) 
} 
% 
\examples{ 
  pprint("to see what happen!","toto.txt"); 
  read.table("toto.txt"); 
  unlink("toto.txt"); 
} 
% 
\description{ 
   
  Just doing a double printing (i) on the screen and 
  (ii) appending to the file fifi 
   
} 
% 
\value{ 
   
  nothing is returned 
   
} 
% 
 
\arguments{ 
    \item{x}{ The object to print.} 
    \item{fifi}{  Name of the file, if \samp{""} the print is only done onto the screen.} 
    \item{\dots}{additional arguments for the print call.} 
} 
 
% 
\keyword{input/output} 
