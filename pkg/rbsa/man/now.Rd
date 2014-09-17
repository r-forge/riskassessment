% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{now} 
% 
\alias{now} 
% 
\title{returns a character giving the present moment} 
% 
\usage{ 
  now(what="dh",format="red",seps=c("_","@",":")) 
} 
% 
\examples{ 
  cat("Now is",now(),"\n"); 
  cat(now("dhms","verbose"),"\n"); 
} 
% 
\details{ 
   
  based on Sys.time R function 
   
} 
% 
\description{ 
   
  Returns a character giving the present moment 
  with different components 
   
} 
% 
\value{ 
   
  a character 
   
} 
% 
 
\arguments{ 
    \item{what}{\samp{character(1)} indicating which components to include among: \cr\samp{d} for the day (including year and month), \cr\samp{h} for the hour, \cr\samp{m} for the minute (including hour), \cr\samp{s} for the second (including hour and minutes.} 
    \item{format}{\samp{r} or \samp{"red"} for a reduced output.} 
    \item{seps}{\samp{character(3)} providing the separations for the different components of the reduced returned output.} 
} 
 
% 
\keyword{utilities} 
