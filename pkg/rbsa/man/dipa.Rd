% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{dipa} 
% 
\alias{dipa} 
% 
\title{normalize directory paths} 
% 
\usage{ 
  dipa(chemins,sep=.Platform$file.sep) 
} 
% 
\examples{ 
  dipa(c("/","~","~/.bashrc",getwd(),"toto.txt")); 
} 
% 
\details{ 
   
  Multiple \samp{sep} are removed as well as any final \samp{sep} 
  with the exception of \samp{sep} being the unique path, that is 
  the root path in Linux operating system. 
   
} 
% 
\description{ 
   
  returns the paths contained into \samp{chemins} in 
  a normalized way. No check of existence of the path(s) 
  is performed, for that the function \samp{fidi9(dipa(...))} could be used. 
   
} 
% 
\value{ 
   
  a character with same length as \samp{chemins} 
   
} 
% 
 
\arguments{ 
    \item{chemins}{ \samp{character} supposed to contain directory paths.} 
    \item{sep}{ The separator to use.} 
} 
 
% 
\keyword{directory} 
\seealso{ 
\code{\link{fidi9}} 
} 
