% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{source8file} 
% 
\alias{source8file} 
% 
\title{sources a set of R files} 
% 
\usage{ 
  source8file(files,
              display=1,
              directory=character(0),...) 
} 
% 
\details{ 
   
  The useness of this function is to allow (1) sourcing several files with 
  a unique call and (ii) to easily change the directory when 
  working in different computers. 
   
} 
% 
\description{ 
   
  Just sourcing \samp{files} after adding a \samp{directory} 
  path and emetting a \samp{message}. 
   
} 
% 
\value{ 
   
  nothing is returned but sourcing and displaying file names if asked are performed. 
   
} 
% 
 
\arguments{ 
    \item{files}{ The R file(s) to be sourced.} 
    \item{display}{If non negative, display before its sourcing, the name of each file, by a call to \samp{form3title} with the \samp{empha} parameter set to its value.} 
    \item{directory}{ The directory path to add before the files.} 
    \item{\dots}{Further arguments to be passed to the \samp{source} function.} 
} 
 
% 
\keyword{file} 
\keyword{source} 
