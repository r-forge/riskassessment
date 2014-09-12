% 
% created by /documair/ at 14_09_12@10:34 
% 
\name{copy8objects} 
% 
\alias{copy8objects} 
% 
\title{generates a file comprising indicated functions} 
% 
\usage{ 
  copy8objects(nfiles,nobjects,file) 
} 
% 
\details{ 
   
  Standard tags defined by \samp{documair0$tags} are used. 
   
} 
% 
\description{ 
   
  Explores a series of R code files supposed to 
  comprise the needed functions with \pkg{tagging} 
  and copy them into a new not already existing files. 
   
} 
% 
\value{ 
   
  \samp{character(0)} if everything went right, 
  if not, some warning messages about found 
  difficulties. 
   
} 
% 
 
\arguments{ 
    \item{nfiles}{ Files where to look for the functions.} 
    \item{nobjects}{ A \samp{character} indicating the objects to pick up from the indicated files.} 
    \item{file}{ Name of the file to create.} 
} 
 
% 
\keyword{copy} 
\keyword{object} 
