% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{relpath} 
% 
\alias{relpath} 
% 
\title{returns a relative path} 
% 
\usage{ 
  relpath(pfrom,pto) 
} 
% 
\examples{ 
  relpath("A/B/C/D/","A/B/Q/R/"); 
  relpath("../../../../","A/B/Q/R/"); 
} 
% 
\details{ 
   
  Seems compulsory to forbid \samp{../} components into \samp{pto} 
   
} 
% 
\description{ 
   
  returns the relative path associated to \samp{pto} 
  starting from \samp{pfrom}; both paths being supposed 
  to be defined from the same point; possibly the \samp{/} 
  one. 
   
} 
% 
\value{ 
   
  A \samp{character} with the computed path. 
   
} 
% 
 
\arguments{ 
    \item{pfrom}{Starting path.} 
    \item{pto}{Target path. Must not comprise any \samp{../} component.} 
} 
 
% 
