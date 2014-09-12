% 
% created by /documair/ at 14_09_12@10:34 
% 
\name{compile8pkg} 
% 
\alias{compile8pkg} 
% 
\title{compiles a package} 
% 
\usage{ 
  compile8pkg(pkg,pkgdir,
              chkdir=getwd(),resdir=getwd(),
              what="pz",
              display=FALSE) 
} 
% 
\description{ 
   
  From the standard directory containing the package sources, launches 
  the checking and building by \pkg{R} itself. Pdf manual and tar ball 
  can be placed in another directory. 
   
} 
% 
\value{ 
   
  Nothing but the check, the compilation, [the copy of the manual, 
  the tar.gz] are performed. 
   
} 
% 
 
\arguments{ 
    \item{pkg}{ Name of the package.} 
    \item{pkgdir}{ Directory where the building of the package has been prepared. Probably by the function \samp{prepare8pkg}.} 
    \item{chkdir}{ Directory where to put the directory where "R" makes the checking. Must exist.  } 
    \item{resdir}{ Directory where to copy the resulting files. Must exist.} 
    \item{what}{ \samp{character(1)}. Which resulting file to copy: \samp{p} for the pdf manual and \samp{z} for the tar.gz ball.} 
    \item{display}{ Must indications about the process progress be displayed on the screen?} 
} 
 
% 
\keyword{compilation} 
\keyword{package} 
\section{Future}{ 
Make \samp{display} more efficient when \samp{FALSE}. 
} 
\seealso{ 
\code{\link{prepare8pkg}} 
} 
