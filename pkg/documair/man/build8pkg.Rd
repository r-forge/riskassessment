% 
% created by /documair/ at 14_09_15@12:32 
% 
\name{build8pkg} 
% 
\alias{build8pkg} 
% 
\title{builds a package} 
% 
\usage{ 
  build8pkg(pkg,documair7dir,destination7dir,
            what="pz",display=TRUE,
            signature=2,check=FALSE) 
} 
% 
\details{ 
   
  Whatever is the argument \samp{what}, the preparation of the arborescence is made.\cr 
  Checking with fatal errors can be issued by the functions called by \samp{build8pkg}. If this 
  occurs, it means that the check made by \samp{build8pkg} are not sufficient: please indicate us it. 
   
} 
% 
\description{ 
   
  From the documair directory containing at least the description file (\samp{pkg.DESCRIPTION}), 
  the presentation file (\samp{pkk.pacakge.Rd}) and the code files (\samp{*.code.r}) launches 
  the preparation, the checking and the building by \pkg{R} itself.\cr 
  Resulting \samp{pdf} manual and \samp{pkg.tar.gz} files are placed into the indicated 
  destination directory. Some intermediate directory (especially the \samp{pkg.Rcheck} directory) 
  are placed then deleted (in case of no error) into the within directory: so the possibility 
  to create and remove them there is necessary. To look at the intermediate results, \samp{prepare8pkg} 
  and \samp{compile8pkg} must be directly used, or \samp{l} for \samp{left} must be included in 
  the argument \samp{what}. 
   
} 
% 
\value{ 
   
  \samp{character(0)} when all went right if not a \samp{character} 
  reporting the motives of difficulties. 
   
} 
% 
 
\arguments{ 
    \item{pkg}{ Name of the package.} 
    \item{documair7dir}{ Directory where are gathered all necessary files for \pkg{documair}.} 
    \item{destination7dir}{ Directory where the resulting files have to be placed. If not existing will be created.} 
    \item{what}{ \samp{character(1)}. Indicates the actions to perform. It is lowercased before decoding. Wwhen it comprises \samp{c} the check is performed; when it comprises \samp{z} the tar.gz is generated; when it comprises \samp{p} the pdf manual is generated; when it comprises \samp{l} intermediate directories are left.} 
    \item{display}{ Must indications about the process progress be displayed onto the screen?} 
    \item{signature}{ \samp{numeric(1)}. Must additional information be included in the \samp{pkg.package.Rd} file? When \samp{0} no; when \samp{1} just the name of \pkg{documair}; when \samp{2} plus the list of the masked functions.} 
    \item{check}{ Must checking be done by the user after each debugging display? Checking means that the program rrrpause after each display awaiting for an answer from the user to continue or stop.} 
} 
 
% 
\keyword{build} 
\keyword{package} 
\section{Future}{ 
Make \samp{display} more efficient when \samp{FALSE}. 
} 
\seealso{ 
\code{\link{prepare8pkg}} 
\code{\link{compile8pkg}} 
} 
