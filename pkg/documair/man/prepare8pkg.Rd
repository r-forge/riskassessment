% 
% created by /documair/ at 14_09_12@10:34 
% 
\name{prepare8pkg} 
% 
\alias{prepare8pkg} 
% 
\title{prepares a package} 
% 
\usage{ 
  prepare8pkg(pkg,perdir,pkgdir,
              signature=2,
              display=TRUE,
              check=FALSE) 
} 
% 
\details{ 
   
  The behaviour of \samp{prepare8pkg} with respect to files and objects coded in them, is also 
  driven by a possible \samp{pkg.which.txt} file where can be indicated which 
  objects are exported or hidden, which files contains a series of aliased objects, 
  which files and/or objects must be displayed. See the general description of the package for 
  details.\cr 
  When no such file is present, the default behavior is that all object are exported, 
  no series of objects are aliased, the displaying is just listing the explored files 
  and the objects they contain. Notice that the default displaying as well as the 
  \samp{which} file display options can be cancelled with the argument \samp{display}. 
   
} 
% 
\description{ 
   
  Prepares all necessary directories and files for the building of an R package named 
  \samp{pkg} from files made by the user and placed in the \samp{perdir} directory. 
  The built arborescence will placed into the preexisting \samp{pkgdir} directory.\cr 
  When an error occurs, it is not always very simple to know the origin of it. To get 
  some clues, the user is suggested to switch the display options on within the \samp{pkg.which.txt} 
  file and putting the \samp{check} argument to \samp{TRUE}. 
   
} 
% 
\value{ 
   
  Nothing but the preparation is made (see the description section) 
  with possible displays to and checks from the user. 
   
} 
% 
 
\arguments{ 
    \item{pkg}{ Name of the package, associated files like \samp{pkg.DESCRIPTION} must exist into \samp{perdir} directory.} 
    \item{perdir}{ Directory where the prepared files have to be found.} 
    \item{pkgdir}{ Directory where the building of the package has to be prepared. It is supposed to have already been created, at least empty. The result of the preparation of the package will be placed within it with a directory having the package name. In case, it is not empty, it is first completely cleaned of its contents.} 
    \item{signature}{ \samp{numeric(1)}. Must additional information be included in the \samp{pkg.package.Rd} file? When \samp{0} no; when \samp{1} just the name of \pkg{documair}; when \samp{2} plus the list of the masked functions.} 
    \item{display}{Must the programmed debbuging displays be performed?} 
    \item{check}{ Must checking be done by the user after each debugging display? Checking means that the program r.pause after each display awaiting for an answer from the user to continue or stop.} 
} 
 
% 
\keyword{package} 
\keyword{preparation} 
\section{Future}{ 
Introduce more sections and improve the existing ones. 
} 
\seealso{ 
\code{\link{compile8pkg}} 
\code{\link{display8tags}} 
} 
