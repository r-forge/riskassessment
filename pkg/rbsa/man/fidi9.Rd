% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{fidi9} 
% 
\alias{fidi9} 
% 
\title{checks files and directories} 
% 
\usage{ 
  fidi9(chemins,retour=c("-","f","d")) 
} 
% 
\examples{ 
  fidi9(c("/","~","~/.bashrc",getwd(),"toto.txt")); 
} 
% 
\details{ 
   
  the distinction between files and directories is 
  based on \samp{file.info} function. 
   
} 
% 
\description{ 
   
  checks if \samp{chemin} is a file or a directory 
   
} 
% 
\value{ 
   
  a character of same length as \samp{chemins} with one of the 
  components of \samp{retour}. 
   
} 
% 
 
\arguments{ 
    \item{chemins}{ \samp{character}; for each element of it, the existence as file or directory must be checked.} 
    \item{retour}{ \samp{character(3)} indicating what to return in case of (non existence), (existence of a file) and (existence of a directory).} 
} 
 
% 
\keyword{file} 
\keyword{directory} 
\keyword{file} 
\section{Future}{ 
introduce the notion of accessibility 
} 
\seealso{ 
\code{\link{dipa}} 
} 
