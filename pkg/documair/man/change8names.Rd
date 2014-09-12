% 
% created by /documair/ at 14_09_12@10:34 
% 
\name{change8names} 
% 
\alias{change8names} 
% 
\title{changes object names in R script files} 
% 
\usage{ 
  change8names(nfiles,nnames) 
} 
% 
\details{ 
   
  It is difficult to ensure that the modifications 
  are perfect, but at least they can give a hand.\cr 
  Each file is dealt in turn. When old and new names of a file 
  coincide, the old file is first saved into a temporary 
  file suffixed with the moment 
  of the saving and only destroyed at the end. So if something 
  went wrong, you can recover the initial file. 
   
} 
% 
\description{ 
   
  Explores a series of R script files and modify them 
  changing some objects names for new ones.\cr 
  Changes must be unambiguous; they are performed sequentially 
  according to the order of \samp{nnames} rows. Also, if some functions 
  have got common names used in comments or messages, they will be 
  changed as well since the syntaxic analysis made in 
  \samp{r.text3places8word} is very limited. 
   
} 
% 
\value{ 
   
  \samp{character(0)} if everything went right, 
  if not, some warning messages. 
   
} 
% 
 
\arguments{ 
    \item{nfiles}{A two column \samp{character} matrix, giving by row old and new names of files to consider. Old and new names can be identical.} 
    \item{nnames}{A two column \samp{character} matrix, giving by row old and new names of objects.} 
} 
 
% 
\keyword{change} 
\keyword{names} 
