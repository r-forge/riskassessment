% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{df4kbd} 
% 
\alias{df4kbd} 
% 
\title{Produces a data.frame from the keyboard.} 
% 
\usage{ 
  df4kbd(def) 
} 
% 
\examples{ 
  nom <- list(question="The name?",a7type="character"); 
  edad <- list(question="How old?",a7type="numeric"); 
  defi <- list(NAME=nom,AGE=edad); 
  \dontrun{df4kbd(defi)} 
} 
% 
\details{ 
   
  Questions are proposed with the function \samp{inquiry}. Activating 
  the 'stop' in any of the questions is interpreted as the end of the 
  data capture. 
   
} 
% 
\description{ 
   
  This function issues repeatedly a series of questions onto the screen 
  and from the answers builds a data.frame. 
   
} 
% 
\value{ 
   
  The resulting data frame, possibly with zero row when no individual 
  was correctly recorded. 
   
} 
% 
 
\arguments{ 
    \item{def}{A named list describing the different variables of the data frame to produce.  Its names give the names of the variables. Each component of \samp{def} are the list of arguments to provide to function \samp{inquiry} to get the values for each individual. More precisely \samp{question} which is compulsory, \samp{help} (default \samp{NULL}), \samp{a7type} (compulsory either \samp{numeric} or \samp{character}), \samp{a7possibilities} (default \samp{NULL}).} 
} 
 
% 
\keyword{input/output} 
\keyword{data-frame} 
\keyword{keyboard} 
