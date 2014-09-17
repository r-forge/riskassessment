% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{pause} 
% 
\alias{pause} 
% 
\title{pauses the program until an answer is given} 
% 
\usage{ 
  pause(what="",mensaje=NULL,top=0,ans=NULL) 
} 
% 
\examples{ 
  pause("Time for lunch!",top=NA); 
  \dontrun{pause("Time for lunch?")} 
  \dontrun{pause("Look at your results before the process continue",top=NULL)} 
  \dontrun{pause("Can we continue?",top=c("n","N","no","NO"),ans=c("y","Y","yes","YES"))} 
} 
% 
\details{ 
   
  The answer provided by the user is interpreted as 
  a \samp{character(1)}.\cr 
  When \samp{top} is \samp{NA} the call is ignored.\cr 
  When \samp{top} is \samp{NULL} no stopping is possible 
  but the answer is returned. This answer can be 
  anything when \samp{is.na(ans)} but a void answer 
  is refused. This answer can be anything including 
  nothing when \samp{is.null(ans)}. The answer is 
  returned.\cr 
  When \samp{top} is \samp{numeric}, \samp{ans} is not considered. 
  an empty answer allows the process to follow 
  and no void answers stop the process.\cr 
  When \samp{top} and \samp{ans} are \samp{character}s, 
  the only accepted answers are a component of \samp{top} (to 
  stop the process) or a component of \samp{ans} (to be returned).\cr 
  When \samp{top} is a \samp{character} and \samp{is.null(ans) | is.na(ans)} 
  stopping is made when the answer is one of the components of 
  \samp{top}, other answers will be returned.\cr 
   
} 
% 
\description{ 
   
  This function issues a pause with a message allowing 
  to stop the process or to continue it (without or with and 
  according to the answer given by the user). 
   
} 
% 
\value{ 
   
  When the process is not stopped, returns the answer provided 
  by the user. 
   
} 
% 
 
\arguments{ 
    \item{what}{ Short message commenting the pause} 
    \item{mensaje}{ A possible longer message to give details. When \samp{NULL} a shortened message is issued.} 
    \item{top}{ Precises the rules to follow for the execution to be stopped (see the \samp{details} section).} 
    \item{ans}{ Precises the accepted answers (see the \samp{details} section).} 
} 
 
% 
\keyword{input/output} 
