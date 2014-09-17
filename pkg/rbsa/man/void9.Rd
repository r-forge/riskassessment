% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{void9} 
% 
\alias{void9} 
% 
\title{tests the nullness of objects} 
% 
\usage{ 
  void9(x,vector=FALSE) 
} 
% 
\examples{ 
  \dontrun{void9(toto);} 
  void9(numeric(0)); 
  void9(NULL); 
  void9(""); 
  void9(c("","A")); 
  void9(c("","A"),vector=TRUE); 
} 
% 
\details{ 
   
  Non-existing objects are not detected as void! 
  Functions are considered as not void. 
  Mainly \samp{NULL}, \samp{NA} and length-zero object are void 
  then a \samp{data.frame} with at least one variable is declared as not void. 
  Character \samp{""} is considered as void. 
   
} 
% 
\description{ 
   
  Returns TRUE is the structure is empty.\cr 
  This function was proposed because the \samp{NULL} cannot replace 
  any kind of objects.\cr 
  Notice that \samp{void9("")} is \samp{TRUE}. 
   
} 
% 
\value{ 
   
  TRUE when the object is considered as void 
  FALSE if not. Can also be a vector for characters when \samp{vector}. 
  See the proposed examples. 
   
} 
% 
 
\arguments{ 
    \item{x}{object to be scrutinazed} 
    \item{vector}{This argument is considered only when \samp{x} is a \samp{character} or a (\samp{numeric} or \samp{logical}). When \samp{FALSE} it is considered void when all components are \samp{""} or \samp{NA}; when \samp{TRUE}, a vector with same length as \samp{x} is returned indicating which components are \samp{""} or \samp{NA}.} 
} 
 
% 
\keyword{programming} 
\keyword{miscellaneous} 
\section{Future}{ 
Find a trick for non-existing objects: difficulty of the kind is that \samp{TRUE} 
is detected as void and more 
} 
