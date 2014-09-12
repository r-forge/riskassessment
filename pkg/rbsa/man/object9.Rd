% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{object9} 
% 
\alias{object9} 
% 
\title{checks the type, the length and the content of some standard object} 
% 
\usage{ 
  object9(x,typ,len=-1,con=numeric(0),
          mensaje=NULL,fatal=TRUE,na.rm=FALSE,
          cla=NULL,speci=NULL) 
} 
% 
\examples{ 
  object9(letters,c("numeric","character"),c(20,30),mensaje="!OK"); 
  object9(structure("A",.Names="a"),"ncharacter",-1,LETTERS); 
  object9("A","ncharacter",-1,mensaje="BAD",fatal=FALSE); 
  object9(structure("A",.Names="a"),"character",-1,letters,"bad",fatal=FALSE); 
} 
% 
\details{ 
   
  'integer' has not got the meaning in \samp{is.integer} R 
  standard function. 'null' must be understood as 
  resulting TRUE with function \samp{void9}. 
   
} 
% 
\description{ 
   
  checks the type, the length and the content 
  of some standard object. 
  If not correct, a message and a fatal error are issued. 
  \samp{NA} are detected and considered as wrong or not.\cr 
  When \samp{typ} is 'directory' or 'file', \samp{x} must be 
  a character containing path(s) of directories or files. 
   
} 
% 
\value{ 
   
  When everything is OK returns a \samp{character(0)}. 
  If not according to the argument \samp{fatal} print an error message 
  and stops, or just returns the message as a warning. 
   
} 
% 
 
\arguments{ 
    \item{x}{object to be checked.} 
    \item{typ}{ Character vector indicating the accepted types for the object to be checked. See the code for the list of accepted types, among them 'null', 'integer', 'numeric', 'character', 'logical', 'list', 'any', 'function', 'data.frame', 'matrix','file','directory',... As indicated, 'any' implies that no check of the type is performed.} 
    \item{len}{If \samp{length(len)==1}, the exact length to be checked, if not must be of length two for the possible range of \samp{length(x)}. When -1, no check on the length is performed.\cr For data.frame, it is the number of columns. When \samp{na.rm} the check is made after removing the NA values.} 
    \item{con}{ If \samp{length(con)>0}, some check about the content of \samp{x} is done for some of the types. More precisely for (integer, numeric): all values must belong to the interval \samp{[con[1],con[2]]} and for (character), the set of possible \samp{character(1)} is described by \samp{con}.} 
    \item{mensaje}{ Some additional message to be issued before stopping in case of error.} 
    \item{fatal}{ what to do when discovering an inconsistency ? TRUE: this function prints the message and stops; FALSE: this function returns the message as a character.} 
    \item{na.rm}{ Must the \samp{NA} values be removed before checking? This is implemented only for the types integer, numeric, character and logical.} 
    \item{cla}{ The class to which the object is supposed to belong. When \samp{NULL} no check is made with respect to the class.} 
    \item{speci}{ Some additional specific check for some object types. The following possibilities are implemented:\cr - \samp{NULL} when not used,\cr - \samp{"named"} to indicate that a vector must be named,\cr - a \samp{matrix} with two rows and two column for matrix objects to indicate intervals for the row number (first row) and the column number (second row); \samp{NA} values means no check,\cr - a \samp{matrix} with \samp{ndim+1} rows and two columns for array objects to indicate intervals for the number of dimensions and the size of each of them; \samp{NA} values means no check.} 
} 
 
% 
\keyword{error} 
\keyword{object} 
\section{Future}{ 
(i) improves the treatment of 'NA's; (ii) allows the check of forbidden values 
} 
