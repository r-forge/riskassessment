% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{outputting} 
% 
\alias{outputting} 
\alias{open8graph} 
\alias{close8graph} 
\alias{open8text} 
\alias{close8text} 
% 
\title{opens/closes graph/text files} 
% 
\usage{ 
  open8graph(prefix=rbsa0$fpx$v,...) 
  close8graph(mensaje=character(0),monitor=rbsa0$monitor$v) 
  open8text(append=TRUE) 
  close8text(mensaje=character(0),monitor=rbsa0$monitor$v) 
} 
% 
\examples{ 
  open8graph("my_graph"); 
  close8graph(); 
  open8graph(5); 
  close8graph(); 
} 
% 
\details{ 
   
   (open8graph) 
  The file opened for storing the graph is named with 
  two components separated with dots: the prefix and the suffix. 
  The prefix is either given by the first argument or \cr 
  \samp{paste(rbsa0$fpx$v,prefix,sep=".")}; the suffix 
  is rbsa0$mgr$v associated to the type of the graph (either 'pdf' 
  or 'png'). 
  \cr(close8graph) 
  This action is conditioned by the value of 
  \samp{rbsa0$mfi$v}. The pausing is conditionned by (i) 
  a non empty \samp{mensaje} and (ii) \samp{monitor$pau$v}. 
  \cr(open8text) 
  Usual derivation is done with \samp{sink} until \samp{close8text} is called. 
  \cr(close8text) 
  This action is conditioned by the value of 
  \samp{rbsa0$mfi$v}. The pausing is conditionned by (i) 
  a non empty \samp{mensaje} and (ii) \samp{monitor$pau$v}. 
   
} 
% 
\description{ 
   
   (open8graph) 
  According to the global constant \samp{rbsa0$mfi$v} a 
  graphical device is open or not. 
  To be called before plotting something that ones want 
  to keep under \samp{rbsa0$mgr$v} type. 
  \cr (close8graph)  
  Closes the file 
  open by \samp{open8graph}. When \samp{monitor$pau$v} is \samp{TRUE}, 
  a pause is issued. 
  \cr (open8text)  
  Opens the standard output text for /rbsa/. 
  According to the global constant \samp{rbsa0$mfi$v} the 
  standard output text of rbsa is open (in 
  append mode) or not. The name of this file is provided 
  by the constant \samp{rbsa0$fou$v}. 
  Must be called before printing something ones want 
  to keep on file. 
  \cr (close8text)  
  Closing the output file \samp{rbsa0$fou$v} according to \samp{rbsa0$mfi$v}. A pause allowing 
  to stop the process is issued if \samp{rbsa0$pau$v} is \samp{TRUE}. 
   
} 
% 
\value{ 
   
   (open8graph) 
  \samp{0} when \samp{is.character(prefix)} and 
  \samp{prefix+1} when \samp{is.numeric(prefix)}. 
  The last option is intended to generate a series of numbered graph 
  file names. 
  \cr (close8graph)  
  Nothing but the actions indicated in the description field are performed. 
  \cr (open8text)  
  nothing but the indicated actions are performed 
  \cr (close8text)  
  nothing but the actions indicated in the description field are performed 
   
} 
% 
 
\arguments{ 
    \item{prefix}{ When \samp{numeric}, it is used with three digits to complete the constant \samp{rbsa0$fpx$v}. If not, must be a character giving the complete prefix to use before the suffix.} 
    \item{\dots}{ argument(s) to be transmitted to the opening device of graphics. Quite useful for specific character and picture sizes, or to get more than one graph into the file.} 
    \item{mensaje}{ Message to display on the terminal. When empty, no message will be displayed.} 
    \item{monitor}{ List of monitoring constants, see \samp{rbsa0$monitor$v} to know its structure.} 
    \item{append}{ Must the current file rbsa0$fou$v be continued ?} 
} 
 
% 
\keyword{input/output} 
\keyword{graph} 
\keyword{open} 
\keyword{close} 
\keyword{text} 
\keyword{input/output} 
