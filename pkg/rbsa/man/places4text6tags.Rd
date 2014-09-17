% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{places4text6tags} 
% 
\alias{places4text6tags} 
% 
\title{returns the places of a series of tags within a /text/} 
% 
\usage{ 
  places4text6tags(text,tags,check=FALSE) 
} 
% 
\examples{ 
  places4text6tags("Bonjour Monsieur","on"); 
  places4text6tags("Bonjour Monsieur",c("on","ons","mon")); 
  \dontrun{places4text6tags("Bonjour Monsieur",c("on","ons","mon"),check=TRUE)}; 
  places4text6tags(rbsa0$text1$v,c("1","2","7")); 
  places4text6tags(rbsa0$text2$v[1:3],"t"); 
  places4text6tags(rbsa0$text3$v,"uu"); 
} 
% 
\details{ 
   
  One tags cannot be upon two successive lines. The same line 
  can have more than one tag. This is a clear generalization of the 
  function \samp{text3places8word} which is called to elaborate the result.\cr 
  If some tags have an intersection in the text, the case is considered 
  as no acceptable and a fatal error is issued. For instance in 
  "The computation is performed" the two tags "perf" and "formed" are 
  not compatible; that is\cr 
  \samp{places4text6tags("is performed",c("perf","formed"))} 
  stops with a fatal error. 
   
} 
% 
\description{ 
   
  From a text, 
  independently for a series of tags, gives back the positions 
  where they are in the text. A position is given by four 
  values \samp{(li1,co1,li2,co2)}, respectively the numbers of 
  \samp{li}ne and \samp{co}lumn of the first character and last 
  character of the tag. \cr 
  Non-overlapping between tags can be checked. 
   
} 
% 
\value{ 
   
  A five column data frame: a line for each found tag indicated by a factor; 
  four columns giving respectively the \samp{line}, 
  the \samp{beg}inning and \samp{end}ing positions plus the fifth column 
  to indicate the corresponding tag value. When they 
  exist \samp{names(tags)} are used for the row dimnames. 
   
} 
% 
 
\arguments{ 
    \item{text}{ A \samp{character} vector containing the text.} 
    \item{tags}{ \samp{character()} the series of tags to be considered.} 
    \item{check}{ Must overlapping be checked?} 
} 
 
% 
\keyword{position} 
\keyword{tags} 
\keyword{text} 
