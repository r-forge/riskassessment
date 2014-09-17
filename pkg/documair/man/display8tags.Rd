% 
% created by /documair/ at 14_09_15@12:32 
% 
\name{display8tags} 
% 
\alias{display8tags} 
% 
\title{returns the contents of a 'tags' object} 
% 
\usage{ 
  display8tags(tags=documair0$tags,
               what="values",imp=FALSE) 
} 
% 
\details{ 
   
  More details on the possibilities offered with argument \samp{what} 
  are obtained into \samp{documair0$tags$d}. 
   
} 
% 
\description{ 
   
  Typically, \samp{tags} is either \samp{documair0$tags} or 
  \samp{documair0$tgs} which defines the tagging of the R code 
  to introduce the documentation to be tackled by /documair/.\cr 
  Such a function is provided to help the user in seeing the 
  present constants, and possibly to change their value. 
   
} 
% 
\value{ 
   
  A character or a list according to \samp{what} 
   
} 
% 
 
\arguments{ 
    \item{tags}{ List to be displayed.} 
    \item{what}{a \samp{character(1)} indicating what must be returned. The acceptable values are \samp{names}, \samp{definitions}, \samp{usage}, \samp{content}, \samp{preparation}, \samp{title}  and \samp{values}. For user convenience, the argument is lowercased before checked, and only the first character is taken into consideration.} 
    \item{imp}{ indicates if an adapted printing of the results must be done.} 
} 
 
% 
\keyword{display} 
\keyword{tags} 
\section{Future}{ 
Make an equivalent function to change the tags values 
} 
