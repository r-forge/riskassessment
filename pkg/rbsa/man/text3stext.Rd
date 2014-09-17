% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{text3stext} 
% 
\alias{text3stext} 
% 
\title{returns a subtext} 
% 
\usage{ 
  text3stext(text,sub=c(l1=1,p1=1,l2=Inf,p2=Inf)) 
} 
% 
\examples{ 
  aa <- c(paste(letters,collapse=""),paste(LETTERS,collapse=""),paste(0:9,collapse="")); 
  text3stext(aa); 
  text3stext(aa,c(1,12,1,15)); 
  text3stext(aa,c(1,12,2,15)); 
  text3stext(aa,c(1,12,3,15)); 
  # In this last example, the last character of line 2 is kept (flexibility)! 
  text3stext(aa,c(2,68,3,15)); 
} 
% 
\details{ 
   
  Definition of the subtext is flexible (outside positions 
  are interpreted as minimum / maximum positions). 
   
} 
% 
\description{ 
   
  from a text, 
  gives back a portion according to the \samp{sub} argument. 
  Delimiting positions are included, so \samp{sub=c(1,1,1,1)} 
  gives back the first character of \samp{text}. 
   
} 
% 
\value{ 
   
  The resulting sub-text 
   
} 
% 
 
\arguments{ 
    \item{text}{ A \samp{character} vector containing the text (a component, a line).} 
    \item{sub}{ A \samp{numeric(4)} indicating the portion of the \samp{text} to consider; can be a matrix but only one row is allowed. See the function \samp{text3interval} for details.} 
} 
 
% 
\keyword{text} 
\keyword{text} 
