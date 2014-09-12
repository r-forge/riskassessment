% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{text3acceptance} 
% 
\alias{text3acceptance} 
% 
\title{acceptance of word within a line} 
% 
\usage{ 
  text3acceptance(line,posi,
                  b.accepted=NULL,a.accepted=NULL,
                  b.rejected=NULL,a.rejected=NULL,
                  b.line=TRUE,e.line=TRUE) 
} 
% 
\examples{ 
  text3acceptance("Et pourquoi pas ?",c(1,3)); 
  text3acceptance("Et pourquoi pas ?",c(4,11),b.accepted=" "); 
  text3acceptance("Et pourquoi pas ?",c(4,11),b.accepted=letters); 
  text3acceptance("Et pourquoi pas ?",c(5,11),b.accepted=letters); 
  \dontrun{text3acceptance("Et",1:2,b.accepted="letters",b.rejected=LETTERS)} 
} 
% 
\details{ 
   
  \samp{posi[1]>=posi[2]} case is accepted. Outside 
  position are also accepted being valued to the bounds. 
   
} 
% 
\description{ 
   
  Indicates the acceptance of a sequence within a line 
  based on some positive and negative criteria. 
   
} 
% 
\value{ 
   
  \samp{TRUE} or \samp{FALSE}. 
   
} 
% 
 
\arguments{ 
    \item{line}{ A \samp{character(1)} within which is the sequence.} 
    \item{posi}{ The two positions within \samp{line} of the sequence.} 
    \item{b.accepted}{ A \samp{character} providing the surrounding characters before the words to determine the acceptance of the sequence.} 
    \item{a.accepted}{ A \samp{character} providing the surrounding characters after the words to determine the acceptance of the sequence.} 
    \item{b.rejected}{ A \samp{character} providing the surrounding characters before the words to determine the rejection of the sequence.} 
    \item{a.rejected}{ A \samp{character} providing the surrounding characters after the words to determine the rejection of the sequence.} 
    \item{b.line}{ Must sequences at the beginning of a line be accepted?} 
    \item{e.line}{ Must sequences at the end of a line be accepted?} 
} 
 
% 
\keyword{acceptance} 
\keyword{text} 
\seealso{ 
\code{\link{text3replace}} 
} 
