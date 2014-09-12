% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{text-indexing} 
% 
\alias{text-indexing} 
\alias{text3ij2n} 
\alias{text3n2ij} 
% 
\title{gives the absolute positions of (row,column) positions within a /text/} 
% 
\usage{ 
  text3ij2n(ij,text,flexible=TRUE) 
  text3n2ij(n,text,flexible=TRUE) 
} 
% 
\examples{ 
  ## 
  ## text3ij2n 
  aa <- c(paste(letters,collapse=""),paste(LETTERS,collapse=""),paste(0:9,collapse="")); 
  text3ij2n(c(1,3),aa); 
  text3ij2n(c(2,1),aa); 
  text3ij2n(rbind(c(1,3),c(2,1)),aa); 
  ## 
  ## text3n2ij 
  aa <- c(paste(letters,collapse=""),paste(LETTERS,collapse=""),paste(0:9,collapse="")); 
  text3n2ij(text3ij2n(c(1,3),aa),aa); 
  text3n2ij(text3ij2n(c(2,1),aa),aa); 
  text3n2ij(text3ij2n(rbind(c(1,3),c(2,1)),aa),aa); 
} 
% 
\details{ 
   
  Definition of the positions is flexible (outside positions 
  is interpreted as minimum / maximum positions) or not, according to \samp{flexible}. 
   
} 
% 
\description{ 
   
   (text3ij2n) 
  returns numerical values equal to the rank of a position when all 
  lines of the \samp{text} are concatenated without separators. 
  \cr (text3n2ij)  
  returns the (line, column) position associated to an absolute position when the 
  lines of the \samp{text} are concatanated.\cr 
  In fact, the reverse of \samp{text3ij2n}. 
   
} 
% 
\value{ 
   
   (text3ij2n) 
  The resulting ranks associated to the rows of \samp{ij}. 
  \cr (text3n2ij)  
  The resulting positions associated to \samp{n}. When \samp{length(n)==1} 
  it is a \samp{numeric(2)} if not a matrix with two columns. 
   
} 
% 
 
\arguments{ 
    \item{ij}{ A \samp{numeric(2)} indicating (line,column) of the position. May also be a matrix with two columns.} 
    \item{text}{ A \samp{character} vector containing the text (a component, a line).} 
    \item{n}{ A \samp{numeric(1)} indicating the absolute position. May also be a vector. Values outside the possible range are bounded to the extrema according to \samp{flexible}.  } 
    \item{flexible}{ When \samp{TRUE} values of \samp{n} outside the acceptable range are accepted and replaced with first or last positions.} 
} 
 
% 
\keyword{indexing} 
\keyword{text} 
\keyword{indexing} 
\keyword{indexing} 
