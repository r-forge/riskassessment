% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{texts4text} 
% 
\alias{texts4text} 
% 
\title{returns a list of texts from a text} 
% 
\usage{ 
  texts4text(text,where,
             addbeg=TRUE,addend=TRUE,rmeli=TRUE) 
} 
% 
\examples{ 
  uu <- paste(letters,collapse=""); UU <- toupper(uu); 
  ww <- matrix(c(1,2,2,1,12,13,2,5,10),ncol=3,byrow=TRUE); 
  texts4text(c(uu),ww[1,,drop=FALSE]); 
  texts4text(c(uu),c(1,7,6)); 
  texts4text(c(uu),ww[1:2,]); 
  texts4text(c(uu,UU),ww); 
  texts4text(c(uu,"",UU),ww); 
  texts4text(c(uu,"",UU),ww,rmeli=FALSE); 
  texts4text(c(uu),ww); 
  texts4text(c(uu),ww[1,,drop=FALSE],addbeg=FALSE); 
  texts4text(c(uu),ww[1,,drop=FALSE],addbeg=FALSE,addend=FALSE); 
} 
% 
\details{ 
   
  When \samp{nrow(where)==0} an empty list is returned without 
  warning.\cr 
  The component number \samp{i} of the resulting list, is the portion of \samp{text} 
  in between the two positions indicated by rows \samp{i} and \samp{i+1} of 
  \samp{where} of course, it can comprise several lines, that is be a 
  character vector.\cr 
  The two positions defining a delimitation are included (i.e. second and third columns 
  of the \samp{where} matrix) in the tag. Nevertheless a split can be obtained at 
  position \samp{7} in row \samp{3} by the following row in matrix \samp{where}: 
  \samp{c(3,7,6)}. 
  When delimitations are not consistent, a fatal error is issued.\cr 
   
} 
% 
\description{ 
   
  from a text, 
  gives back a list of texts according to the delimitations 
  proposed into \samp{where}; typically \samp{where} can be deduced from 
  output of \samp{places4text6tags}. Empty lines can be removed.\cr 
  The idea is from a text comprising several lines, to get pieces of it 
  after eliminating the delimitating tags; when the tags have already been 
  identified since they are indicated through their positions with 
  the argument \samp{where}. A delimitation is given in each row of \samp{where}, 
  so according to the values of \samp{addbeg} and \samp{addend} the maximum 
  length of the returned list is \samp{nrow(where)-1}, \samp{nrow(where)} 
  or \samp{nrow(where)+1}. 
   
} 
% 
\value{ 
   
  A list of \samp{character}s each corresponding to an extracted text. 
   
} 
% 
 
\arguments{ 
    \item{text}{ A named \samp{character} vector containing the text (a component, a line).} 
    \item{where}{ A two or three column matrix (when a two column matrix, the second column is first duplicated as third column and substracted with 1 to generate a simple splitting at this point). The first column gives the line number; the second column, the first character of the position (excluded in the previous text component); the third column, the last character of the position (excluded in the next text component). Overlapping of the extraction is not admitted so the difference between the second and third column can be greater than one.} 
    \item{addbeg}{Add (or not) what is before the first position?} 
    \item{addend}{Add (or not) what is after the last position?} 
    \item{rmeli}{Must empty lines of the found texts (\samp{""}) must be eliminated?} 
} 
 
% 
\keyword{text} 
\keyword{text} 
