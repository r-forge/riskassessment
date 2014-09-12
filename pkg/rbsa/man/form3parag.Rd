% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{form3parag} 
% 
\alias{form3parag} 
% 
\title{prints or prepares paragraphes from a character vector.} 
% 
\usage{ 
  form3parag(texte,titre=-1,
             wid=60,fli=NULL,sep=1,
             jus=1,trunc=TRUE,
             ed="",ef="",
             monitor=rbsa0$monitor$v,
             imp=TRUE) 
} 
% 
\examples{ 
  form3parag(c("My Title","My important first sentence.","Last one!")); 
  form3parag(rbsa0$text2$v,wid=20); 
} 
% 
\description{ 
   
  prints or prepares a character string \samp{texte} 
  as a small formatted text.\cr 
  Each component is supposed to be a 
  paragraph but the first one can be 
  considered as a title. 
   
} 
% 
\value{ 
   
  either nothing or a character string according to \samp{imp} 
   
} 
% 
 
\arguments{ 
    \item{texte}{The text to print (character vector).} 
    \item{titre}{ When > -2 indicates that the first component is a title (if not a simple paragraph). Then the absolute value of \samp{titre} gives the emphasize to put on the title. Notice that the title is not splitted in several lines as are the other components according to 'wid' value.} 
    \item{wid}{ The maximum width for each line (in characters) without including indentation and frames.} 
    \item{fli}{ When NULL, the first line of each paragraph (in fact the second if there is a title) is issued as a standard line. If not, \samp{fli[1]} spaces are added before and the considered width is \samp{fli[2]} (not including the added spaces). Also this means that your already wrote \samp{fli[3]} characters on the first line; this last possibility can be used only when there are no title and for the the first component. For instance, a French paragraph will be issued with \samp{fli=c(5,wid,0)}. The possibility of modifying wid for the first line can be of use when adding the name of an item first.} 
    \item{sep}{ Number of lines separating each paragraph.} 
    \item{jus}{ Type of justification (1=left, 2=centred, 3=left).} 
    \item{trunc}{ Must truncation be done when a word is greater than the proposed wid?} 
    \item{ed}{ Framing at the beginning of each line.} 
    \item{ef}{ Framing at the end of each line.} 
    \item{monitor}{ List of monitoring constants, see \samp{rbsa0$monitor$v} to know its structure.} 
    \item{imp}{ Printing is performed and nothing is returned. If FALSE, the character string is returned (including possible new lines)} 
} 
 
% 
\keyword{print} 
\keyword{format} 
\keyword{paragraph} 
