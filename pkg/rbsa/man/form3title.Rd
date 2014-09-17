% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{form3title} 
% 
\alias{form3title} 
% 
\title{prints or prepares a title} 
% 
\usage{ 
  form3title(tit,empha=3,indent=2+2*empha,imp=TRUE) 
} 
% 
\examples{ 
  for (ii in 0:8) {form3title("Some Title",ii,imp=TRUE)}; 
} 
% 
\description{ 
   
  prints or prepares the character string \samp{tit} 
  with more or less emphasis. 
  This function is a shortcut of the hidden function \samp{form3titre}, 
  with some specialized calls. 
   
} 
% 
\value{ 
   
  either nothing or a character string according to imp 
   
} 
% 
 
\arguments{ 
    \item{tit}{the title to print (just one line)} 
    \item{empha}{ Level of emphasizing.\cr (0): single line without carriage return\cr (1): single line\cr (2): underlined\cr (3): underlined and overlined\cr (4): (2) + 1 line before\cr (5): (3) + 1 line after\cr (6): (2) + 2 lines before and after\cr (7): corners + 1 line before and after (plus surrounding)\cr (8): box + 1 lines before and after (plus surrounding)\cr} 
    \item{indent}{ Number of spaces to introduce before the title} 
    \item{imp}{ Printing is performed and nothing is returned. If FALSE, the character string is returned (including possible new lines)} 
} 
 
% 
\keyword{print} 
\keyword{format} 
\keyword{title} 
