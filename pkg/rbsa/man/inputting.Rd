% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{inputting} 
% 
\alias{inputting} 
\alias{file2text} 
\alias{text2file} 
% 
\title{reads a file and transforms it in a single character} 
% 
\usage{ 
  file2text(file,path="",
            clean=TRUE,
            ended=")STOP(",
            comment="#",
            skip=matrix(c(")START_SKIPPING(",")END_SKIPPING(",
            "/*","*/"),ncol=2,byrow=TRUE),
            include=c(")FILE("),
            monitor=rbsa0$monitor$v
            ) 
  text2file(text,file,path="",
            ap=FALSE,
            monitor=rbsa0$monitor$v
            ) 
} 
% 
\examples{ 
  ## 
  ## file2text 
  sink("rbsa.text.txt") 
  cat("# comments can be included as well\n") 
  cat(" something\n"); 
  cat("/* skipping from here\n"); 
  cat("blablabla\n"); 
  cat("  */ until here (this line is ALSO eliminated\n"); 
  cat(" interesting:\n"); 
  cat("un dos tres\n"); 
  cat(")STOP(\n"); 
  cat(" It is finished!\n"); 
  cat(" Don't insist!\n"); 
  sink(); 
  file2text("rbsa.text.txt"); 
  unlink("rbsa.text.txt"); 
  ## 
  ## text2file 
  text2file(letters,"toto.txt"); 
  unlink("toto.txt"); 
  text2file(letters,character(0)); 
} 
% 
\details{ 
   
  All tags are supposed to be in the first position of the line after 
  cleaning when asked.\cr 
  Successive performed actions are : (i) cleaning the lines, i.e. removing starting and 
  ending spaces, (ii) eliminating commented lines, (iii) eliminating 
  lines after a 'stop', (iv) including indicated files and (v) skipping 
  sequences of lines. 
   
} 
% 
\description{ 
   
   (file2text) 
  reads a conveniently tagged file to produce a vector of characters 
  where all non used lines are eliminated. Each component of the resulting 
  vector is associated to an original line. 
  \cr (text2file)  
  The reverse operation of \samp{file2text}. This function is not 
  very tricky, it was written for completeness. 
   
} 
% 
\value{ 
   
   (file2text) 
  a character of length, the number of retained lines. 
  \cr (text2file)  
  Nothing but a file is created or completed when everything is right -or- 
  according to the value of \samp{file}, a character is returned. 
   
} 
% 
 
\arguments{ 
    \item{file}{ file which have to be read and transformed into a list.} 
    \item{text}{ The \samp{character} to be written into \samp{file}.} 
    \item{path}{ Directory containing the file.} 
    \item{clean}{Indicates if starting and ending spaces must be eliminated at first.} 
    \item{ended}{ The tag indicating the line from which to stop the reading.} 
    \item{comment}{At the beginning of a line, it indicates that this line must not be considered. More than one commenting character can be considered when it is a vector. For instance \samp{c("#","\%")} means that lines starting with an hash or a percent are comment lines. If no comment line must be filtered, just give \samp{comment} the value of \samp{character(0)}.} 
    \item{skip}{ To indicate set(s) of lines to be skipped. Must be a character matrix where the two columns correspond respectively to the opening and closing tags, and where each row is associate to a couple of tags. Tags are considered successively following the order of these matrix rows; that is skipping with the first row is performed, then with the remaining lines, skipping witht the second row is performed, and so on.} 
    \item{include}{ Tags to indicate a file (including possible path) by a \samp{character(1)} to include at this point its contents as a text file with the same tags specifications. Including files can be recursive. } 
    \item{monitor}{List of constants indicating the monitoring choices, see the \samp{rbsa0$monitor$v} provided object as an example.} 
    \item{ap}{Must the file be appended if it already exist?} 
} 
 
% 
\keyword{input/output} 
\keyword{file} 
\keyword{text} 
\keyword{input/output} 
