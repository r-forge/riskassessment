% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{file2list} 
% 
\alias{file2list} 
\alias{list2file} 
% 
\title{reads a file and transforms it in a list (of lists) of characters} 
% 
\usage{ 
  file2list(file,path="",
            clean=TRUE,ended=")STOP(",comment="#",
            skip=matrix(c(")SKIPPING(",")READING("),1),
            include=c(")FILE("),
            tags=rbsa0$tag1$v,
            sep=rbsa0$sep0$v,
            rsep="no_action",
            stag=c("/","/")
            ) 
  list2file(lili,file,path="",
            tags=rbsa0$tag1$v,
            stag=c("/",";","/"),
            comment="#",
            comments=character(0),
            ap=FALSE,
            monitor=rbsa0$monitor$v
            ) 
} 
% 
\examples{ 
  ## 
  ## file2list 
  sink("rbsa.list.txt") 
  cat("# comments can be included as well\n") 
  cat("<<A>>\n"); 
  cat("[[a]]/*/v 1*un deux trois\n"); 
  cat("[[b]]/*/v 1*2*3\n"); 
  cat("un uno one\n"); 
  cat("deux dos two\n"); 
  cat("trois tres three\n"); 
  cat("<<B>>\n"); 
  cat("[[a]] un deux trois\n"); 
  cat("# the following three are interesting\n"); 
  cat("[[b]] un  uno  one\n"); 
  cat(" deux dos two\n"); 
  cat("trois tres three\n"); 
  cat("<<C>> 1 2 3\n"); 
  sink(); 
  file2list("rbsa.list.txt"); 
  unlink("rbsa.list.txt"); 
  ## 
  ## list2file 
  list2file(rbsa0$lis1$v,"toto.txt"); 
  unlink("toto.txt"); 
  list2file(rbsa0$lis1$v,file=character(0)); 
} 
% 
\details{ 
   
  (list2file) 
  No use is made of the general constant 
  \samp{monitor$chk$v}. 
  Of course, the permission to write must exist in the proposed directory. 
   
} 
% 
\description{ 
   
   (file2list) 
  reads a conveniently tagged text file into nested lists. 
  It is just the linking of the two functions \samp{file2text} 
  and \samp{text2list}, see their comments for the description of the arguments. 
  \cr (list2file)  
  Transforms a list and writes it onto a text file. 
  The reverse operation of \samp{file2list} and just the successive calls 
  of \samp{list2text} and \samp{text2file}. See their own descriptions for 
  complete details. 
   
} 
% 
\value{ 
   
   (file2list) 
  a list [of lists [of lists [...] ] ] of character (possibly named) vectors 
  or matrices or arrays. 
  \cr (list2file)  
  Nothing but a file is created or modified when everything is right; except when\cr 
  \samp{file==character(0)}, in that case a character is returned. 
   
} 
% 
 
\arguments{ 
    \item{file}{ file to be read and transformed into a list.} 
    \item{lili}{ The list structure to be transformed and written in \samp{file}.} 
    \item{path}{ Directory containing the file.} 
    \item{clean}{Indicates if starting and ending spaces must be eliminated at first.} 
    \item{ended}{ To indicate the line at which to stop the reading.} 
    \item{comment}{At the beginning of a line, it indicates that this line must not be considered. More than one commenting character can be considered when it is a vector. For instance \samp{c("#","-")} means that lines starting with an hash or a hyphen are comment lines.} 
    \item{skip}{ To indicate set(s) of lines to be skipped. Must be a character matrix where the two columns correspond respectively to the opening and closing tags, and where each row is associate to a couple of tags. Tags are considered successively following the order of these matrix rows; that is skipping with the first row is performed, then with the remaining lines, skipping witht the second row is performed, and so on.} 
    \item{include}{ Tags to indicate a file (including possible path) by a \samp{character(1)} to include at this point its contents as a text file with the same tags specifications. Including files can be recursive. } 
    \item{tags}{ Character matrix with two columns indicating the opening and closing tags of the lists at different levels. Its row number gives the maximum number of levels. Opening tags must be different.} 
    \item{sep}{ Character sequence used to split the character vectors of every line. Notice that \samp{LF} is always considered as a separator.} 
    \item{rsep}{ Indicates if repetitions of \samp{sep} must be considered as significant or not; when significant \samp{""} values are introduced. If \samp{no_action} then the repetitions will be ignored.} 
    \item{stag}{ Two character strings indicating the tag to define different \samp{sep} for a given [sub]list. These two correspond to \samp{stag[c(1,3)]} of \samp{list2file} function.} 
    \item{comments}{Comments that the user want to be added at the beginning of the file. Moreover, the function will introduce its signature at the beginning of the file.  } 
    \item{ap}{Must the file be appended if it already exist?} 
    \item{monitor}{List of constants indicating the monitoring choices, see the \samp{rbsa0$monitor$v} provided object as an example.} 
} 
 
% 
\keyword{input/output} 
\keyword{file} 
\keyword{list} 
\keyword{input/output} 
