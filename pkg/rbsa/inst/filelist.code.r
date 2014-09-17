
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
file2list <- function(file,path="",
                      clean=TRUE,ended=")STOP(",comment="#",
                      skip=matrix(c(")SKIPPING(",")READING("),1),
                      include=c(")FILE("),
                      tags=rbsa0$tag1$v,
                      sep=rbsa0$sep0$v,
                      rsep="no_action",
                      stag=c("/","/")
                     )
#TITLE  reads a file and transforms it in a list (of lists) of characters
#DESCRIPTION
# reads a conveniently tagged text file into nested lists.
# It is just the linking of the two functions \samp{file2text}
# and \samp{text2list}, see their comments for the description of the arguments.
#DETAILS
#KEYWORDS IO
#INPUTS
#{file} << file to be read and transformed into a list.>>
#[INPUTS]
#{path} << Directory containing the file.>>
#{clean} <<Indicates if starting and ending spaces must be eliminated at first.>>
#{ended} << To indicate the line at which to stop the reading.>>
#{comment} <<At the beginning of a line, it indicates that this line must not be
#          considered. More than one commenting character can be considered when
#          it is a vector. For instance \samp{c("#","-")} means that
#          lines starting with an hash or a hyphen are comment lines.>>
#{skip} << To indicate set(s) of lines to be skipped. Must be a character matrix
#          where the two columns correspond respectively to the opening and 
#          closing tags, and where each row is associate to a couple of tags.
#          Tags are considered successively following the order of these matrix rows;
#          that is skipping with the first row is performed, then with the remaining
#          lines, skipping witht the second row is performed, and so on.>>
#{include} << Tags to indicate a file (including possible path) by
#             a \samp{character(1)} to include at this point its contents
#             as a text file with the same tags specifications.
#             Including files can be recursive. >>
#{tags} << Character matrix with two columns indicating the opening and closing
#          tags of the lists at different levels. Its row number gives the maximum
#          number of levels. Opening tags must be different.>>
#{sep} << Character sequence used to split the character vectors of every
# line. Notice that \samp{LF} is always considered as a separator.>>
#{rsep} << Indicates if repetitions of \samp{sep} must be considered as 
#          significant or not; when significant \samp{""} values are introduced.
#          If \samp{no_action} then the repetitions will be ignored.>>
#{stag} << Two character strings indicating the tag to define different \samp{sep} for a given
#           [sub]list. These two correspond to \samp{stag[c(1,3)]} of \samp{list2file} function.>>
#VALUE
# a list [of lists [of lists [...] ] ] of character (possibly named) vectors
# or matrices or arrays.
#EXAMPLE
# sink("rbsa.list.txt")
# cat("# comments can be included as well\n")
# cat("<<A>>\n");
# cat("[[a]]/*/v 1*un deux trois\n");
# cat("[[b]]/*/v 1*2*3\n");
# cat("un uno one\n");
# cat("deux dos two\n");
# cat("trois tres three\n");
# cat("<<B>>\n");
# cat("[[a]] un deux trois\n");
# cat("# the following three are interesting\n");
# cat("[[b]] un  uno  one\n");
# cat(" deux dos two\n");
# cat("trois tres three\n");
# cat("<<C>> 1 2 3\n");
# sink();
# file2list("rbsa.list.txt");
# unlink("rbsa.list.txt");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 08_12_10
#REVISED 14_06_23
#--------------------------------------------
{
# everything, including the check are deported
# to the called functions
#
# from file to character
res <- file2text(file,
                 path=path,clean=clean,ended=ended,
                 comment=comment,skip=skip,
                 include=include
                );
#
# from character to list
res <- text2list(res,                      #
                 tags=tags,sep=sep,
                 rsep=rsep,stag=stag
                );

# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
list2file <- function(lili,file,path="",
                      tags=rbsa0$tag1$v,
                      stag=c("/",";","/"),
                      comment="#",
                      comments=character(0),
                      ap=FALSE,
                      monitor=rbsa0$monitor$v
                     )
#TITLE  transforms a list and writes it onto a text file
#DESCRIPTION
#  Transforms a list and writes it onto a text file.
# The reverse operation of \samp{file2list} and just the successive calls
# of \samp{list2text} and \samp{text2file}. See their own descriptions for
# complete details.
#ALIAS list2file
#DETAILS
# No use is made of the general constant
# \samp{monitor$chk$v}. 
# Of course, the permission to write must exist in the proposed directory.
#KEYWORDS IO
#INPUTS
#{lili} << The list structure to be transformed and written in
#          \samp{file}.>>
#{file} << file to be written. According to \samp{ap} when already exist, 
# it will be destroyed and recreated or completed. But when equal to \samp{character(0)}
# no file is considered and the result is returned as a \samp{character} vector.
# >>
#[INPUTS]
#{path} << Directory containing the file.>>
#{tags} << Character matrix with two columns indicating the opening and closing
#          tags of the [sub]lists at different levels. Its row numbers gives the maximum
#          accepted number of levels. Opening tags must be different.>>
#{stag} << Three character strings indicating the tagging to define the separator for each
#          character vector \samp{stag[2]} between \samp{stag[1]} and \samp{stag[3]}.>>
#{comment} <<At the beginning of a line, it indicates that this line must not be
#          considered.>>
#{comments} <<Comments that the user want to be added at the beginning of the file.
#             Moreover, the function will introduce its signature at the beginning
#             of the file.  >>
#{ap} <<Must the file be appended if it already exist?>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{rbsa0$monitor$v} provided object as an example.>>
#VALUE
# Nothing but a file is created or modified when everything is right; except when\cr
# \samp{file==character(0)}, in that case a character is returned.
#EXAMPLE
# list2file(rbsa0$lis1$v,"toto.txt");
# unlink("toto.txt");
# list2file(rbsa0$lis1$v,file=character(0));  
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_03_25
#REVISED 14_06_23
#--------------------------------------------
{
#
# no checking (will be done in text2file)
#
# creating the intermediary character
texta <- list2text(lili,tags=tags,
                        stag=stag,
                        comment=comment,
                        comments=comments,
                        monitor=monitor
                  );
#
# creating the file and returning
text2file(texta,file=file,
          path=path,ap=ap,
          monitor=monitor
         );
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
