
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
file2text <- function(file,path="",
                      clean=TRUE,
                      ended=")STOP(",
                      comment="#",
                      skip=matrix(c(")START_SKIPPING(",")END_SKIPPING(",
                                    "/*","*/"),ncol=2,byrow=TRUE),
                      include=c(")FILE("),
                       monitor=rbsa0$monitor$v
                     )
#TITLE  reads a file and transforms it in a single character
#DESCRIPTION
# reads a conveniently tagged file to produce a vector of characters
# where all non used lines are eliminated. Each component of the resulting
# vector is associated to an original line.
#DETAILS
# All tags are supposed to be in the first position of the line after
# cleaning when asked.\cr
# Successive performed actions are : (i) cleaning the lines, i.e. removing starting and
# ending spaces, (ii) eliminating commented lines, (iii) eliminating
# lines after a 'stop', (iv) including indicated files and (v) skipping 
# sequences of lines.
#KEYWORDS IO
#ALIAS inputting
#INPUTS
#{file} << file which have to be read and transformed into a list.>>
#[INPUTS]
#{path} << Directory containing the file.>>
#{clean} <<Indicates if starting and ending spaces must be eliminated at first.>>
#{ended} << The tag indicating the line from which to stop the reading.>>
#{comment} <<At the beginning of a line, it indicates that this line must not be
#          considered. More than one commenting character can be considered when
#          it is a vector. For instance \samp{c("#","\%")} means that
#          lines starting with an hash or a percent are comment lines.
#          If no comment line must be filtered, just give \samp{comment} the
#          value of \samp{character(0)}.>>
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
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{rbsa0$monitor$v} provided object as an example.>>
#VALUE
# a character of length, the number of retained lines.
#EXAMPLE
# sink("rbsa.text.txt")
# cat("# comments can be included as well\n")
# cat(" something\n");
# cat("/* skipping from here\n");
# cat("blablabla\n");
# cat("  */ until here (this line is ALSO eliminated\n");
# cat(" interesting:\n");
# cat("un dos tres\n");
# cat(")STOP(\n");
# cat(" It is finished!\n");
# cat(" Don't insist!\n");
# sink();
# file2text("rbsa.text.txt");
# unlink("rbsa.text.txt");
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 08_12_10
#REVISED 14_08_04
#--------------------------------------------
{
#
# checking
if (monitor$chk$v) {
    object9(file,"character",1,mensaje="file must indicate the name of one file");
    #
    object9(path,"character",1,mensaje="path must indicate the name of one directory");
    #
    if (path!="") { fifi <- paste(path,file,sep="/");} else { fifi <- file;}
    if (is.na(file.info(fifi)$size)) { erreur(fifi,"This file seems not to exist.");}
    #
    object9(clean,"logical",1,mensaje="clean must be a logical(1)");
    #
    object9(ended,"character",1,mensaje="ended must be a character of length one");
    #
    object9(comment,"character",-1,mensaje="comment must be a character");
    #
    object9(skip,"character",c(2,Inf),mensaje="skip must be a CHARACTER matrix with two columns");
    if (!is.matrix(skip)) {erreur(skip,"skip must be a character MATRIX of two columns");}
    if (ncol(skip)!=2) {erreur(skip,"skip must be a character matrix OF TWO COLUMNS");}
    #
    object9(include,"character",1,mensaje="include must be a character of length one");
    #
}
#
# reading the proposed file
if (path!="") {file <- paste(path,file,sep="/");}
lu <- readLines(file);
#
# cleaning
if (clean) { for (ii in bf(lu)) {
    # removing framing spaces
    lu[ii] <- form3crop(lu[ii]," "," ");
}}
#
# removing the lines after a possible stop
sto <- which(substr(lu,1,nchar(ended))==ended);
if (length(sto)>0) {
    lu <- lu[1:(sto[1]-1)];
}
#
# removing the empty lines
lu <- lu[nchar(lu)>0];
#
# removing the commented lines
if (!void9(comment)) {
  mu <- filter8text(lu,comment,remove=TRUE,
                         exact=8,lower=FALSE,
                         monitor=monitor);
}
#
# removing the skipped lines
for (nn in bc(nrow(skip))) {
    deb <- skip[nn,1]; fin <- skip[nn,2];
    debu <- substr(lu,1,nchar(deb)) == deb;
    fini <- substr(lu,1,nchar(fin)) == fin;
    sk1 <- apply(outer(which(debu)  ,bf(lu),"<="),2,sum);
    sk2 <- apply(outer(which(fini)+1,bf(lu),"<="),2,sum);
    nsk <- ((sk1-sk2) < 1);
    lu <- lu[nsk];
}
#
# including the indicated files
inclus <- which(substr(lu,1,nchar(include))==include);
for (ii in bf(inclus)) {
    jj <- length(inclus) + 1 - ii;
    kk <- inclus[jj];
    fi <- strsplit(lu[kk]," ",fixed=TRUE)[[1]][2];
    plus <- Recall(fi,path=path,
                      clean=clean,
                      ended=ended,
                      comment=comment,
                      skip=skip,
                      include=include);
    plus <- c(lu[bc(kk-1)],plus);
    if (kk < length(lu)) { plus <- c(plus,lu[(kk+1):length(lu)]);}
    lu <- plus;
}
#
# returning
lu;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
text2file <- function(text,file,path="",
                      ap=FALSE,
                       monitor=rbsa0$monitor$v
                     )
#TITLE  writes a character onto a file
#DESCRIPTION
# The reverse operation of \samp{file2text}. This function is not
# very tricky, it was written for completeness.
#KEYWORDS IO
#INPUTS
#{text} << The \samp{character} to be written into
#          \samp{file}.>>
#{file} << file to be written. According to \samp{ap} when already exist, 
# it will be destroyed and recreated or completed.
# When the \samp{file} is \samp{character(0)}, no file is considered but the
# corresonding \samp{character} is returned.
# >>
#[INPUTS]
#{path} << Directory containing the file.>>
#{ap} <<Must the file be appended if it already exist?>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{rbsa0$monitor$v} provided object as an example.>>
#VALUE
# Nothing but a file is created or completed when everything is right -or-
# according to the value of \samp{file}, a character is returned.
#EXAMPLE
# text2file(letters,"toto.txt");
# unlink("toto.txt");
# text2file(letters,character(0));
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 08_02_13
#REVISED 14_06_24
#--------------------------------------------
{
# checking
if (monitor$chk$v) {
    object9(text,"character",-1,mensaje="'text' must be a character");
    #
    object9(file,"character",c(0,1),mensaje="file must indicate the name of one file");
    #
    if (!void9(path)) {
      path <- dipa(path);
      object9(path,"character",1,mensaje="path must indicate the name of one directory");
      #
      if (is.na(file.info(path)$size)) { erreur(path,"This directory seems not to exist.");}
    }
    #
    object9(ap,"logical",1,mensaje="'ap' must be a single logical");
}
#
# The returning case
if (void9(file)) { return(text);}
#
# opening the proposed file
if (!is.null(path)) { file <- paste0(path,file);}
sink(file,append=ap);
#
# writting each component
for (ii in bf(text)) {
    cat(text[ii],"\n");
}
#
# closing the file
sink();
#
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
