
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
explore8list <- function(lili,monitor=rbsa0$monitor$v)
#TITLE  returns the structure of a list in matrix
#DESCRIPTION
# Recursively explores the branches of a list, returning them into a 
# character matrix: a row for each branch, the columns describing
# some characteristics.\cr
# The columns are 
# 'number' (the number of this component from its branch),
# 'numbers' (the succesions of all numbers leading to 
#            this component separated with spaces),
# 'name' (<NA> if does not exist),
# 'names' (succession of names by default separated with spaces),
# 'level' (the branch level, i.e. a numbering of the branches),
# 'depth' (the branch depth from the root, implicitely 0 for the root),
# 'class' (the classes of the component separated with spaces).
#DETAILS
# \samp{rbsa0$sep1$v} is used to join the names, as a consequence it must
# not be present into the names. In case an error is issued.
#KEYWORDS IO
#INPUTS
#{lili} << The list structure to be explored.>>
#[INPUTS]
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{rbsa0$monitor$v} provided object as an example.>>
#VALUE
# The resulting character matrix (see the description section).
#EXAMPLE
# explore8list(rbsa0$lis1$v);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_04_01
#REVISED 10_06_23
#--------------------------------------------
{
# checking
if (monitor$chk$v) {
    object9(lili,"list",-1,mensaje="lili must be a list");
}
#
# starting the table
nta <- c("numbers","number","names","name","depth","level","classes");
tab <- matrix(NA,0,length(nta));
dimnames(tab) <- list(NULL,nta);
if (length(lili) == 0) { return(tab);
} else {
    for (ii in bf(lili)) {
        nom <- names(lili)[ii];
        if (is.null(nom)) { nom <- "<NA>";}
        if (nom=="") { nom <- "<NA>";}
        if (belong9(rbsa0$sep1$v,nom)) {
            erreur(list(rbsa0$sep1$v,nom),
                   "This name for the list comprises 'rbsa0$sep1$v' which is not accepted by 'explore8list'");
        }
        tab <- rbind(tab,c(ii,ii,nom,nom,1,1,NA));
    }
}
#
qq <- which(is.na(tab[,"classes"]));
if (length(qq) > 0) { qq <- qq[1];
} else { qq <- 0; }
# filling the table
niv <- 1;
while (qq > 0) {
    # the qq-th component must be explored
    coco <- paste("lili[[",
                  paste(strsplit(tab[qq,"numbers"]," ")[[1]],
                        collapse="]][["),
                  "]]",sep="");
    coco <- paste("coco <-",coco);
    eval(parse(text=coco));
    # completing the class
    tab[qq,"classes"] <- paste(class(coco),collapse=" ");
    # if a list adding more components
    if (tab[qq,"classes"] == "list") {
        pro <- length(strsplit(tab[qq,"numbers"]," ")[[1]])+1;
        niv <- niv + 1;
	for (ii in bf(coco)) {
	    nom <- names(coco)[ii];
	    if (is.null(nom)) { nom <- "<NA>";}
	    if (nom=="") { nom <- "<NA>";}
            noms <- paste(tab[qq,"names"],nom,sep=rbsa0$sep1$v);
            iii <- paste(tab[qq,"numbers"],ii);
	    tab <- rbind(tab,c(iii,ii,noms,nom,pro,niv,NA));
	}
    }
    # something more to explore?
    qq <- which(is.na(tab[,"classes"]));
    if (length(qq) > 0) { qq <- qq[1];
    } else { qq <- 0; }
}
# returning
tab;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
expr3extract <- function(cara,opa="(",cpa=")",
                         monitor=rbsa0$monitor$v)
#TITLE  extracts the contents of parentheses from a character
#DESCRIPTION returns a character vector with the 
#  contents of successive parentheses. Parentheses and text outside
#  parentheses are eliminated.
#DETAILS
# Parenthesis are defined with 'opa' and 'cpa' arguments
#KEYWORDS utilities
#INPUTS
#{cara}<<\samp{character(1)} to be considered.>>
#[INPUTS]
#{opa}<< opening tag (\samp{character(1)}).>>
#{cpa}<< closing tag (\samp{character(1)}).>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{rbsa0$monitor$v} provided object as an example.>>
#VALUE
# A character vector (of length zero when there is no parentheses)
#EXAMPLE
# expr3extract('avant (8h) ce n_est pas l_heure, plus tard que (9h) non plus')
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_12_17
#REVISED 10_08_12
#--------------------------------------------
{
# preparing
res <- character(0);
i.cara <- cara;
# degenerate case
if (length(cara)==0) { return(res);}
# checking
if (monitor$chk$v) {
    object9(cara,"character",1);
    object9( opa,"character",1);
    object9( cpa,"character",1);
}
# extracting
essaie <- TRUE;
jbd <- 0;
while (essaie) {
    inu <- strsplit(cara,opa,fixed=TRUE)[[1]][1];
    # this double check about inu is strange
    if (is.na(inu)) { essaie <- FALSE;
    } else {
      if (inu==cara) { essaie <- FALSE;
      } else {
          cara <- form3crop(cara,paste0(inu,opa),"");
          uti <- strsplit(cara,cpa,fixed=TRUE)[[1]][1];
          if (is.na(uti)) {
              erreur(list(i.cara,opa,cpa),"Non accepted case: wrong syntax of your input? Most common's a bad stopping tag");
          }
          if (uti==cara) { essaie <- FALSE;
          } else {
              jbd <- jbd + 1;
              res[jbd] <- uti;
              cara <- form3crop(cara,paste0(uti,cpa),"");
          }
      }
    }  
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
extract4list <- function(liste,comp="v") 
#TITLE returns a vector from a list
#DESCRIPTION
# from a list of lists sharing a common component 
# returns a named vector of the component values.
#DETAILS
# The object is supposed to be a list with all its 
# components being a list, every one having as component
# \samp{comp}. See the example.\cr
# The type of the vector is deduced from the first component.
#KEYWORDS 
#INPUTS
#{liste} <<The list of lists to be extracted.>>
#[INPUTS]
#{comp} <<The component to extract of the lists of \samp{liste}.>>
#VALUE
# A [possibly named] vector. When \samp{length(obj)==0}
# then \samp{NULL} is returned.
#EXAMPLE
# uu <- list(a=list(u=1,v="A"),b=list(u=2,v="B"));
# extract4list(uu);
# extract4list(uu,"u");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT adapation of extract8objet of /documair/
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 13_10_11
#REVISED 14_05_27
#--------------------------------------------
{
  # degenerate case
  if (length(liste)==0) { return(NULL);}
  # some checks
  object9(liste,"list",
             mensaje="'extract4list' works on 'lists'");
  # looping on the list components
  for (cc in bf(liste)) {
    xx <- liste[[cc]];
    if (is.null(xx[[comp]])) {
      print(names(xx));
      cat("The ",cc,"th component of 'liste' doesn't have name ",
          comp,"\n",sep="");
      stop();
    } else {
      if (cc==1) {
        res <- xx[[comp]];
      } else {
        res <- c(res,xx[[comp]]);
      }
    }
  }
  # returning
  names(res) <- names(liste);
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3normalize <- function(cha,redu=" ",bef=" ",aft=" ",
                       monitor=rbsa0$monitor$v)
#TITLE removes redundant characters into a character string
#DESCRIPTION
#  Removes redundant characters into a character string.
# The most immediate use (default) is to transform
# sequences of \samp{" "} into a simple \samp{" "}.
# More generally, removes all sequences of \samp{redu} before
# \samp{bef} or after \samp{aft}.
#DETAILS
# Every sequence of \samp{paste0(redu,bef)} is replaced with
# \samp{bef}, then every sequence of \samp{paste0(aft,redu)}
# is replaced with \samp{aft}.
#KEYWORDS IO
#INPUTS
#{cha} << The character to normalize, can be a vector.>>
#[INPUTS]
#{redu} << The character to remove, single or not.>>
#{bef} << The ante-tag, single or not.>>
#{aft} << The post-tag, single or not.>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{rbsa0$monitor$v} provided object as an example.>>
#VALUE
# The transformed character (with the same length than \samp{cha}).
#EXAMPLE
# form3normalize(" pour   voir  ");
# form3normalize(c(" A > B ","B > C","D"),bef=">");
# form3normalize(c(" A > B ","B > C","D"),bef=">",aft=">");
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_06_16
#REVISED 14_06_04
#--------------------------------------------
{
# checking
if (monitor$chk$v) {
    object9(cha, "character",-1,mensaje=" cha must be a character");
    object9(redu,"character", 1,mensaje="redu must be a character(1)");
    object9(bef, "character", 1,mensaje=" bef must be a character(1)");
    object9(aft, "character", 1,mensaje=" aft must be a character(1)");
}
if ((length(cha)>0) & (nchar(redu)>0)) {
    # removing before
    what <- paste0(redu,bef);
    ou <- grep(what,cha,fixed=TRUE);
    while (length(ou)>0) {
        cha <- gsub(what,bef,cha,fixed=TRUE);
        ou <- grep(what,cha,fixed=TRUE);
    }
    # removing before
    what <- paste0(aft,redu);
    ou <- grep(what,cha,fixed=TRUE);
    while (length(ou)>0) {
        cha <- gsub(what,aft,cha,fixed=TRUE);
        ou <- grep(what,cha,fixed=TRUE);
    }
}
# returning
cha;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3titre <- function(tit,box="un",
                           lbef=0,sbef=5,
                           saft=0,laft=1,
                           charbox=c("+","|","+","-",
                                     "+","|","+","-",
                                     "."),
                           alig=2,
                           caret=TRUE,
                           imp=TRUE,
                       monitor=rbsa0$monitor$v)
#TITLE  prints or prepares a title
#DESCRIPTION
# prints or prepares the character string \samp{tit} (can comprise
# several components) with more or less emphasis.
#DETAILS
#KEYWORDS print
#INPUTS
#{tit}<<the title to print, each component will be printed in a
#       different line.>>
#[INPUTS]
#{box} <<defines the type of box to put around the title. Possible
#        options are:
#         \cr\samp{"no"} for nothing,
#         \cr\samp{"un"} for underlined,
#         \cr\samp{"ov"} for overlined,
#         \cr\samp{"par"} for parentherized,
#         \cr\samp{"cor"} for the four corners,
#         \cr\samp{"unov"} for underlined and overlined,
#         \cr\samp{"box"} for a complete box.>>
#{lbef} <<if \samp{numeric(1)} defines the number of empty
#         lines before the title; when \samp{character}
#         provides the lines to add before the title.>>
#{sbef} <<either \samp{numeric(1)} or \samp{character(1)}
#         indicating the number of spaces or the characters
#         to introduce before the box.>>
#{saft} <<same as \samp{sbef} but after.>>         
#{laft} <<same as \samp{lbef} but after.>>         
#{charbox} <<nine single characters to define the box starting
#            after noon, and the extra 9th being the filling
#            character for multiple line titles.>>
#{alig} <<The aligment to be done by \samp{form3justify}: 
#         1 for left,2 for center and 3 for right.>>
#{caret} <<Indicates if \samp{\\n} must be added at the end
#          of each created line.>>
#{imp} << Printing is performed and nothing is returned.
#                If FALSE, the character string is returned 
#                (including possible new lines)>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{rbsa0$monitor$v} provided object as an example.>>
#VALUE
# According to \samp{imp}: nothing when printing is performed,
# a character string 
#EXAMPLE
# form3titre("Some Title");
# form3titre(c("The title","can comprise","several lines"),
#            box="box",lbef=4,laft=2);
# form3titre(c("And the box","can be incomplete", "as well!"),
#            box="cor");
# form3titre(c("The title","can comprise","several lines"),
#            box="box",lbef=4,laft=2,
#            charbox=c("*","+","/","=","*","+","/","="," "));
#REFERENCE
#SEE ALSO
#CALLING {form3repeat}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_24
#REVISED 10_07_25
#--------------------------------------------
{
# constants
#            1    2    3     4     5      6     7
boxes <- c("no","un","ov","par","cor","unov","box");
# checking
if (monitor$chk$v) {
    object9(caret,"logical",1);
    object9(tit,"character",-1);
    object9(box,"character",1);
    if (!belong9(box,boxes,TRUE)) {
        erreur(list(box,boxes),"'box' must be one of 'boxes'");
    }
    object9(lbef,c("numeric","character"),c(1,Inf));
    object9(laft,c("numeric","character"),c(1,Inf));
    object9(sbef,c("numeric","character"),1);
    object9(lbef,c("numeric","character"),1);
    object9(charbox,"character",9);
    if (!all(nchar(charbox)==1)) {
        erreur(charbox,"For the moment only unicharacter in 'charbox'");
    }
    object9(imp,"logical",1);
}
# the null case
if (void9(tit)) { return(character(0));}
# preparing
if (is.numeric(lbef)) { lbef <- rep(" ",max(0,round(lbef[1])));}
if (is.numeric(laft)) { laft <- rep(" ",max(0,round(laft[1])));}
if (is.numeric(sbef)) { sbef <- form3repeat(" ",max(0,round(sbef[1])));}
if (is.numeric(saft)) { saft <- form3repeat(" ",max(0,round(saft[1])));}
#
if (box %in% boxes[c(1,2,4)])   { charbox[c(7,8,1)]   <- NA;}
if (box %in% boxes[c(1,3,4)])   { charbox[c(3,4,5)]   <- NA;}
if (box %in% boxes[c(1,2,3,6)]) { charbox[c(1:3,5:7)] <- "";}
if (box %in% boxes[c(5)])       { charbox[c(4,8,2,6)] <- " ";}
#
lmax <- max(sapply(tit,nchar));
tit <- form3justify(tit,lmax,format=alig,carac=charbox[9]);
# producing
res <- character(0);
# first lines
for (ii in bf(lbef)) { res <- c(res,lbef[ii]);}
# overline
if (box %in% boxes[c(3,5,6,7)]) {
    lili <- sbef;
    lili <- paste0(lili,charbox[7]);
    lili <- paste0(lili,paste(rep(charbox[8],lmax),collapse=""));
    lili <- paste0(lili,charbox[1]);
    lili <- paste0(lili,saft);
    res <- c(res,lili);
}
# title lines
for (ii in bf(tit)) {
    lili <- sbef;
    lili <- paste0(lili,charbox[6]);
    lili <- paste0(lili,tit[ii]);
    lili <- paste0(lili,charbox[2]);
    lili <- paste0(lili,saft);
    res <- c(res,lili);
}
# underline
if (box %in% boxes[c(2,5,6,7)]) {
    lili <- sbef;
    lili <- paste0(lili,charbox[5]);
    lili <- paste0(lili,paste(rep(charbox[4],lmax),collapse=""));
    lili <- paste0(lili,charbox[3]);
    lili <- paste0(lili,saft);
    res <- c(res,lili);
}
# last lines
for (ii in bf(laft)) { res <- c(res,laft[ii]);}
# adding carriage returns
if (caret) { for (ii in bf(res)) {
    res[ii] <- paste0(res[ii],"\n");
}}
# returning
if (imp) {
    cat(res,sep="");
    invisible();
} else { return(res);}
# 
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
get8comp7list <- function(lili,tata,
                       monitor=rbsa0$monitor$v)
#TITLE  returns components from a list structure
#DESCRIPTION
# This is not a end-user function at all! To understand it, one must
# have in mind how works the function \samp{explore8list}.\cr
# Returns components from a list structure as a one level list.
# The list \samp{lili} must have been explored with \samp{explore8list}
# and the branch(es) to return are indicated through their line numbers
#  (\samp{tata}) in the table it generates.
#DETAILS
# Names of the produced list are the stacked names of the initial list 
# \samp{lili}.
#KEYWORDS IO
#INPUTS
#{lili} << The list structure components of which have to be extracted.>>
#{tata} << The lines of the table provided by \samp{explore8list}.>>
#[INPUTS]
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{rbsa0$monitor$v} provided object as an example.>>
#VALUE
# The resulting list with as many component as indicated rows.
#EXAMPLE
# uu <- list(A=1:3,
#            B=matrix(letters[1:20],nrow=5,dimnames=list(1:5,c("on","tw","th","fo"))),
#            C=list(a=1:3,b=letters,c=list(final=array(1:8,c(2,2,2)))));
# vv <- explore8list(uu);
# get8comp7list(uu,vv[7,])[[1]];
##
# uu <- list(1:3,
#            matrix(letters[1:20],nrow=5,dimnames=list(1:5,c("on","tw","th","fo"))),
#            list(a=1:3,b=letters,c=list(final=array(1:8,c(2,2,2)))));
# vv <- explore8list(uu);
# get8comp7list(uu,vv[7,])[[1]];
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_04_01
#REVISED 10_04_20
#--------------------------------------------
{
# in case of
if (length(tata)==0) {
    return(vector("list",0));
}
nta <- c("numbers","number","names","name","depth","level","classes");
# checking
if (monitor$chk$v) {
    object9(lili,"list",-1,mensaje="lili must be a list");
    #
    object9(tata,"character",-1,
               mensaje="'tata' must be a 'character'");
    if (!is.matrix(tata)) {
        object9(tata,"character",length(nta),
                   mensaje="When not a matrix, 'tata' must have the same length that 'nta'");
    } else {
        if (ncol(tata) != length(nta)) {
            erreur(tata,"The 'tata' matrix must have got as many columns as the length of nta");
        }
        if (is.null(dimnames(tata)[[2]])) {
            erreur(tata,"The 'tata' matrix must have named columns");
        }
        if (length(union(dimnames(tata)[[2]],nta))>length(nta)) {
            erreur(list(tata,nta),"The 'tata' matrix must have named columns with 'nta'");
        }
    }
}
# preparation
if (!is.matrix(tata)) {
   tata <- matrix(tata,1,length(nta));
   dimnames(tata) <- list(NULL,nta);
}
#
# starting the resulting list
res <- vector("list",nrow(tata));
nono <- tata[,"names"];
nunu <- tata[,"numbers"];
if (length(unique(nono))<length(nono)) {
    nono <- nunu;
}
names(res) <- nono;
for (ii in bc(nrow(tata))) {
    coco <- paste("lili[[",
                  paste(strsplit(nunu[ii],rbsa0$sep0$v)[[1]],
                        collapse="]][["),
                  "]]",sep="");
    coco <- paste("coco <-",coco);
    eval(parse(text=coco));
    res[[ii]] <- coco;
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
text3interval <- function(text,sub=c(l1=1,p1=1,l2=length(text),p2=Inf))
#TITLE transforms /sub/ into a consistent interval for the /text/
#DESCRIPTION from a text and an interval
# gives back a precised interval taking into account the lengths
# of \samp{text} and possible inconsistencies. When the interval is empty
# \samp{numeric(0)} is returned.
#DETAILS
# Definition of the subtext is flexible (outside positions
# are interpreted as minimum / maximum positions).
#KEYWORDS 
#INPUTS
#{text}<< A \samp{character} vector containing the text
# (a component, a line).>>
#[INPUTS]
#{sub} << A \samp{numeric(4)} indicating the portion of the \samp{text} to consider.
# More precisely (first line, first position within the line, last line and 
# last position in the last line. Can be a matrix with four columns as well.>>
#VALUE
# The resulting interval: \samp{matrix[nsub,4]} where \samp{nsub} is the number
# of intervals. When one interval is not consistent, \samp{NA} are introduced.
#EXAMPLE
# aa <- c(paste(letters,collapse=""),paste(LETTERS,collapse=""),paste(0:9,collapse=""));
# text3interval(aa);
# text3interval(aa,c(1,1,12,15));
# text3interval(aa,c(1,2,12,15));
# text3interval(aa,c(10,2,1,15));
# text3interval(aa,c(1,3,12,15));
# text3interval(aa,c(2,3,68,15));
# text3interval(aa,matrix(c(1,1,12,15,
#                           1,2,12,15,
#                           10,2,1,15,
#                           1,3,12,15,
#                           2,3,68,15),byrow=TRUE,ncol=4));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 13_10_17
#REVISED 13_10_29
#--------------------------------------------
{
  # checking
  object9(text,"character");
  if (is.matrix(sub)) {
    if (ncol(sub)!=4) {
      stop("A four columns matrix is expected");
    }
  } else {
      object9(sub,"numeric",len=4);
      sub <- matrix(sub,ncol=4);
  }
  # initialization
  res <- matrix(NA,nrow=nrow(sub),ncol=ncol(sub));
  # getting the lines
  i1 <- pmin(pmax(1,sub[,1]),length(text));
  i2 <- pmax(pmin(length(text),sub[,3]),1);
  # getting the positions
  p1 <- pmin(pmax(1,sub[,2]),nchar(text[i1]));
  p2 <- pmax(pmin(nchar(text[i2]),sub[,4]),1);
  # preparing the result
  res <- cbind(i1,p1,i2,p2);
  res[(i1>i2) | ((i1==i2)&(p1>p2)),] <- NA;
  dimnames(res) <- list(NULL,c("l1","p1","l2","p2"));
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
text3places8brackets <- function(text,bra=c("{","}"),
                                 col1=c(1,Inf),col2=c(1,Inf),
                                 which=c(1,Inf))
#TITLE returns the places of some pairs of brackets
#DESCRIPTION from a text,
# gives back the positions of a couple of brackets (opening and
# closing tags) indicated by \samp{bra} under the constraint
# that the first character of each delimiter be in the column
# interval of each line indicated with \samp{col1} (respectively
# \samp{col2}).
#DETAILS
# The tags cannot be upon two successive lines. The same line
# can have more than one tag. The result is built by an analysis
# of the result provided by \samp{text3places8word}.\cr
# When an opening tag is discovered, the following opening
# tag is considered only when a closing tag has been encountered.
# so the sequence \samp{"\{ toto \{tut\} bof\} \{deux\}"} will gives two
# contents, respectively \samp{" toto \{tut"} and \samp{"deux"}.\cr
# When an opening tags remains not consistently closed a fatal
# error is issued with some indications.
#KEYWORDS 
#INPUTS
#{text}<< A \samp{character} vector containing the text
# (a component, a line).>>
#[INPUTS]
#{bra}<< \samp{character(2)} the pair of tags to use.>>
#{col1} <<Positions within a line where the opening bracket has to be found.>>
#{col2} <<Positions within a line where the closing bracket has to be found.>>
#{which} <<Which occurences of \samp{tag} (not the line numbers)
# must be returned defined by the
# the number of the first one and the number of the last one.>>
#VALUE
# A three dimensions array. First dimension for the 
# interval associated to the bracket (see function \samp{text3interval} for
# an explanation; second dimension for the first and last
# position of the content of each bracket; third dimension for the number of
# of discovered brackets.\cr
# Be aware that the bracket tags are included (for a technical reason).
#EXAMPLE
# text3places8brackets(paste(letters,collapse=""),c("j","u"));
# text3places8brackets(paste(letters,collapse=""),c("ab","xyz"));
# text3places8brackets(c(" juste {un","deux ou trois} suffiront !"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE Think of a way to introduce "end of line" as a possible tag.
#AUTHOR J.-B. Denis
#CREATED 13_10_15
#REVISED 13_10_30
#--------------------------------------------
{
  # checking
  object9(text,"character");
  object9(bra,"character",len=2);
  object9(which,"numeric",len=2);
  object9( col1,"numeric",len=2);
  object9( col2,"numeric",len=2);
  # looking for opening positions
  ooo <- text3places8word(text,bra[1],which=which,column=col1);
  # looking for closing positions
  ccc <- text3places8word(text,bra[2],which=which,column=col2);
  # translating them into absolute rankings
  ooa <- matrix(NA,nrow(ooo),2);
  cca <- matrix(NA,nrow(ccc),2);
  for (ii in bc(nrow(ooo))) {
    ooa[ii,] <- c(text3ij2n(ooo[ii,1:2],text),
                  text3ij2n(ooo[ii,3:4],text));
  }
  for (ii in bc(nrow(ccc))) {
    cca[ii,] <- c(text3ij2n(ccc[ii,1:2],text),
                  text3ij2n(ccc[ii,3:4],text));
  }
  # initializing
  res <- array(NA,dim=c(2,2,nrow(ooa)),dimnames=list(c("li","co"),bra,NULL));
  # matching opening and closing
  nbb <- 0;
  while(nrow(ooa)>=1) {
    nbb <- nbb+1;
    # getting the next opening
    res[,1,nbb] <- ooo[1,1:2];
    # looking for the first associated closing
    fipo <- 1 + text3ij2n(ooo[1,3:4],text);
    bons <- which(cca[,1] >= fipo);
    cca <- cca[bons,,drop=FALSE];
    if (nrow(cca) == 0) {
      print(text);
      stop("'text3places8brackets' found an opening bracket without a closing one!");
    }
    ccc <- ccc[bons,,drop=FALSE];
    res[,2,nbb] <- ccc[1,3:4];
    # removing the closed opening brackets
    fipo <- 1 + text3ij2n(ccc[1,3:4],text);
    bons <- which(ooa[,1] >= fipo);
    ooa <- ooa[bons,,drop=FALSE];
    ooo <- ooo[bons,,drop=FALSE];
  }
  # removing non used brackets
  keep <- which(!is.na(res[1,1,]));
  res <- res[,,keep,drop=FALSE];
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
