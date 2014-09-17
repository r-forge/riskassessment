
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssform3title <- function(tit,empha=3,indent=2+2*empha,imp=TRUE)
#TITLE  prints or prepares a title
#DESCRIPTION
# prints or prepares the character string \samp{tit}
# with more or less emphasis.
# This function is a shortcut of the hidden function \samp{sssform3titre},
# with some specialized calls.
#DETAILS
#KEYWORDS print
#INPUTS
#{tit}<<the title to print (just one line)>>
#[INPUTS]
#{empha} << Level of emphasizing.\cr
#          (0): single line without carriage return\cr
#          (1): single line\cr
#          (2): underlined\cr
#          (3): underlined and overlined\cr
#          (4): (2) + 1 line before\cr
#          (5): (3) + 1 line after\cr
#          (6): (2) + 2 lines before and after\cr
#          (7): corners + 1 line before and after (plus surrounding)\cr
#          (8): box + 1 lines before and after (plus surrounding)\cr>>
#{indent} << Number of spaces to introduce before the title>>
#{imp} << Printing is performed and nothing is returned.
#                If FALSE, the character string is returned 
#                (including possible new lines)>>
#VALUE
# either nothing or a character string according to imp
#EXAMPLE
# for (ii in 0:8) {sssform3title("Some Title",ii,imp=TRUE)};
#REFERENCE
#SEE ALSO
#CALLING {sssform3repeat}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_24
#REVISED 10_09_15
#--------------------------------------------
{
# adjusting
empha <- round(max(0,min(8,empha)));
# preparing the argument for sssform3titre
if (length(tit)>1) { tit <- paste(tit,collapse=" ");}
if (empha == 0) { tit <- paste0("<",tit,">")}
if (empha == 1) { tit <- paste0("(*)",tit,"(*)")}
sbef <- round(max(indent,0));
caret <- (empha != 0);
saft=""; lbef <- 0; laft <- 0;
box <- "no";
if (empha == 2) { box <- "un";}
if (empha == 3) { box <- "unov";}
if (empha == 4) { box <- "unov"; lbef <- 1;}
if (empha == 5) { box <- "unov"; laft <- 1; lbef <- 1;}
if (empha == 6) { box <- "unov"; laft <- 2; lbef <- 2;}
if (empha == 7) {
  box <- "cor" ; laft <- 1; lbef <- 1;
  tit<- paste0(" ",tit," ");
}
if (empha == 8) {
  box <- "box" ; laft <- 1; lbef <- 1;
  tit<- paste0(" ",tit," ");
}
# calling sssform3titre
res <- sssform3titre(tit,box=box,
                  lbef=lbef,sbef=sbef,
                  saft=saft,laft=laft,
                  charbox=c("+","|","+",
                            "-","+","|",
                            "+","-"," "),
                  alig=2,caret=caret,imp=imp);
# returning
if (imp) { return(invisible());}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssform3titre <- function(tit,box="un",
                           lbef=0,sbef=5,
                           saft=0,laft=1,
                           charbox=c("+","|","+","-",
                                     "+","|","+","-",
                                     "."),
                           alig=2,
                           caret=TRUE,
                           imp=TRUE,
                       monitor=sssrbsa0$monitor$v)
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
#{alig} <<The aligment to be done by \samp{sssform3justify}: 
#         1 for left,2 for center and 3 for right.>>
#{caret} <<Indicates if \samp{\\n} must be added at the end
#          of each created line.>>
#{imp} << Printing is performed and nothing is returned.
#                If FALSE, the character string is returned 
#                (including possible new lines)>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{sssrbsa0$monitor$v} provided object as an example.>>
#VALUE
# According to \samp{imp}: nothing when printing is performed,
# a character string 
#EXAMPLE
# sssform3titre("Some Title");
# sssform3titre(c("The title","can comprise","several lines"),
#            box="box",lbef=4,laft=2);
# sssform3titre(c("And the box","can be incomplete", "as well!"),
#            box="cor");
# sssform3titre(c("The title","can comprise","several lines"),
#            box="box",lbef=4,laft=2,
#            charbox=c("*","+","/","=","*","+","/","="," "));
#REFERENCE
#SEE ALSO
#CALLING {sssform3repeat}
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
    sssobject9(caret,"logical",1);
    sssobject9(tit,"character",-1);
    sssobject9(box,"character",1);
    if (!sssbelong9(box,boxes,TRUE)) {
        ssserreur(list(box,boxes),"'box' must be one of 'boxes'");
    }
    sssobject9(lbef,c("numeric","character"),c(1,Inf));
    sssobject9(laft,c("numeric","character"),c(1,Inf));
    sssobject9(sbef,c("numeric","character"),1);
    sssobject9(lbef,c("numeric","character"),1);
    sssobject9(charbox,"character",9);
    if (!all(nchar(charbox)==1)) {
        ssserreur(charbox,"For the moment only unicharacter in 'charbox'");
    }
    sssobject9(imp,"logical",1);
}
# the null case
if (sssvoid9(tit)) { return(character(0));}
# preparing
if (is.numeric(lbef)) { lbef <- rep(" ",max(0,round(lbef[1])));}
if (is.numeric(laft)) { laft <- rep(" ",max(0,round(laft[1])));}
if (is.numeric(sbef)) { sbef <- sssform3repeat(" ",max(0,round(sbef[1])));}
if (is.numeric(saft)) { saft <- sssform3repeat(" ",max(0,round(saft[1])));}
#
if (box %in% boxes[c(1,2,4)])   { charbox[c(7,8,1)]   <- NA;}
if (box %in% boxes[c(1,3,4)])   { charbox[c(3,4,5)]   <- NA;}
if (box %in% boxes[c(1,2,3,6)]) { charbox[c(1:3,5:7)] <- "";}
if (box %in% boxes[c(5)])       { charbox[c(4,8,2,6)] <- " ";}
#
lmax <- max(sapply(tit,nchar));
tit <- sssform3justify(tit,lmax,format=alig,carac=charbox[9]);
# producing
res <- character(0);
# first lines
for (ii in sssbf(lbef)) { res <- c(res,lbef[ii]);}
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
for (ii in sssbf(tit)) {
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
for (ii in sssbf(laft)) { res <- c(res,laft[ii]);}
# adding carriage returns
if (caret) { for (ii in sssbf(res)) {
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
sssform3display <- function(x,pau=FALSE,cat=FALSE,...)
#TITLE  displays with its name any object
#DESCRIPTION
#  displays any object after giving the name of
# the variable containing it. A ssspause can be introduced
# to give the opportunity to scrutinize the result.  
#DETAILS
#KEYWORDS print
#INPUTS
#{x}<<The object to print.>>
#[INPUTS]
#{pau} << Must a ssspause be performed after the display?>>
#{cat} << Must the printing be done with 'cat' instead of print?>>
#{\dots} <<possible arguments for the print function.>>
#VALUE
# a print (or cat) is done and \samp{x} is returned
#EXAMPLE
# uu <- "azerty";
# sssform3display(uu);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_09_17
#REVISED 09_10_12
#--------------------------------------------
{
cat("<< Displaying ",deparse(substitute(x))," >>\n");
if (cat) { cat(x,"\n");
} else { print(x,...);}
if (pau) { ssspause("affichage");}
# returning
x;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
ssspause <- function(what="",mensaje=NULL,top=0,ans=NULL) 
#TITLE  pauses the program until an answer is given
#DESCRIPTION
# This function issues a ssspause with a message allowing 
# to stop the process or to continue it (without or with and 
# according to the answer given by the user).
#DETAILS
# The answer provided by the user is interpreted as
#  a \samp{character(1)}.\cr
# When \samp{top} is \samp{NA} the call is ignored.\cr
# When \samp{top} is \samp{NULL} no stopping is possible
#      but the answer is returned. This answer can be
#      anything when \samp{is.na(ans)} but a void answer
#      is refused. This answer can be anything including
#      nothing when \samp{is.null(ans)}. The answer is
#      returned.\cr
# When \samp{top} is \samp{numeric}, \samp{ans} is not considered.
#       an empty answer allows the process to follow
#       and no void answers stop the process.\cr
# When \samp{top} and \samp{ans} are \samp{character}s, 
#      the only accepted answers are a component of \samp{top} (to 
#      stop the process) or a component of \samp{ans} (to be returned).\cr
# When \samp{top} is a \samp{character} and \samp{is.null(ans) | is.na(ans)}
#      stopping is made when the answer is one of the components of
#      \samp{top}, other answers will be returned.\cr
#KEYWORDS IO
#INPUTS
#[INPUTS]
#{what} << Short message commenting the ssspause>>
#{mensaje} << A possible longer message to give details. When
#          \samp{NULL} a shortened message is issued.>>
#{top} << Precises the rules to follow for the execution to be stopped
#         (see the \samp{details} section).>>
#{ans} << Precises the accepted answers
#         (see the \samp{details} section).>>
#VALUE
# When the process is not stopped, returns the answer provided
# by the user.
#EXAMPLE
# ssspause("Time for lunch!",top=NA);
# \dontrun{ssspause("Time for lunch?")}
# \dontrun{ssspause("Look at your results before the process continue",top=NULL)}
# \dontrun{ssspause("Can we continue?",top=c("n","N","no","NO"),ans=c("y","Y","yes","YES"))}
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_25
#REVISED 14_07_31
#--------------------------------------------
{
  # no action
  if(length(top)==1) { if (is.na(top)) { return(invisible());}}
  # writing the question
  if (!sssvoid9(mensaje)) {cat(">>> (",mensaje,")\n");}
  if (!sssvoid9(what))  {cat(">>> --------------> ",what,"\n");}
  # precising the possibilities and getting the answer
  if (is.numeric(top)) {
    # raw ssspause
    cat(">>> 'Enter' to continue | any key(s) +'Enter' to stop \n");
    quoi <- scan(what="character",nmax=1);
    if (length(quoi) != 0) {
      stop("(...YOU decided to stop the job...)",call.=FALSE);
    } else {
      return(invisible());
    }
  } else {
    if (is.null(top)) {
      # no stopping is possible
      if (is.null(ans)) {
        # any answer is possible
        cat(">>> Give your answer and 'Enter' \n");
        quoi <- scan(what="character",nmax=1);
        return(quoi);
      } else {
        if (is.na(ans[1])) {
          # any non void answer is possible
          cat(">>> Give a non-empty answer and 'Enter' \n");
          quoi <- scan(what="character",nmax=1);
          while (length(quoi) == 0) {
            cat("EMPTY ANSWER is not admitted\n");
            quoi <- scan(what="character",nmax=1);
          }
          return(quoi);
        } else {
          # a restricted answer is required
          cat(">>> Give an answer among the possible and 'Enter' \n");
          cat(">>> Possible are: /",paste(ans,collapse="/"),"/\n",sep="");
          quoi <- scan(what="character",nmax=1);
          while((length(quoi)==0) || (!(quoi %in% as.character(ans)))) {
            cat("POSSIBLE ANSWERS are in the following list:\n");
            cat(">>> /",paste(ans,collapse="/"),"/\n",sep="");
            quoi <- scan(what="character",nmax=1);
          }
          return(quoi);
        }
      }     
    } else {
      # possibly stopping
      if (is.null(ans)) {
        # any answer is possible
        cat(">>> Give an answer and 'Enter' \n");
        cat(">>> To stop: /",paste(top,collapse="/"),"/\n",sep="");
        quoi <- scan(what="character",nmax=1);
        if ((length(quoi)>0) && (quoi %in% top)) {
          stop("(...YOU decided to stop the job...)",call.=FALSE);
        } else {
          return(quoi);
        }
      } else {
        if (is.na(ans[1])) {
          # any non void answer is possible
          cat(">>> Give a non empty answer and 'Enter' \n");
          cat(">>> To stop: /",paste(top,collapse="/"),"/\n",sep="");
          quoi <- scan(what="character",nmax=1);
          while (length(quoi) == 0) {
            cat("EMPTY ANSWER is not admitted\n");
            quoi <- scan(what="character",nmax=1);
          }
          if (quoi %in% top) {
            stop("(...YOU decided to stop the job...)",call.=FALSE);
          }
          return(quoi);
        } else {
          # a restricted answer is required
          topans <- union(top,ans);
          if (length(topans)!=(length(ans)+length(top))) {
            stop("Possible answers and stopping answers are not disjoint!");
          }
          cat(">>> Give an answer among the possible and 'Enter' \n");
          cat(">>> To stop: /",paste(top,collapse="/"),"/ ",sep="");
          cat("or: /",paste(ans,collapse="/"),"/\n",sep="");
          quoi <- scan(what="character",nmax=1);
          while((length(quoi)==0) || (!(quoi %in% as.character(topans)))) {
            cat("POSSIBLE ANSWERS are in the following list:\n");
            cat(">>> To stop: /",paste(top,collapse="/"),"/ ",sep="");
            cat("or: /",paste(ans,collapse="/"),"/\n",sep="");
            quoi <- scan(what="character",nmax=1);
          }
          if (quoi %in% top) {
            stop("(...YOU decided to stop the job...)",call.=FALSE);
          }
          return(quoi);
        }
      }
    }
  }
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssvma2text <- function(x,xsep=sssrbsa0$sep1$v,
                       monitor=sssrbsa0$monitor$v)
#TITLE  transforms a vector (or matrix, or array) into a character
#DESCRIPTION
# from a vector, or a matrix, or
# an array, builds a \samp{character} vector. More or less the 
# inverse function of \samp{ssstext2vma}. This vector is the first
# component of a returned list, the second component of the list 
# gives the type (\samp{vector}, \samp{matrix} or \samp{array}) of \samp{x},
# the converted object.
#DETAILS
# When some dimnames exist, the possible missing
# ones will be added.
#KEYWORDS IO
#INPUTS
#{x} << The object to transform.>>
#[INPUTS]
#{xsep} << \samp{character(1)} to be use for the separations.>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{sssrbsa0$monitor$v} provided object as an example.>>
#VALUE a list with two components: \samp{[[1]]} the coded character vector and
# \samp{[[2]]} the type according to \samp{ssstext2vma}.
#EXAMPLE
#####
## vectors
# sssvma2text(letters);
# x <- letters; names(x) <- LETTERS;
# xx <- sssvma2text(x);
# ssstext2vma(xx[[1]],xx[[2]]);
# sssvma2text(character(0));
#####
## matrices
# x <- matrix(1:20,4);
# sssvma2text(x);
# dimnames(x) <- list(letters[1:4],LETTERS[1:5]);
# sssvma2text(x);
# x1 <- matrix(NA,3,0);
# xx1 <- sssvma2text(x1);
# ssstext2vma(xx1[[1]],xx1[[2]]);
# dimnames(x1) <- list(c("i","ii","iii"),NULL);
# xx1 <- sssvma2text(x1);
# ssstext2vma(xx1[[1]],xx1[[2]]);
#####
## arrays
# x <- array(1:24,2:4);
# sssvma2text(x);
# dimnames(x) <- list(1:2,c("i","ii","iii"),c("I","II","III","IV"));
# sssvma2text(x,xsep="|||");
# x0 <- array(NA,c(3,0,2));
# dimnames(x0) <- list(1:3,NULL,c("i","ii"));
# xx0 <- sssvma2text(x0);
# ssstext2vma(xx0[[1]],xx0[[2]]);
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_03_29
#REVISED 10_06_29
#--------------------------------------------
{
# checking
if (monitor$chk$v) {
    #
    sssobject9(xsep,"character",1,mensaje="must indicate the character string of separation");
    #
    if (!is.vector(x) &
        !is.matrix(x) &
        !is.array(x)) {
        ssserreur(class(x),"'x' must be a vector or a matrix or an array!");
    }
}
#
res <- vector("list",2);
names(res) <- c("character","type");
#
if (is.array(x)&(!is.matrix(x))) {
    # dealing with an array
    res[[1]] <- c(as.character(dim(x)),xsep);
    if (is.null(dimnames(x))) {
        res[[2]] <- sssrbsa0$vma$v["a"];
    } else {
        nna <- dimnames(x);
        for (hh in sssbf(nna)) {
            if (is.null(nna[[hh]])) {
                nna[[hh]] <- sssbc(dim(x)[hh]);
            }
        }
        if (is.null(names(nna))) {
            for (ii in sssbf(nna)) {
                res[[1]] <- c(res[[1]],nna[[ii]],xsep);
            }
        res[[2]] <- sssrbsa0$vma$v["A"];
        } else {
            res[[1]] <- c(res[[1]],names(nna),xsep);
            for (ii in sssbf(nna)) {
                res[[1]] <- c(res[[1]],nna[[ii]],xsep);
            }
        res[[2]] <- sssrbsa0$vma$v["B"];
        }
    }
    res[[1]] <- c(res[[1]],as.character(x));
} else {
    if (is.matrix(x)) {
        # dealing with a matrix
        if (is.null(dimnames(x))) {
            res[[1]] <- character(0);
            for (ii in sssbc(nrow(x))) {
                res[[1]] <- c(res[[1]],x[ii,],xsep);
            }
            res[[1]] <- res[[1]][-length(res[[1]])];
            res[[2]] <- sssrbsa0$vma$v["m"];
        } else {
            nna <- dimnames(x);
            for (hh in sssbf(nna)) {
                if (is.null(nna[[hh]])) {
                    nna[[hh]] <- sssbc(dim(x)[hh]);
                }
            }
            res[[1]] <- c(as.character(nna[[2]]),xsep);
            for (ii in sssbc(nrow(x))) {
                res[[1]] <- c(res[[1]],nna[[1]][ii],as.character(x[ii,]),xsep);
            }
            res[[1]] <- res[[1]][-length(res[[1]])];
            res[[2]] <- sssrbsa0$vma$v["p"];
        }
    } else {
        # dealing with a simple vector
        if (is.null(names(x))) {
            res[[1]] <- as.character(x);
            res[[2]] <- sssrbsa0$vma$v["v"];
        } else {
            res[[1]] <- c(names(x),as.character(x));
            res[[2]] <- sssrbsa0$vma$v["V"];
        }
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssform3join <- function(vcara,none="-",
                       OPA="{",CPA="}",
                       opa="(",cpa=")",sep="+",
                       imp=FALSE,cr=imp)
#TITLE  formats a series of names or a similar list
#DESCRIPTION returns a scalar character of the components
#  of a \samp{character} surrounded by any kind of parenthesis
#  (each  and the whole)
#  separated with the same separator.\cr
# Easy to undestand when applying the examples.
#DETAILS
#KEYWORDS print format
#INPUTS
#{vcara}<<Character vector to be considered.>>
#[INPUTS]
#{none}<< The internal result when \samp{vcara} is length zero.
#         When it is \samp{character(0)} then the 
#         global parenthesis are not given contrary
#         when it is \samp{""}.>>
#{OPA}<< The opening parenthesis to surround the entire list.>>
#{CPA}<< The closing parenthesis to surround the entire list.>>
#{opa}<< The opening parenthesis to surround each component.>>
#{cpa}<< The closing parenthesis to surround each component.>>
#{sep}<< The symbol to separate each component.>>
#{imp}<< Must the result be printed (with cat) or returned?>>
#{cr}<< Must a line feed be added at the end?>>
#VALUE
# A character string or nothing when imp is TRUE
#EXAMPLE
# sssform3join(letters[1:4])
# sssform3join(NULL);
# sssform3join(NULL,NULL);
# sssform3join(NULL,character(0));
# sssform3join(NULL,"");
#REFERENCE
#SEE ALSO sssform3split
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_06_25
#REVISED 08_10_30
#--------------------------------------------
{
vcara <- as.character(vcara);
vide <- (length(vcara) == 0);
if (length(vcara) == 1) { if (vcara == "") {
    vide <- TRUE;
}}
if (vide) {
    res <- none;
    if (length(none)==0) {
        OPA <- CPA <- "";
    }
} else {
    for (hd in sssbf(vcara)) {
        hdc <- paste0(opa,vcara[hd],cpa);
        if (hd == 1) { res <- hdc;
        } else { res <- paste0(res,sep,hdc);}
    }
}
res <- paste0(OPA,res,CPA);
if (cr) { res <- paste0(res,"\n");}
if(!imp) { return(res);}
cat(res);
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
ssslist2file <- function(lili,file,path="",
                      tags=sssrbsa0$tag1$v,
                      stag=c("/",";","/"),
                      comment="#",
                      comments=character(0),
                      ap=FALSE,
                      monitor=sssrbsa0$monitor$v
                     )
#TITLE  transforms a list and writes it onto a text file
#DESCRIPTION
#  Transforms a list and writes it onto a text file.
# The reverse operation of \samp{sssfile2list} and just the successive calls
# of \samp{ssslist2text} and \samp{ssstext2file}. See their own descriptions for
# complete details.
#ALIAS ssslist2file
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
#            see the \samp{sssrbsa0$monitor$v} provided object as an example.>>
#VALUE
# Nothing but a file is created or modified when everything is right; except when\cr
# \samp{file==character(0)}, in that case a character is returned.
#EXAMPLE
# ssslist2file(sssrbsa0$lis1$v,"toto.txt");
# unlink("toto.txt");
# ssslist2file(sssrbsa0$lis1$v,file=character(0));  
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
# no checking (will be done in ssstext2file)
#
# creating the intermediary character
texta <- ssslist2text(lili,tags=tags,
                        stag=stag,
                        comment=comment,
                        comments=comments,
                        monitor=monitor
                  );
#
# creating the file and returning
ssstext2file(texta,file=file,
          path=path,ap=ap,
          monitor=monitor
         );
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
ssstext2file <- function(text,file,path="",
                      ap=FALSE,
                       monitor=sssrbsa0$monitor$v
                     )
#TITLE  writes a character onto a file
#DESCRIPTION
# The reverse operation of \samp{sssfile2text}. This function is not
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
#            see the \samp{sssrbsa0$monitor$v} provided object as an example.>>
#VALUE
# Nothing but a file is created or completed when everything is right -or-
# according to the value of \samp{file}, a character is returned.
#EXAMPLE
# ssstext2file(letters,"toto.txt");
# unlink("toto.txt");
# ssstext2file(letters,character(0));
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
    sssobject9(text,"character",-1,mensaje="'text' must be a character");
    #
    sssobject9(file,"character",c(0,1),mensaje="file must indicate the name of one file");
    #
    if (!sssvoid9(path)) {
      path <- sssdipa(path);
      sssobject9(path,"character",1,mensaje="path must indicate the name of one directory");
      #
      if (is.na(file.info(path)$size)) { ssserreur(path,"This directory seems not to exist.");}
    }
    #
    sssobject9(ap,"logical",1,mensaje="'ap' must be a single logical");
}
#
# The returning case
if (sssvoid9(file)) { return(text);}
#
# opening the proposed file
if (!is.null(path)) { file <- paste0(path,file);}
sink(file,append=ap);
#
# writting each component
for (ii in sssbf(text)) {
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

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
ssslist2text <- function(lili,
                      tags=sssrbsa0$tag1$v,
                      stag=c("/",";","/"),
                      comment="#",
                      comments=character(0),
                       monitor=sssrbsa0$monitor$v
                     )
#TITLE  transforms a list into a character
#DESCRIPTION
# The reverse operation of \samp{ssstext2list}. The list must be a
# rsbsa-list, that is complying
# some properties : all components of the [sub-]lists must be either
# a list or a [named] character vector/matrix/array. The number of nested list
# must not be greater than the number of rows in matrix \samp{tags}.
# Every list component must be named.\cr
# The idea is to get a character compatible with \samp{ssstext2list} to produce back
# the object \samp{lili}.\cr
# Some comments are added to the content of the list by the function itself and/or
# according to the whish of the user, this is why an escaping character
# for comments is asked.
#DETAILS
# The character strings of the structure must not comprise
# the \samp{sssrbsa0$sep0$v} constant but this global constant can be
# conveniently modified. The same for \samp{sssrbsa0$sep1$v}.\cr
# Use is made of the general constant \samp{monitor$chk$v} for the checking.
#KEYWORDS IO
#INPUTS
#{lili} << The list structure to be transformed into a 
#          \samp{character}.>>
#[INPUTS]
#{tags} << Character matrix with two columns indicating the opening and closing
#          tags of the [sub]lists at different levels. Its row numbers gives the maximum
#          accepted number of levels. Opening tags must be different.>>
#{stag} << Three character strings indicating the tagging to define the separator for each
#          character vector \samp{stag[2]} between \samp{stag[1]} and \samp{stag[3]}.>>
#{comment} <<At the beginning of a line, it indicates that this line must not be
#          considered.>>
#{comments} <<Comments that the user want to be added at the beginning of the file.>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{sssrbsa0$monitor$v} provided object as an example.>>
#VALUE
# The resulting character.
#EXAMPLE
# ssslist2text(sssrbsa0$lis1$v);
# ssstext2list(ssslist2text(sssrbsa0$lis1$v));
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_03_25
#REVISED 10_09_13
#--------------------------------------------
{
# checking
if (monitor$chk$v) {
    sssobject9(lili,"list",-1,mensaje="lili must be a list");
    #
    sssobject9(tags,"character",c(2,Inf),mensaje="tags must be a CHARACTER matrix with two columns");
    if (!is.matrix(tags)) {ssserreur(tags,"tags must be a character MATRIX of two columns");}
    if (ncol(tags)!=2) {ssserreur(tags,"tags must be a character matrix OF TWO COLUMNS");}
    if (length(unique(tags[,1]))!= nrow(tags)) { ssserreur(tags,"Opening tags are not unique!");}
    #
    sssobject9(stag,"character",3,mensaje="stag must be a character of length three");
    if (any(nchar(stag) == 0)) { ssserreur(stag,"stag must not have zero length components");}
    if (length(grep(" ",stag[c(1,3)])) > 0) { ssserreur(stag,"stag must not comprise spaces in first and/or third position");}
    #
    sssobject9(comment,"character",1,mensaje="'comment' must be a character of length one");
    #
    sssobject9(comments,"character",-1,mensaje="'comment' must be a character of any length");
}
# 
# preparing
res <- character(0);
#
# issuing the starting comments
res <- c(res,comment);
res <- c(res,paste(comment,"  This character was created on",sssnow("d"),"by ssslist2file of rbsa package"));
res <- c(res,paste(comment,"  from the object :",deparse(substitute(x))));
res <- c(res,comment);
res <- c(res,comment);
for (ii in sssbf(comments)) {
    res <- c(res,paste(comment,comment,sssrbsa0$sep0$v,sssrbsa0$sep0$v,comments[ii]));
}
res <- c(res,comment);
res <- c(res,comment);
#
# processing the list to translate it
if (length(lili) == 0) {
    res <- c(res,paste(comment,"The proposed list was of length zero!"));
    res <- c(res,comment);
} else {
    # exploring the list
    liliex <- sssexplore8list(lili);
    # checking the list-vma nature 
    if(!all(liliex[,"classes"] %in% c("list",
                                      "logical","integer","numeric","character",
                                      "matrix","array"))) {
        ssserreur(liliex,"The 'lili' list is not composed of list/vector/matrix/array");
    }
    # checking the existence of names for all levels
    if (any(is.na(liliex[,"name"]))) {
        ssserreur(liliex,"Not all 'lili' components have got a name");
    }
    # checking the levels
    xlev <- max(as.numeric(liliex[,"depth"]));
    if (xlev > nrow(tags)) {
        ssserreur(list(liliex,tags),"'lili' has got too many levels for the proposed 'tags'");
    }
    #
    # ordering the table
    uv <- order(liliex[,"numbers"]);
    # writing down the file
    for (ii in uv) {
        niv <- as.numeric(liliex[ii,"depth"]);
        if (niv == 1) { res <- c(res,paste0(comment,sssform3repeat("=",20)));}
        if (niv == 2) { res <- c(res,comment);}
        rrr <- paste0(tags[niv,1],liliex[ii,"name"],tags[niv,2]);
        if (liliex[ii,"classes"] != "list") {
            coco <- sssget8comp7list(lili,liliex[ii,],monitor=monitor)[[1]];
            caca <- sssvma2text(coco,sssrbsa0$sep1$v);
            rrr <- paste0(rrr,stag[1],stag[2],stag[3],caca$type);
            res <- c(res,rrr);
            res <- c(res,paste(caca$character,collapse=stag[2]));
        } else {
            res <- c(res,rrr);
        }
    }
}
#
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssfile2text <- function(file,path="",
                      clean=TRUE,
                      ended=")STOP(",
                      comment="#",
                      skip=matrix(c(")START_SKIPPING(",")END_SKIPPING(",
                                    "/*","*/"),ncol=2,byrow=TRUE),
                      include=c(")FILE("),
                       monitor=sssrbsa0$monitor$v
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
#            see the \samp{sssrbsa0$monitor$v} provided object as an example.>>
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
# sssfile2text("rbsa.text.txt");
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
    sssobject9(file,"character",1,mensaje="file must indicate the name of one file");
    #
    sssobject9(path,"character",1,mensaje="path must indicate the name of one directory");
    #
    if (path!="") { fifi <- paste(path,file,sep="/");} else { fifi <- file;}
    if (is.na(file.info(fifi)$size)) { ssserreur(fifi,"This file seems not to exist.");}
    #
    sssobject9(clean,"logical",1,mensaje="clean must be a logical(1)");
    #
    sssobject9(ended,"character",1,mensaje="ended must be a character of length one");
    #
    sssobject9(comment,"character",-1,mensaje="comment must be a character");
    #
    if (length(skip) == 0) {
      skip <- matrix("",0,2);
    } else {
      sssobject9(skip,"matrix",speci=matrix(c(1,2,Inf,2),2),
              mensaje="skip must be a character MATRIX with two columns");
      sssobject9(skip,"character",
              mensaje="skip must be a CHARACTER matrix with two columns");
    }
    #
    sssobject9(include,"character",1,mensaje="include must be a character of length one");
    #
}
#
# reading the proposed file
if (path!="") {file <- paste(path,file,sep="/");}
lu <- readLines(file);
#
# cleaning
if (clean) { for (ii in sssbf(lu)) {
    # removing framing spaces
    lu[ii] <- sssform3crop(lu[ii]," "," ");
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
if (!sssvoid9(comment)) {
  lu <- sssfilter8text(lu,comment,remove=TRUE,
                         exact=8,lower=FALSE,
                         monitor=monitor);
}
#
# removing the skipped lines
for (nn in sssbc(nrow(skip))) {
    deb <- skip[nn,1]; fin <- skip[nn,2];
    debu <- substr(lu,1,nchar(deb)) == deb;
    fini <- substr(lu,1,nchar(fin)) == fin;
    sk1 <- apply(outer(which(debu)  ,sssbf(lu),"<="),2,sum);
    sk2 <- apply(outer(which(fini)+1,sssbf(lu),"<="),2,sum);
    nsk <- ((sk1-sk2) < 1);
    lu <- lu[nsk];
}
#
# including the indicated files
inclus <- which(substr(lu,1,nchar(include))==include);
for (ii in sssbf(inclus)) {
    jj <- length(inclus) + 1 - ii;
    kk <- inclus[jj];
    fi <- strsplit(lu[kk]," ",fixed=TRUE)[[1]][2];
    plus <- Recall(fi,path=path,
                      clean=clean,
                      ended=ended,
                      comment=comment,
                      skip=skip,
                      include=include);
    plus <- c(lu[sssbc(kk-1)],plus);
    if (kk < length(lu)) { plus <- c(plus,lu[(kk+1):length(lu)]);}
    lu <- plus;
}
#
# returning
lu;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
ssstext2list <- function(text,
                      tags=sssrbsa0$tag1$v,
                      sep=sssrbsa0$sep0$v,
                      rsep="no_action",
                      stag=c("/","/"),
                      comment="#",
                      monitor=sssrbsa0$monitor$v
                     )
#TITLE  transforms a character into a list (of lists) of characters, and conversely
#ALIAS
#DESCRIPTION
# from a conveniently character vector comprising tags, returns nested lists.
# Most often such a character is obtained by reading a file with
# the function \samp{sssfile2text}, this explains why it is convenient
# to speak about 'lines' rather about 'components of the character
# vector'.\cr
# All lists are named lists and the tags give the names of their
# component. The maximum number of nested levels of lists is given by the
# number of rows of the matrix \samp{tags}. Its corresponding two columns
# providing the opening and closing sequences of the tags. Final lists 
# contain \samp{character} vectors, each component of them being
# on the same line and/or on the following line (before a new tag).\cr
# All tags must start at the very beginning of a line. All separator
# tags must be used sticked to the list tag.\cr
# Lines starting with a \samp{comment} are first eliminated.
#DETAILS
# It is compulsory to tag each level of the lists, this implies that 
# when starting a new list, a character vma (see \samp{ssstext2vma}
# for the details: vma means vector or matrix or array) is provided meaning 
# that this is the final level for this branch, or a new sublist 
# is started with the corresponding tag, or a new component of the
# list is given at a level less or equal to the present.\cr
# Separator between different character components is given by the
# \samp{sep} argument or indicated by the separator tag (\samp{stag}), 
# in the last case, it can be different from a leaf list to another.\cr
# Be aware that before reading a line for a character translation,
# all starting and ending spaces are eliminated.\cr
#KEYWORDS IO
#INPUTS
#{text} << character vector to be transformed into a list.>>
#[INPUTS]
#{tags} << Character matrix with two columns indicating the opening and closing
#          tags of the lists at different levels. Its row numbers gives the maximum
#          number of levels. Opening tags must be different.>>
#{sep} << Character sequence used to split the character vectors of every
# line. Notice that \samp{LF} is always considered as a separator.>>
#{rsep} << Indicates if repetitions of \samp{sep} must be considered as 
#          significant or not and which null value to introduce.
#          If \samp{no_action} then the repetitions will be ignored
#          if not \samp{rsep} component(s) will be introduced.>>
#{stag} << Two character strings indicating the tag to define different \samp{sep} for a given
#           [sub]list. These two correspond to \samp{stag[c(1,3)]} of \samp{ssslist2file} function.>>
#{comment} << Set a characters indicating that the line is a comment line when found
#             in the first positions.>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{sssrbsa0$monitor$v} provided object as an example.>>
#VALUE
# a list [of lists [of lists [...] ] ] of character (possibly named) vectors
# or matrices or arrays.
#EXAMPLE
# text <- c("<<A>>",
#            "[[a]]/*/v 1*un deux trois",
#            "[[b]]/*/v 1*2*3",
#            "un uno one",
#            "deux dos two",
#            "trois tres three",
#            "<<B>>",
#            "[[a]] un deux trois",
#            "[[b]] un  uno  one",
#            " deux dos two",
#            "trois tres three",
#            "<<C>> 1 2 3");
# ssstext2list(text);
# ssslist2text(ssstext2list(text));
#REFERENCE
#SEE ALSO ssstext2file sssfile2list
#CALLING
#COMMENT
#FUTURE Extend the possibility of the list by allowing
#       contents at the intermediate levels (not only
#       at the final levels as it is presently).
#AUTHOR J.-B. Denis
#CREATED 08_12_10
#REVISED 14_08_02
#--------------------------------------------
{
# checking
if (monitor$chk$v) {
    sssobject9(text,"character",-1,mensaje="text must be a character");
    #
    sssobject9(tags,"character",c(2,Inf),mensaje="tags must be a CHARACTER matrix with two columns");
    if (!is.matrix(tags)) {ssserreur(tags,"tags must be a character MATRIX of two columns");}
    if (ncol(tags)!=2) {ssserreur(tags,"tags must be a character matrix OF TWO COLUMNS");}
    if (length(unique(tags[,1]))!= nrow(tags)) { ssserreur(tags,"Opening tags are not unique!");}
    #
    sssobject9(sep,"character",1,mensaje="sep must indicate one string character");
    if (nchar(sep) == 0) { ssserreur(sep,"sep must not be an empty string");}
    #
    sssobject9(rsep,"character",1,mensaje="rsep must be a character(1)");
    #
    sssobject9(stag,"character",2,mensaje="stag must be a character of length two");
    if (any(nchar(stag) == 0)) { ssserreur(stag,"stag must not have zero length components");}
    if (length(grep(" ",stag)) > 0) { ssserreur(stag,"stag must not comprise spaces");}
    #
    sssobject9(comment,"character",1,mensaje="'comment' must be a character of length one");
}
#
# removing the comment lines
if (!sssvoid9(comment)) {
  text <- sssfilter8text(text,comment,remove=TRUE,
                         exact=8,lower=FALSE,
                         monitor=monitor);
}
#
# getting the tagged lines 
tagged <- matrix(NA,0,6);
dimnames(tagged) <- list(NULL,c("num","lev","name","leaf","sep","type"));
#
# tagged will contain all useful informations about the tags.
#    num: line number into text
#    lev: level number of the tag
#    name: name of the tag
#    leaf: "yes" when final branch, "no" if not. The last
#          two columns will be significant only for leaf sub-list.
#    sep: separator string to be used to get the vma character
#    type: indicates the type of the associated sub-sub-...-sub list
#          we are dealing with : "lists" indicates that more
#          levels are present, if not it is a leaf of the structure
#          and \samp{sssrbsa0$vma$v["c","v","V","w","m",...]} one of the type of vma character
#          is expected. 
nbtag <- 0;
for (i in sssbf(text)) {
    lev <- 0;
    # looking for a tag
    for (j in sssbc(nrow(tags))) { if (lev == 0) {
        pot <- sssform3crop(text[i],tags[j,1],"");
        if (nchar(pot) < nchar(text[i])) { lev <- j;}
    }}
    if (lev > 0) {
        # some tag was perhaps identified looking for the name
        put <- strsplit(pot,tags[lev,2],fixed=TRUE)[[1]];
        if (nchar(put[1]) < nchar(pot)) {
            # indeed it was a tag
            fch <- substr(put[2],1,1);
            if (fch %in% c(""," ",NA)) {
                # default tags and separator
                ssep <- sep;
                typing <- sssrbsa0$vma$v["v"];
                text[i] <- put[2];
            } else {
                # getting the possible explicit separator
                if (substr(put[2],1,nchar(stag[1]))==stag[1]) {
                    # looking for the separator
                    pit <- sssform3crop(put[2],stag[1]);
                    pat <- strsplit(pit,stag[2],fixed=TRUE)[[1]];
                    ssep <- pat[1];
                    pyt <- pat[2];
                } else {
                    # default separator
                    ssep <- sep;
                    pyt <- put[2];
                }
                # getting the possible information
                ouou <- strsplit(pyt," ",fixed=TRUE)[[1]];
                if (length(ouou) > 1) {
                    text[i] <- paste(ouou[-1],collapse=" ");
                } else {
                    text[i] <- "";
                }
                pyt <- ouou[1];
                # getting the type of the tag
                tta <- which(pyt==sssrbsa0$vma$v);
                if (length(tta)==0) {
                    # default type
                    typing <- sssrbsa0$vma$v["v"];
                } else {
                    # explicite type
                    typing <- names(sssrbsa0$vma$v)[tta];
                }
            }
        tagged <- rbind(tagged,c(i,lev,put[1],"???",ssep,typing));
        } # end of nchar(put[1]) < nchar(pot)
    } # end of lev > 0
} # end of i in sssbf(text)
#
#
# in case of no list
if (nrow(tagged) == 0) { return(vector("list",0));}
# detecting the final lists
tagged[,"leaf"] <- "yes";
for (ii in sssbc(nrow(tagged)-1)) {
    lev1 <- as.numeric(tagged[  ii,"lev"]);
    lev2 <- as.numeric(tagged[ii+1,"lev"]);
    if (lev2-lev1==1) { tagged[ii,"leaf"] <- "no";}
}
#
#   matrix 'tagged' is sssnow available
#
# checking the sequential progress of the levels
for (ii in sssbc(nrow(tagged)-1)) {
    lev1 <- as.numeric(tagged[  ii,"lev"]);
    lev2 <- as.numeric(tagged[ii+1,"lev"]);
    if ((lev2-lev1) > 1) {
        ssserreur(tagged,"The progression of levels is not accepted: all levels must be introduced");
    }
}
# checking that no leaf tags are followed by another tag
for (ii in sssbc(nrow(tagged)-1)) { if (tagged[ii,"leaf"]=="no") {
    actuel <- as.numeric(tagged[  ii,"num"]);
    suivan <- as.numeric(tagged[ii+1,"num"]);
    if ((suivan-actuel) != 1) {
        ssserreur(list(tagged,ii),"In line 'ii', a 'no-leaf' tags is not immediately followed by another tag (only terminal levels can have contents)");
    }
}}
#
if (tagged[1,"lev"] != "1") {
    ssserreur(tagged,"The first level must be 1");
}
# replacing the introduced NA
for (ii in sssbf(text)) { if (is.na(text[ii])) { text[ii] <- "";}}
#
#
# constructing the character strings associated to each leaf list
#
#
oul1 <- which(tagged[,"leaf"]=="yes");
oul2 <- as.numeric(tagged[oul1,"num"]);
lulu <- character(length(oul1));
oul2 <- c(oul2,length(text)+1);
for (ss in sssbf(lulu)) {
    leaf <- oul1[ss];
    ssep <- tagged[leaf,"sep"];
    for (sss in oul2[ss]:(oul2[ss+1]-1)) {
        nou <- sssform3crop(text[sss]," ");
        if (nchar(nou)>0) {
            if (nchar(lulu[ss])>0) {  
                # here multiple lines are concatenated
                lulu[ss] <- paste(lulu[ss],nou,sep=ssep);
            } else {
                lulu[ss] <- nou;
            }
        }
    }
}
#
# building the list of lists...
res <- vector("list",0);
vari <- "res"; niv <- 0; ulul <- 0;
for (ll in sssbc(nrow(tagged))) {
    # getting the name
    leve <- as.numeric(tagged[ll,"lev"]);
    if (niv<leve) {
        if (niv+1!=leve) { stop("Error(1) found into 'file2test'");}
        vari <- paste0(vari,"[['",tagged[ll,"name"],"']]");
    }
    if (niv>=leve) {
        va <- strsplit(vari,"[['",fixed=TRUE)[[1]];
        vari <- paste(va[1:leve],collapse="[['");
        nom <- tagged[ll,"name"];
        #???if (nom == "<NA>") { nom <- NULL;}
        vari <- paste0(vari,"[['",nom,"']]");
    }
    niv <- leve;
    # filling the value
    if (tagged[ll,"leaf"]=="yes") {
        ulul <- ulul+1;
        jsep <- tagged[ll,"sep"];
        # getting the character vector
        choc <- strsplit(lulu[ulul],jsep,fixed=TRUE)[[1]];
	if (rsep=="no_action") {
	    choc <- choc[choc!=""];
	}
        # building the code to interpret the character vector
        chacha <- ssstext2vma(choc,tagged[ll,"type"],sssrbsa0$sep1$v,monitor=monitor);
        eval(parse(text=paste(vari,"<- chacha")));
    } else {
        eval(parse(text=paste(vari,"<- vector('list',0)")));
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssfilter8text <- function(text,pattern,remove=TRUE,
                         exact=FALSE,how="a",lower=FALSE,
                         monitor=sssrbsa0$monitor$v)
#TITLE filters components of a character.
#DESCRIPTION
# For each component of a character vector, checks with function 
# \samp{sssbelong9} if it satisfies the conditions given by the other
# arguments, the component is removed (or selected).
#DETAILS
#KEYWORDS utilities
#INPUTS
#{text} << The \samp{character} to be filtered.>>
#{pattern} <<(\samp{character}) the character string(s) to be found
#             in each component.>>
#[INPUTS]
#{remove} << Must the conforming components be removed (or selected)?>>
#{exact} << When exact, one component must
# be strictly identical, if not a subtring is sufficient.>>
#{how} << Indicates what to do when \samp{length(sch)>1}. The choice are 
# \samp{v}: a logical vector gives back each check independently;
# \samp{1}: returns \samp{TRUE} when at least one of the component belongs
# to the series \samp{ch} and \samp{a} when all components must comply to get TRUE.>>
#{lower} << Must the comparisons being done after case lowering?>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{sssrbsa0$monitor$v} provided object as an example.>>
#VALUE A \samp{character} comprising the non-removed (or selected) components
#      of the initial components.
#EXAMPLE
# sssfilter8text(sssrbsa0$text2$v,"little",exact=FALSE,lower=TRUE,remove=FALSE)
# sssfilter8text(sssrbsa0$text2$v,"On",exact=8,lower=TRUE,remove=FALSE)
# sssfilter8text(sssrbsa0$text2$v,"On",        lower=TRUE,remove=FALSE)
# sssfilter8text(sssrbsa0$text2$v,"On",exact=8,lower=TRUE,remove=TRUE)
#REFERENCE
#SEE ALSO sssbelong9
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 04_08_03
#REVISED 14_08_03
#--------------------------------------------
{
  # first checking
  if (monitor$chk$v) {
    sssobject9(remove,"logical",1);
    sssobject9(text,"character");
  }
  # degenerate cases
  if (length(pattern)==0) {
    if (remove) {
      return(text);
    } else {
      return(character(0));
    }
  }
  if (length(text)==0) { return(text);}
  # second checking
  if (monitor$chk$v) {
    # other checking are made within 'sssbelong9'
  }
  # getting the conforming components
  coco <- vector("logical",length(text));
  for (ii in sssbf(text)) {
    coco[ii] <- sssbelong9(pattern,text[ii],
                        exact=exact,how=how,lower=lower,
                        monitor=monitor);
  }
  if (remove) {
    coco <- (!coco);
  }
  # returning
  text[coco];
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
ssstext2vma <- function(cha,what=sssrbsa0$vma$v["v"],
                     xsep=sssrbsa0$sep1$v,nat="C",
                       monitor=sssrbsa0$monitor$v)
#TITLE  transforms a character into a vector (or matrix, or array), and conversely
#DESCRIPTION
# from a \samp{character} vector, returns a vector, or a matrix, or
# an array of _characters_ with possibly names, or dimames. The information
# can be supplied in different ways for each of the three possibilities.
# It is advised to try the proposed examples.
#DETAILS
# The processing is done in character mode but the result can be
# transformed into numerical or logical values with the help of argument \samp{nat}.
# \cr
# In fact \samp{sssrbsa0$vma$v} coding is used for the argument \samp{what}. 
# This allows to easily modify the coding.
#KEYWORDS IO
#INPUTS
#{cha} << The character to transform.>>
#[INPUTS]
#{what} << Indicates which structure to return: either
# a vector, a matrix or an array.
#
# \cr ~~ \cr
#
#   For vectors, the possibilities are c/C/u/v/U/V (in fact the content of
#     \samp{sssrbsa0$vma$v["c"]}, \samp{sssrbsa0$vma$v["C"]},... but for the sake of the
#     simplicity, the names will be used because they are by default identical to 
#     the value; the same will be done for the other types):
#    \cr\samp{c} for a no named character(1); collapsing
#             is done with \samp{sssrbsa0$sep0$v}.
#    \cr\samp{C} for a no named character() of any length
#             (components are separated with \samp{xsep} which are
#              removed from the result); collapsing
#             is done with \samp{sssrbsa0$sep0$v}.
#    \cr\samp{v} or \samp{u} for a no named vector;
#    \cr\samp{V} for a named vector with
#          all names before the values; then an even number
#          of components must be provided.
#    \cr\samp{U} for a named vector with
#          names interlaced with the value (name_i, value_i); then an even number
#          of components must be provided.
#
# \cr ~~ \cr
#
#   For matrices, the possibilities are m/n/o/p/M/N/O/P:
#    \cr\samp{m} for a no named matrix given by rows, two adjacent rows
#          being separated with \samp{xsep} sequence, introduced as one of the
#          component of \samp{cha}, then for a 2x3 matrix, the length of \samp{cha}
#          will be 6+2 = 8.
#    \cr\samp{n} for a matrix with only the columns names. The expected sequence is
#          the names of columns, then the values as for \samp{m}; then for a 2x3
#          matrix, the length of \samp{cha} will be 3+1+8=12.
#    \cr\samp{o} for a matrix with only rows named. The expected sequence is
#          name of row, values of rows... Then 2x3 will imply a length of 8+2=10.
#    \cr\samp{p} when names for columns and rows, in a mixed way... Then 2x3 will imply
#          a length of 14.
#    \cr
#     When \samp{M}, \samp{N},
#          \samp{O} or \samp{P},
#          the same but the matrix will be transposed after
#          being read; said in another way, the values are given by columns.
#
# \cr ~~ \cr
#
#   For arrays, the possibilities are a/A/b/B:
#    \cr\samp{a} for a no named array, the dimensions, \samp{xsep}, the values in
#    the classical order (varying the first index the fastest). 2x3 will give
#    a length of 2+1+6=9.
#    \cr\samp{A} for a dimnamed array, the dimensions, \samp{xsep}, the dimnames of each
#    dimension in the right order also separated and finished with \samp{xsep}. 
#    2x3 will gives a length of 2+1+2+1+3+1+6=16.
#    \cr\samp{b} for a named dimensions array, the dimensions, \samp{xsep}, the names for the
#    dimension in the right order not separated and finished with \samp{xsep}. 
#    2x3 will gives a length of 2+1+2+1+6=12.
#    \cr\samp{B} for a named and dimnamed array, the dimensions, \samp{xsep}, the names for the
#    dimension in the right order not separated and finished with \samp{xsep}, then the dimnames separated
#    before the values. 
#    2x3 will gives a length of\cr (2+1)+(2+1)+(2+1+3+1)+(6)=19. >>
#{xsep} << Character sequence used to separate the character vector into blocks
#    giving information about the structure (see the examples).>>
#{nat} << Nature of the returned structure. Can be \samp{C} for character, \samp{N}
#         for numeric or \samp{L} for logical.>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{sssrbsa0$monitor$v} provided object as an example.>>
#VALUE a vector or a matrix or an array according to the arguments.
#EXAMPLE
#####
## vectors
# ssstext2vma(letters,"c");
# ssstext2vma(letters,"C",xsep="e");
# ssstext2vma(letters);
# ssstext2vma(letters,"V");
# ssstext2vma(letters,"u");
# ssstext2vma(c(LETTERS,letters),sssrbsa0$vma$v["V"]);
# ssstext2vma(c("A","a","B","b","C","c"),sssrbsa0$vma$v["U"]);
#####
## matrices
# ssstext2vma(c(1:3,"//",4:6),sssrbsa0$vma$v["m"]);
# ssstext2vma(c(1:3,"//",4:6),sssrbsa0$vma$v["M"]);
# ssstext2vma(c(LETTERS[1:3],"//",1:3,"//",4:6),sssrbsa0$vma$v["n"]);
# ssstext2vma(c(LETTERS[1:3],"//",1:3,"//",4:6),"N");
# ssstext2vma(c("a",1:3,"//","b",4:6),"o");
# ssstext2vma(c(c(LETTERS[1:3],"//","a",1:3,"//","b",4:6)),sssrbsa0$vma$v["p"]);
#####
## arrays
# ssstext2vma(c(2:4,"//",1:24),"a");
# ssstext2vma(c(2:4,"//","one","two","//",LETTERS[1:3],"//",
#          letters[1:4],"//",1:24),"A");
# ssstext2vma(c(2:4,"//","one","two","//",LETTERS[1:3],"//",
#          letters[1:4],"//",1:24),"A",nat="N");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_03_28
#REVISED 10_08_13
#--------------------------------------------
{
# flexibility
if (sssvoid9(cha)) { cha <- character(0);}
# of use
ssep <- which(cha==xsep);
# checking
if (monitor$chk$v) {
    sssobject9(cha,"character",-1,mensaje="cha must be a character");
    #
    sssobject9(what,"character",1,
               mensaje="what (character(1)) must indicate the type of desired output");
    if (!(what %in% sssrbsa0$vma$v)) {
        ssserreur(what,"'what' not in the list of possible values...");
    }
    #
    sssobject9(xsep,"character",1,mensaje="must indicate the character string of separation");
    #
    if (what %in% sssrbsa0$vma$v[c("V","U")]) {
        if ((length(cha) %% 2) != 0) {
            ssserreur(list(cha,what),"Here the length of 'cha' must be even");
        }
    }
    #
    if (what %in% sssrbsa0$vma$v[c("n","N","m","M","o","O")]) {
        if (length(ssep) >= 1) {
            nbc <- ssep[1] - 1;
            nbr <- (length(cha)+1) / (nbc+1);
            if ((nbr!=round(nbr)) | (length(cha) != (nbc*nbr+nbr-1))) {
                ssserreur(list(what,length(cha),nbc,nbr),"Dimensions not consistent");
            }
        }
    }
    #
    if (what %in% sssrbsa0$vma$v[c("p","P")]) {
        if (length(ssep) >= 1) {
            nbc <- ssep[1] + 1;
            X <- 2 + length(cha);
            if ((X %% nbc) != 0) {
                ssserreur(list(what,length(cha),nbc),"Dimensions not consistent");
            }
        }
    }
    #
    if (what %in% sssrbsa0$vma$v[c("a","A","b","B")]) { 
	# an array must be returned
        if (length(ssep)==0) {
            ssserreur(list(cha,xsep),"For arrays, dims must be provided");
        }
        didi <- cha[1:(ssep[1]-1)];
        didi <- as.numeric(didi);
        if (sum(is.na(didi))>0) {
            ssserreur(list(cha,xsep),"Dimensions are not numeric !");
        }
        if (any(didi<0)) {
            ssserreur(didi,"Negative dimensions were provided");
        }
        if (any(didi!=round(didi))) {
            ssserreur(didi,"Non integer dimensions were provided");
        }
        X <- length(didi) + 1 + prod(didi);
        if (what %in% sssrbsa0$vma$v[c("A","B")]) { X <- X + sum(didi) + length(didi); }
        if (what %in% sssrbsa0$vma$v[c("b","B")]) { X <- X + length(didi) + 1; }
	if (length(cha) != X) {
            ssserreur(list(cha,what,xsep),"Inconsistency for an array");
	}
    }
    #
    sssobject9(nat,"character",1,mensaje="'nat' must be a character(1)");
}
#
res <- character(0);
#
if (what %in% sssrbsa0$vma$v["c"]) {
    # a single character must be returned
    # sssrbsa0$sep0$v is used as collapsor
    res <- paste(cha,collapse=sssrbsa0$sep0$v);
}
#
if (what %in% sssrbsa0$vma$v["C"]) {
    # a character must be returned
    # sssrbsa0$sep0$v is used as separator
    res <- paste(cha,collapse=sssrbsa0$sep0$v);
    res <- strsplit(res,xsep,fixed=TRUE)[[1]];
    if (length(res)>1) { for (ii in 2:length(res)) {
        res[ii  ] <- sssform3crop(res[ii  ],sssrbsa0$sep0$v,"",1);
        res[ii-1] <- sssform3crop(res[ii-1],"",sssrbsa0$sep0$v,1);
    }}
}
#
if (what %in% sssrbsa0$vma$v[c("v","V","u","U")]) {
    # a vector must be returned
    if (what %in% c(sssrbsa0$vma$v[c("v","u")])) {
        res <- cha;
    } else {
        nb <- floor(length(cha)/2);
        if (what == sssrbsa0$vma$v["V"]) { nam <- rep(c(TRUE,FALSE),each=nb);
        } else { nam <- rep(c(TRUE,FALSE),nb); }
    res <- cha[!nam];
    names(res) <- cha[nam];
    }
}
#
if (what %in% sssrbsa0$vma$v[c("m","M","n","N","o","O","p","P")]) {
    # a matrix must be returned
    nbc <- ssep[1] - 1;
    cha <- c(cha,xsep);
    if (what %in% c(sssrbsa0$vma$v[c("p","P")])) {
        cha <- c(" ",cha);
    }
    if (what %in% sssrbsa0$vma$v[c("p","P")]) { add <- 1;
    } else { add <- 0; }
    cha <- matrix(cha,ncol=nbc+1+add,byrow=TRUE);
    cha <- cha[,-ncol(cha),drop=FALSE];
    if (what %in% sssrbsa0$vma$v[c("m","M")]) {
        res <- cha;
    }
    if (what %in% sssrbsa0$vma$v[c("n","N")]) {
        res <- cha[-1,,drop=FALSE];
        dimnames(res) <- list(NULL,cha[1,,drop=FALSE]);
    }
    if (what %in% sssrbsa0$vma$v[c("o","O")]) {
        res <- cha[,-1,drop=FALSE];
        dimnames(res) <- list(cha[,1,drop=FALSE],NULL);
    }
    if (what %in% sssrbsa0$vma$v[c("p","P")]) {
        res <- cha[-1,-1,drop=FALSE];
        dimnames(res) <- list(cha[-1,1,drop=FALSE],cha[1,-1,drop=FALSE]);
    }
    if (what %in% sssrbsa0$vma$v[c("M","N","O","P")]) {
        res <- t(res);
    }
}
#
if (what %in% sssrbsa0$vma$v[c("a","A","b","B")]) {
    if (length(ssep) == 0) { ssserreur(cha,"For array, dimensions must be provided");}
    # an array must be returned
    didi <- cha[1:(ssep[1]-1)];
    didi <- as.numeric(didi);
    nbdi <- length(didi);
    #
    if (what == sssrbsa0$vma$v["a"]) { vvv <- cha[-(1:ssep[1])];}
    if (what == sssrbsa0$vma$v["A"]) { vvv <- cha[-(1:ssep[1+nbdi])];}
    if (what == sssrbsa0$vma$v["b"]) { vvv <- cha[-(1:ssep[2])];}
    if (what == sssrbsa0$vma$v["B"]) { vvv <- cha[-(1:ssep[2+nbdi])];}
    #
    res <- array(vvv,dim=didi);
    #
    if (what %in% sssrbsa0$vma$v[c("A","B")]) {
        ndi <- vector("list",0);
        for (jj in sssbf(didi)) {
            jjj <- jj + (what == sssrbsa0$vma$v["B"]);
            if (ssep[jjj+1]-ssep[jjj]>1) {
                ndi[[jj]] <- cha[(ssep[jjj]+1):(ssep[jjj+1]-1)];
            }
        }
        dimnames(res) <- ndi;
    }
    if (what %in% sssrbsa0$vma$v["b"]) {
        ndi <- vector("list",0);
        for (jj in sssbf(didi)) {
            ndi[[jj]] <- 1:didi[jj];
        }
        dimnames(res) <- ndi;
    }
    if (what %in% sssrbsa0$vma$v[c("b","B")]) { 
        names(dimnames(res)) <- cha[(ssep[1]+1):(ssep[2]-1)];
    }
}
# transtyping
if (nat %in% c("N","L")) {
    rrr <- as.numeric(res);
    if (nat == "L") { rrr <- as.logical(rrr);}
    attributes(rrr) <- attributes(res);
    res <- rrr;
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssposi9 <- function(which,within,strict=TRUE,repe=TRUE,
                       monitor=sssrbsa0$monitor$v)
#TITLE  returns the positions of identifiers
#DESCRIPTION
#  Returns the positions of identifiers (an identifier is
#  \samp{character(1)} or \samp{integer(1)}).
# From a reference set of
# identifiers (a vector in fact) returns the positions of some of them.
# If they don't belong to the list, an error is issued
# according to \samp{strict}. If there are repetitions
# an error is issued or not according to \samp{repe}.
#DETAILS
#KEYWORDS helpful
#INPUTS
#{which}    <<Identifiers, their position is looked for.
# Must be a character or a numeric.>>
#{within}    <<The ordered vector of all identifiers.
# Must be a character or a numeric of distinct values.>>
#[INPUTS]
#{strict} <<Must \samp{which} be a subset of \samp{within}.>>
#{repe} <<Are repetitions allowed?>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{sssrbsa0$monitor$v} provided object as an example.>>
#VALUE
# A numeric vector of the positions. It is named with the identifiers.
# When the identifier does not belong to the list and \samp{strict} is
# FALSE, NA values are returned.
#EXAMPLE
# sssposi9(10:1,1:10);
# sssposi9(c("Z","E","P","L","I","N"),LETTERS);
# sssposi9(c("B","o","F","!"),LETTERS,strict=FALSE);
# sssposi9(c("B","E","B","E"),LETTERS);
# \dontrun{sssposi9(c("B","E","B","E"),LETTERS,repe=FALSE);}
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_03_09
#REVISED 10_10_12
#--------------------------------------------
{
# checking
if (monitor$chk$v) {
    sssobject9(which,   c("character","numeric"),-1,mensaje="   'which' is not valid");
    sssobject9(within,  c("character","numeric"),-1,mensaje="  'within' is not valid");
    sssobject9(strict,c("logical"),            1,mensaje="'strict' is not valid");
}
which  <- as.character(which);
within <- as.character(within);
if (length(unique(within)) < length(within)) {
  ssserreur(within,"The reference set contains duplicated items");
}
if (strict) {
    uu <- union(which,within);
    if (length(uu)!=length(within)) {
        ssserreur(list(setdiff(uu,within),within),"Some identifiers does not belong to the list");
    }
}
# computing
res <- as.numeric(outer(which,within,"==")%*%sssbf(within));
names(res) <- which;
res[res==0] <- NA;
# repetitions
if (!repe) {
    rr <- res[!is.na(res)];
    if (length(unique(rr)) < length(rr)) {
        ssserreur(rr,"Some repetitions were found while forbidden!");
    }
}  
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssread8list <- function(file,path=getwd(),bullet=")BULLETS(",
                      clean=TRUE,ended=")STOP(",comment="#",
                      skip=matrix(c(")SKIPPING(",")READING("),1),
                      include=c(")FILE("),
                      tags=sssrbsa0$tag1$v,
                      sep=sssrbsa0$sep0$v,
                      rsep="no_action",
                      stag=c("/","/"),
                       monitor=sssrbsa0$monitor$v
                     )
#TITLE  reads a file and transforms it in a list (of lists) of characters
#DESCRIPTION
# Reads a file and transforms it in a list (of lists) of characters
# It is a slight generalization of \samp{sssfile2list}.\cr
# It is worth explaining why such a function was written.
# The obvious one is that not always names for the components of the
# list are natural. (In future version, it could be good to mixed with
# and without names). Another reason is that the contraint of 'value
# only at the final level is not always natural. \samp{sssread8list} adds
# itself 'null' components (named \samp{sssrbsa0$l00$v})
# to allow it. Finally, levels can be skipped
# and missing levels are introduced.\cr
# So the generalization is that
# the components of the list have not to be given
# in the file but are determined by the function from an additionnal
# line (everywhere in the file) tagged with the third argument
# starting at the first character.
# This line contains the list of bullets, giving then their number.
# Notice that all proposed bullets must be different from
#  \samp{sssrbsa0$l00$v}.
# But everything has to be paid: sssnow the constraint is
# that every component must have a non empty content.
#DETAILS
# An intermediary file is generated (and not deleted) to be read by
# \samp{sssfile2list} function; its name is obtained from the global constant 
# \samp{sssrbsa0$fin$v}.\cr
# The numbering of the levels of the list is determined by cycling the
# global constant \samp{sssrbsa0$tyl$v} (run the example to have an idea of what this means).\cr
# Only the first three arguments plus the tags are used by this function, all others are
# directly passed to \samp{sssfile2list}.\cr
# For the moment, the numbering of the different levels does not take 
# into account the level hierarchy, this must be offered in
# future versions of this function.\cr
#KEYWORDS IO
#INPUTS
#{file} << file to be read and transformed into a list.>>
#[INPUTS]
#{path} << Directory containing the file.>>
#{bullet} <<Indicates the bullets to be considered. This tags must
#           be at the very beginning of a line. But the bullets
#           can be preceeded by spaces.>>
#{clean} <<Indicates if starting and ending spaces of every lines
#          must be eliminated at first.>>
#{ended} << To indicate the line from which to stop the reading.>>
#{comment} <<At the beginning of a line, it indicates that this line must not be
#          considered. More than one commenting character can be considered when
#          it is a vector. For instance \samp{c("#","\%")} means that
#          lines starting with an hash or a percent are comment lines.>>
#{skip} << To indicate set(s) of lines to be skipped. Must be a character matrix
#          where the two columns correspond respectively to the opening and 
#          closing tags, and where each row is associate to a couple of tags.
#          Tags are considered successively following the order of these matrix rows.>>
#{include} << Tags to indicate a file (including possible path) by
#             a \samp{character(1)} to include at this point its contents
#             as a text file with the same tags specifications.
#             Including files can be recursive. >>
#{tags} << Character matrix with two columns indicating the opening and closing
#          tags of the lists at different levels. Its row number gives the maximum
#          number of levels. Opening tags must be different.>>
#{sep} << Character sequence used to split the components of character vectors
#         provided within the same 
# line. Notice that \samp{LF} is always considered as a separator.>>
#{rsep} << Indicates if repetitions of \samp{sep} must be considered as 
#          significant or not and which null value to introduce.
#          If \samp{no_action} then the repetitions will be ignored
#          if not \samp{rsep} component(s) will be introduced.>>
#{stag} << Two character strings indicating the tag to define different \samp{sep} for a given
#           [sub]list. These two correspond to \samp{stag[c(1,3)]} of \samp{ssslist2file} function.>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{sssrbsa0$monitor$v} provided object as an example.>>
#VALUE
# a list [of lists [of lists [...] ] ] of character (possibly named) vectors
# or matrices or arrays.
#EXAMPLE
# sink("rbsa.list.txt")
# cat(sssrbsa0$text5$v,sep="\n");
# sink();
# sssread8list("rbsa.list.txt");
# # deleting the created file
# unlink("rbsa.list.txt");
# # deleting the intermediate file
# unlink(sssrbsa0$fin$v);
#REFERENCE
#SEE ALSO ssslist2file
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 11_01_17
#REVISED 11_01_17
#--------------------------------------------
{
# everything, including the check are deported
# to the called functions
#
#
# checking
if (monitor$chk$v) {
  sssobject9(file,"character",1,mensaje="file must indicate the name of one file");
  #
  sssobject9(path,"character",1,mensaje="path must indicate the name of one directory");
  #
  if (path!="") { fifi <- paste(path,file,sep="/");} else { fifi <- file;}
  if (is.na(file.info(fifi)$size)) { ssserreur(fifi,"This file seems not to exist.");}
  }
#
# reading the file to get the possible bullet
if (path!="") { fifi <- paste(path,file,sep="/");} else { fifi <- file;}
uu <- readLines(fifi);
bu <- grep(bullet,uu,fixed=TRUE);
#
# possibly transforming the character
if (length(bu)>=1) {
  # analyzing the bullets
  bubu <- uu[bu[length(bu)]];
  nabu <- sssform3split(bubu,OPA="",CPA="",opa="",cpa="",sep=" ")[-1];
  if (sssrbsa0$l00$v %in% nabu) {
    ssserreur(list(sssrbsa0$l00$v,nabu),
           "The proposed bullets comprise the standard bullet defined by 'sssrbsa0$l00$v', this is not possible");
  }
  lebu <- nchar(nabu);
  nbul <- length(nabu);
  uu <- uu[-bu];
  # getting the replacements
  if (nbul>nrow(tags)) {
    ssserreur(list(tags,nabu),"More levels of the list are given that the number of provided tags!");
  }
  debu <- rep(tags[,1],nbul)[1:nbul];
  finn <- rep(tags[,2],nbul)[1:nbul];
  nunu <- rep(sssrbsa0$tyl$v,nbul)[1:nbul];
  orajou <- numeric(0);
  lrajou <- numeric(0);
  qrajou <- character(0);
  for (bb in sssbc(nbul)) {
    # dealing the bullets of level bb
    lo <- lebu[bb];
    ou <- grep(nabu[bb],uu,fixed=TRUE);
    # first looking where there are and how they are
    ouou <- numeric(0);
    for (ii in ou) {
      # removing the possible first spaces
      lili <- sssform3crop(uu[ii]);
      if (substr(lili,1,lo)==nabu[bb]) {
        ouou <- c(ouou,ii);
      }
    }
    # preparing everything
    if (length(ouou)>0) {
      nou <- sssform3numbering(length(ouou),nunu[bb]);
      iii <- 0;
      for (ii in ouou) {
        iii <- 1+iii;
        # getting rid of first spaces
        lili <- sssform3crop(uu[ii]);
        if (substr(lili,1,lo)!=nabu[bb]) {
          stop("in 'sssread8list' about 'ouou'");
        }
        lulu <- lili;
        lili <- substr(lili,lo+1,nchar(lili));
        # the string cannot be empty
        if (sssform3crop(lili)=="") {
          ssserreur(list(fifi,lulu),
                 "This tagged line is empty which is not allowed!");
        }
        # when we are not at the last level,
        # additional levels must be introduced
        if (bb<nbul) {
          orajou <- c(orajou,ii);
          lrajou <- c(lrajou,bb);
          qrajou <- c(qrajou,nou[iii]);
          uu[ii] <- paste0(debu[nbul],sssrbsa0$l00$v,finn[nbul],lili);
        } else {
          uu[ii] <- paste0(debu[bb],nou[iii],finn[bb],lili);
        }
      }
    }
  }
  #
  # adding the level to insert
  if (length(orajou)>0) {
    # FIRST ordering things into order
    oo <- order(orajou);
    orajou <- orajou[oo];
    qrajou <- qrajou[oo];
    lrajou <- lrajou[oo];
    for (ii in sssbf(orajou)) {
      iii <- length(orajou) - ii + 1;
      iiu <- orajou[iii];
      nbajou <- (nbul - lrajou[iii]);
      # shifting at the end to introduce new lines
      uu[sssbd(iiu+nbajou,length(uu)+nbajou)] <- uu[sssbd(iiu,length(uu))];
      # adding the chief line
      uu[iiu] <- paste0(debu[lrajou[iii]],
                       qrajou[iii],
                       finn[lrajou[iii]]);
      # adding possible intermediary lines
      jk <- 0;
      for (jjj in sssbd(lrajou[iii]+1,nbul-1)) {
        jk <- 1+jk;
        uu[iiu+jk] <- paste0(debu[jjj],
                            sssrbsa0$l00$v,
                            finn[jjj]);
      }
    }
  }
}
#
# writing the intemediary file
if (path!="") { fofo <- paste(path,sssrbsa0$fin$v,sep="/");} else { fofo <- sssrbsa0$fin$v;}
writeLines(uu,fofo);
#
# finally calling 'sssfile2list'
res <- sssfile2list(
                 file=sssrbsa0$fin$v,path=path,
                 clean=clean,ended=ended,comment=comment,
                 skip=skip,
                 include=include,
                 tags=tags,
                 sep=sep,
                 rsep=rsep,
                 stag=stag
                                     );
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssnow <- function(what="dh",format="red",seps=c("_","@",":"))
#TITLE  returns a character giving the present moment
#DESCRIPTION
# Returns a character giving the present moment
# with different components
#DETAILS
# based on Sys.time R function
#KEYWORDS utilities
#INPUTS
#[INPUTS]
#{what} <<\samp{character(1)} indicating which components to include
#  among:
#  \cr\samp{d} for the day (including year and month),
#  \cr\samp{h} for the hour,
#  \cr\samp{m} for the minute (including hour),
#  \cr\samp{s} for the second (including hour and minutes.>>
#{format} <<\samp{r} or \samp{"red"} for a reduced output.>>
#{seps} <<\samp{character(3)} providing the separations for the
#         different components of the reduced returned output.>>
#VALUE
# a character
#EXAMPLE
# cat("Now is",sssnow(),"\n");
# cat(sssnow("dhms","verbose"),"\n");  
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_04_19
#REVISED 14_06_04
#--------------------------------------------
{
# checking
sssobject9(seps,"character",c(1,Inf));
if (length(seps) < 3) { seps <- c(seps,seps,seps);}
# adding the consequences to what
if (sssbelong9("s",what)) { what <- paste0(what,"m");}
if (sssbelong9("m",what)) { what <- paste0(what,"h");}
# getting the moment
mnt <- Sys.time();
maintenant <- as.character(mnt);
# getting each piece of the moment
an <- strsplit(maintenant,"-")[[1]][1];
mois <- strsplit(maintenant,"-")[[1]][2];
jour <-  strsplit(maintenant,"-")[[1]][3];
heure <- strsplit(jour," ")[[1]][2];
jour <- strsplit(jour," ")[[1]][1];
minute <- strsplit(heure,":")[[1]][2];
seconde <- strsplit(heure,":")[[1]][3];
heure <- strsplit(heure,":")[[1]][1];
# building the result
res <- "";
if (format[1]=="r") {
  seps <- rep(seps[1],3);
  format <- "red";
}
if (format[1]=="red") {
    if (sssbelong9("d",what)) {
        res <- paste(res,paste(substr(an,3,100),mois,jour,sep=seps[1]),sep="");
        if (sssbelong9("h",what)) { res <- paste0(res,seps[2]);}
    }
    if (sssbelong9("h",what)) {
        res <- paste0(res,heure);
        if (sssbelong9("m",what)) { res <- paste0(res,seps[3]);}
    }
    if (sssbelong9("m",what)) {
        res <- paste0(res,minute);
        if (sssbelong9("s",what)) { res <- paste0(res,seps[3]);}
    }
    if (sssbelong9("s",what)) {
        res <- paste0(res,seconde);
    }
} else {
    if (sssbelong9("d",what)) {
        res <- paste(res,"le",weekdays(mnt),jour,months(mnt),an);
        if (sssbelong9("h",what)) { res <- paste0(res," `a ");}
    }
    if (sssbelong9("h",what)) {
        res <- paste0(res,heure,"H");
        if (sssbelong9("m",what)) { res <- paste0(res,"");}
    }
    if (sssbelong9("m",what)) {
        res <- paste0(res,minute,"m");
        if (sssbelong9("s",what)) { res <- paste0(res,"");}
    }
    if (sssbelong9("s",what)) {
        res <- paste0(res,seconde,'s');
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssform3split <- function(cara,none="-.-",
                       OPA="{",CPA="}",
                       opa="(",cpa=")",sep="+",
                       monitor=sssrbsa0$monitor$v)
#TITLE  inverse function of sssform3join
#DESCRIPTION returns a vector character of the names
#  from a character string generated by \samp{sssform3join}.
#DETAILS
# Of course, it is implicitely supposed that the
# inversion is unambiguous.\cr
# The syntax (consistent use of the parentheses and 
# separators) is checked: an error is issued when
# not correct. The possible \samp{\\n} added by \samp{sssform3join}
# is taken into account and removed.
# For the moment, sep cannot be an empty string.
#KEYWORDS print format
#INPUTS
#{cara}<<Character to be considered.>>
#[INPUTS]
#{none}<< idem as in sssform3join.>>
#{OPA}<< idem as in sssform3join.>>
#{CPA}<< idem as in sssform3join.>>
#{opa}<< idem as in sssform3join.>>
#{cpa}<< idem as in sssform3join.>>
#{sep}<< idem as in sssform3join.>>
#{monitor} << List of monitoring constants, see \samp{sssrbsa0$monitor$v} to
#             know its structure.>>
#VALUE
# A character vector
#EXAMPLE
# uu <- sssform3join(letters[1:4]);
# sssform3split(uu);
#REFERENCE
#SEE ALSO sssform3join
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_10_30
#REVISED 08_10_30
#--------------------------------------------
{
# checking
if (monitor$chk$v) {
    sssobject9(cara,"character",1);
}
# removing the possible "\n"
nl <- nchar(cara);
if (nl > 2) {
  fin <- substr(cara,nl,nl);
  if (fin=="\n") { cara <- substr(cara,1,nl-1);}
}
# removing the external braces
n1 <- nchar(OPA); n2 <- nchar(CPA);
if (n1 > 0) {
    ax <- substr(cara,1,n1);
    if (ax!=OPA) {
        ssserreur(c(OPA,ax),"Non consistent OPA and cara arguments.");
    }
    cara <- substr(cara,1+n1,nchar(cara));
}
if (n2 > 0) {
    nn <- nchar(cara);
    ax <- substr(cara,nn-n2+1,nn);
    if (ax!=CPA) {
        ssserreur(c(CPA,ax),"Non consistent OPA and cara arguments.");
    }
    cara <- substr(cara,1,nn-n2);
}
# removing the separator and constituting the vector
if (cara==none) { res <- character(0);
} else {
    # splitting
    if (nchar(sep)==0) {
        ssserreur(cara,"Sorry by the function does not act properly for empty separators");
    }
    res <- strsplit(cara,sep,fixed=TRUE)[[1]];
    # removing the internal braces
    n1 <- nchar(opa); n2 <- nchar(cpa);
    for (hd in sssbf(res)) {
        rr <- res[hd];
        if (n1 > 0) {
            ax <- substr(rr,1,n1);
            if (ax!=opa) {
                ssserreur(c(opa,ax),"Non consistent opa and cara arguments.");
            }
            rr <- substr(rr,1+n1,nchar(rr));
        }
        if (n2 > 0) {
            nn <- nchar(rr);
            ax <- substr(rr,nn-n2+1,nn);
            if (ax!=cpa) {
                ssserreur(c(cpa,ax),"Non consistent cpa and cara arguments.");
            }
            rr <- substr(rr,1,nn-n2);
        }
        res[hd] <- rr;
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssform3crop <- function(chaine,bef=sssrbsa0$sep0$v,aft=bef,
                      mxm=Inf,empty=FALSE,
                      monitor=sssrbsa0$monitor$v)
#TITLE  removes framing characters from a character string
#DESCRIPTION
# removes \samp{bef}s before and \samp{aft}s after a character string.
#DETAILS
#KEYWORDS utilities
#INPUTS
#{chaine} <<The character string to refine. 
#           Can be a vector.>>
#[INPUTS]
#{bef} << What to repeatedly remove at the beginning.>> 
#{aft} << What to repeatedly remove at the end.>>
#{mxm} << Maximum number of tags to remove.>>
#{empty} << Must remaining empty lines be removed?>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{sssrbsa0$monitor$v} provided object as an example.>>
#VALUE
# \samp{character} after removings
#EXAMPLE
# sssform3crop('IMPORTANT','IM',' ANT');
# sssform3crop(c('   OUF ',' FOU ',''),' ','',1);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_12_10
#REVISED 14_01_22
#--------------------------------------------
{
# checking
if (monitor$chk$v) {
    sssobject9(chaine,"character",-1,mensaje="sssform3crop: 'chaine' must be a character");
    sssobject9(bef,"character",1,mensaje="sssform3crop: Vector are not accepted for 'bef'");
    sssobject9(aft,"character",1,mensaje="sssform3crop: Vector are not accepted for 'aft'");
    sssobject9(mxm,"numeric",1,mensaje="sssform3crop: mxm must be numeric(1)");
}
# null case
if (length(chaine) == 0) { return(chaine);}
lb <- nchar(bef);
la <- nchar(aft);
for (ich in sssbf(chaine)) {
    cha <- chaine[ich];
    # removing at the beginning of the string
    if (lb>0) {
	nbr <- 0;
	repeat {
	    deb <- substr(cha,1,lb);
	    if ((deb == bef) & (nbr < mxm)) {
		cha <- substring(cha,lb+1);
		nbr <- nbr+1;
	    } else { break;}
	}
    }
    # removing at the end of the string
    if (la>0) {
	nbr <- 0;
	repeat {
	    lc <- nchar(cha);
	    fin <- substr(cha,lc-la+1,lc);
	    if ((fin == aft) & (nbr < mxm)) {
		cha <- substring(cha,1,lc-la);
		nbr <- nbr+1;
	    } else { break;}
	}
    }
    chaine[ich] <- cha;
}
# removing empty lines
if (empty) {
  nbli <- length(chaine);
  for (ii in rev(sssbc(nbli))) {
    if (chaine[ii] == "") { chaine <- chaine[-ii];}
  }
}
# returning
chaine;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssfile2list <- function(file,path="",
                      clean=TRUE,ended=")STOP(",comment="#",
                      skip=matrix(c(")SKIPPING(",")READING("),1),
                      include=c(")FILE("),
                      tags=sssrbsa0$tag1$v,
                      sep=sssrbsa0$sep0$v,
                      rsep="no_action",
                      stag=c("/","/")
                     )
#TITLE  reads a file and transforms it in a list (of lists) of characters
#DESCRIPTION
# reads a conveniently tagged text file into nested lists.
# It is just the linking of the two functions \samp{sssfile2text}
# and \samp{ssstext2list}, see their comments for the description of the arguments.
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
#           [sub]list. These two correspond to \samp{stag[c(1,3)]} of \samp{ssslist2file} function.>>
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
# sssfile2list("rbsa.list.txt");
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
res <- sssfile2text(file,
                 path=path,clean=clean,ended=ended,
                 comment=comment,skip=skip,
                 include=include
                );
#
# from character to list
res <- ssstext2list(res,                      #
                 tags=tags,sep=sep,
                 rsep=rsep,stag=stag
                );
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssform3title <- function(tit,empha=3,indent=2+2*empha,imp=TRUE)
#TITLE  prints or prepares a title
#DESCRIPTION
# prints or prepares the character string \samp{tit}
# with more or less emphasis.
# This function is a shortcut of the hidden function \samp{sssform3titre},
# with some specialized calls.
#DETAILS
#KEYWORDS print
#INPUTS
#{tit}<<the title to print (just one line)>>
#[INPUTS]
#{empha} << Level of emphasizing.\cr
#          (0): single line without carriage return\cr
#          (1): single line\cr
#          (2): underlined\cr
#          (3): underlined and overlined\cr
#          (4): (2) + 1 line before\cr
#          (5): (3) + 1 line after\cr
#          (6): (2) + 2 lines before and after\cr
#          (7): corners + 1 line before and after (plus surrounding)\cr
#          (8): box + 1 lines before and after (plus surrounding)\cr>>
#{indent} << Number of spaces to introduce before the title>>
#{imp} << Printing is performed and nothing is returned.
#                If FALSE, the character string is returned 
#                (including possible new lines)>>
#VALUE
# either nothing or a character string according to imp
#EXAMPLE
# for (ii in 0:8) {sssform3title("Some Title",ii,imp=TRUE)};
#REFERENCE
#SEE ALSO
#CALLING {sssform3repeat}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_24
#REVISED 10_09_15
#--------------------------------------------
{
# adjusting
empha <- round(max(0,min(8,empha)));
# preparing the argument for sssform3titre
if (length(tit)>1) { tit <- paste(tit,collapse=" ");}
if (empha == 0) { tit <- paste0("<",tit,">")}
if (empha == 1) { tit <- paste0("(*)",tit,"(*)")}
sbef <- round(max(indent,0));
caret <- (empha != 0);
saft=""; lbef <- 0; laft <- 0;
box <- "no";
if (empha == 2) { box <- "un";}
if (empha == 3) { box <- "unov";}
if (empha == 4) { box <- "unov"; lbef <- 1;}
if (empha == 5) { box <- "unov"; laft <- 1; lbef <- 1;}
if (empha == 6) { box <- "unov"; laft <- 2; lbef <- 2;}
if (empha == 7) {
  box <- "cor" ; laft <- 1; lbef <- 1;
  tit<- paste0(" ",tit," ");
}
if (empha == 8) {
  box <- "box" ; laft <- 1; lbef <- 1;
  tit<- paste0(" ",tit," ");
}
# calling sssform3titre
res <- sssform3titre(tit,box=box,
                  lbef=lbef,sbef=sbef,
                  saft=saft,laft=laft,
                  charbox=c("+","|","+",
                            "-","+","|",
                            "+","-"," "),
                  alig=2,caret=caret,imp=imp);
# returning
if (imp) { return(invisible());}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssform3titre <- function(tit,box="un",
                           lbef=0,sbef=5,
                           saft=0,laft=1,
                           charbox=c("+","|","+","-",
                                     "+","|","+","-",
                                     "."),
                           alig=2,
                           caret=TRUE,
                           imp=TRUE,
                       monitor=sssrbsa0$monitor$v)
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
#{alig} <<The aligment to be done by \samp{sssform3justify}: 
#         1 for left,2 for center and 3 for right.>>
#{caret} <<Indicates if \samp{\\n} must be added at the end
#          of each created line.>>
#{imp} << Printing is performed and nothing is returned.
#                If FALSE, the character string is returned 
#                (including possible new lines)>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{sssrbsa0$monitor$v} provided object as an example.>>
#VALUE
# According to \samp{imp}: nothing when printing is performed,
# a character string 
#EXAMPLE
# sssform3titre("Some Title");
# sssform3titre(c("The title","can comprise","several lines"),
#            box="box",lbef=4,laft=2);
# sssform3titre(c("And the box","can be incomplete", "as well!"),
#            box="cor");
# sssform3titre(c("The title","can comprise","several lines"),
#            box="box",lbef=4,laft=2,
#            charbox=c("*","+","/","=","*","+","/","="," "));
#REFERENCE
#SEE ALSO
#CALLING {sssform3repeat}
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
    sssobject9(caret,"logical",1);
    sssobject9(tit,"character",-1);
    sssobject9(box,"character",1);
    if (!sssbelong9(box,boxes,TRUE)) {
        ssserreur(list(box,boxes),"'box' must be one of 'boxes'");
    }
    sssobject9(lbef,c("numeric","character"),c(1,Inf));
    sssobject9(laft,c("numeric","character"),c(1,Inf));
    sssobject9(sbef,c("numeric","character"),1);
    sssobject9(lbef,c("numeric","character"),1);
    sssobject9(charbox,"character",9);
    if (!all(nchar(charbox)==1)) {
        ssserreur(charbox,"For the moment only unicharacter in 'charbox'");
    }
    sssobject9(imp,"logical",1);
}
# the null case
if (sssvoid9(tit)) { return(character(0));}
# preparing
if (is.numeric(lbef)) { lbef <- rep(" ",max(0,round(lbef[1])));}
if (is.numeric(laft)) { laft <- rep(" ",max(0,round(laft[1])));}
if (is.numeric(sbef)) { sbef <- sssform3repeat(" ",max(0,round(sbef[1])));}
if (is.numeric(saft)) { saft <- sssform3repeat(" ",max(0,round(saft[1])));}
#
if (box %in% boxes[c(1,2,4)])   { charbox[c(7,8,1)]   <- NA;}
if (box %in% boxes[c(1,3,4)])   { charbox[c(3,4,5)]   <- NA;}
if (box %in% boxes[c(1,2,3,6)]) { charbox[c(1:3,5:7)] <- "";}
if (box %in% boxes[c(5)])       { charbox[c(4,8,2,6)] <- " ";}
#
lmax <- max(sapply(tit,nchar));
tit <- sssform3justify(tit,lmax,format=alig,carac=charbox[9]);
# producing
res <- character(0);
# first lines
for (ii in sssbf(lbef)) { res <- c(res,lbef[ii]);}
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
for (ii in sssbf(tit)) {
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
for (ii in sssbf(laft)) { res <- c(res,laft[ii]);}
# adding carriage returns
if (caret) { for (ii in sssbf(res)) {
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
sssvoid9 <- function(x,vector=FALSE) 
#TITLE  tests the nullness of objects
#DESCRIPTION
# Returns TRUE is the structure is empty.\cr
# This function was proposed because the \samp{NULL} cannot replace
#  any kind of objects.\cr
#  Notice that \samp{sssvoid9("")} is \samp{TRUE}.
#DETAILS
# Non-existing objects are not detected as void!
# Functions are considered as not void.
# Mainly \samp{NULL}, \samp{NA} and length-zero object are void
# then a \samp{data.frame} with at least one variable is declared as not void.
# Character \samp{""} is considered as void.
#KEYWORDS programming
#INPUTS
#{x}    <<object to be scrutinazed>>
#[INPUTS]
#{vector} <<This argument is considered only when \samp{x} is
#           a \samp{character} or a (\samp{numeric} or \samp{logical}). When \samp{FALSE} it is considered void
#           when all components are \samp{""} or \samp{NA}; when \samp{TRUE}, a vector
#           with same length as \samp{x} is returned indicating which
#           components are \samp{""} or \samp{NA}.>>
#VALUE
# TRUE when the object is considered as void
# FALSE if not. Can also be a vector for characters when \samp{vector}.
# See the proposed examples.
#EXAMPLE
# \dontrun{sssvoid9(toto);}
# sssvoid9(numeric(0));
# sssvoid9(NULL);
# sssvoid9("");
# sssvoid9(c("","A"));
# sssvoid9(c("","A"),vector=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE Find a trick for non-existing objects: difficulty of the kind is that \samp{TRUE} 
#       is detected as void and more
#AUTHOR J.-B. Denis
#CREATED 07_10_15
#REVISED 14_08_28
#--------------------------------------------
{
  # non existing object
  #if (!exists(deparse(substitute(x)))) { return(TRUE);}
  # covering some special non null cases
  if (is.function(x))   { return(FALSE);}
  # standard cases
  if (is.null(x))                      { return(TRUE);}
  if (length(x)==0)                    { return(TRUE);}
  # looking when character
  if (is.character(x)) {
    if (vector) {
      return(x=="");
    } else {
      return(all(x==""));
    }
  }
  # looking when numeric
  if (is.numeric(x)|is.logical(x)) {
    if (vector) {
      return(is.na(x));
    } else {
      return(all(is.na(x)));
    }
  }
  # returning
  FALSE;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssfidi9 <- function(chemins,retour=c("-","f","d"))
#TITLE  checks files and directories
#DESCRIPTION
#  checks if \samp{chemin} is a file or a directory
#DETAILS
#  the distinction between files and directories is
# based on \samp{file.info} function.
#KEYWORDS file directory
#INPUTS
#{chemins} << \samp{character}; for each element of it, the
#       existence as file or directory must be checked.>>
#[INPUTS]
#{retour} << \samp{character(3)} indicating what to return
#            in case of (non existence), (existence of a file)
#            and (existence of a directory).>>
#VALUE
# a character of same length as \samp{chemins} with one of the
# components of \samp{retour}.
#EXAMPLE
# sssfidi9(c("/","~","~/.bashrc",getwd(),"toto.txt"));
#REFERENCE
#SEE ALSO sssdipa
#CALLING
#COMMENT
#FUTURE introduce the notion of accessibility
#AUTHOR J.-B. Denis
#CREATED 14_01_22
#REVISED 14_06_25
#--------------------------------------------
{
  # checking the arguments
  sssobject9(chemins,"character");
  sssobject9(retour,"character",3);
  # initializing
  res <- rep(retour[1],length(chemins));
  # proceeding
  for (ii in sssbf(chemins)) {
    if (nchar(chemins[ii])==0) {
      stop(paste0(" The ",ii,"th path is void!"));
    } else {
      fidi <- chemins[ii];
      if (file.exists(fidi)){
        fidir <- file.info(fidi)$isdir;
        if (!sssvoid9(fidir)) {
          res[ii] <- retour[2+fidir];
        }
      }
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssexplore8list <- function(lili,monitor=sssrbsa0$monitor$v)
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
# \samp{sssrbsa0$sep1$v} is used to join the names, as a consequence it must
# not be present into the names. In case an error is issued.
#KEYWORDS IO
#INPUTS
#{lili} << The list structure to be explored.>>
#[INPUTS]
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{sssrbsa0$monitor$v} provided object as an example.>>
#VALUE
# The resulting character matrix (see the description section).
#EXAMPLE
# sssexplore8list(sssrbsa0$lis1$v);
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
    sssobject9(lili,"list",-1,mensaje="lili must be a list");
}
#
# starting the table
nta <- c("numbers","number","names","name","depth","level","classes");
tab <- matrix(NA,0,length(nta));
dimnames(tab) <- list(NULL,nta);
if (length(lili) == 0) { return(tab);
} else {
    for (ii in sssbf(lili)) {
        nom <- names(lili)[ii];
        if (is.null(nom)) { nom <- "<NA>";}
        if (nom=="") { nom <- "<NA>";}
        if (sssbelong9(sssrbsa0$sep1$v,nom)) {
            ssserreur(list(sssrbsa0$sep1$v,nom),
                   "This name for the list comprises 'sssrbsa0$sep1$v' which is not accepted by 'sssexplore8list'");
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
	for (ii in sssbf(coco)) {
	    nom <- names(coco)[ii];
	    if (is.null(nom)) { nom <- "<NA>";}
	    if (nom=="") { nom <- "<NA>";}
            noms <- paste(tab[qq,"names"],nom,sep=sssrbsa0$sep1$v);
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
sssinterv7belonging <- function(x,int,monitor=sssrbsa0$monitor$v)
#TITLE  checks if a series of values belong to a series of intervals
#DESCRIPTION
# computes and returns the indicator vector of the positions of
# values with respect to intervals.
#DETAILS
# This function is compatible with real infinite values
#KEYWORDS misc
#INPUTS
#{x} <<Vector of value(s) to be scrutinized.>>
#{int} <<Series of interval(s) to be considered.
# Either a \samp{numeric(2)} or a matrix with two columns.
# Empty intervals (\samp{numeric(0)} are not admitted.>>
#[INPUTS]
#{monitor} <<List providing the monitoring constants, see \code{sssrbsa0$monitor$v}
#            to know the contents.>>
#VALUE
# A matrix with rows associated to the \code{x} values and
# columns associated to the \code{int} intervals giving
# \code{-2,-1,0,1,2} according to whether \code{x} is less than,
# equal to the lower bound, inside, equal to the upper bound or
# greater than the interval.
#EXAMPLE
# sssinterv7belonging(1:5,1:2);
# sssinterv7belonging(1:5,matrix(1:10,ncol=2));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_11_17
#REVISED 10_12_13
#--------------------------------------------
{
    # checking
    if (monitor$chk$v) {
        sssobject9(  x,"numeric",-1,mensaje="  'x' must be numeric");
        sssobject9(int,"numeric",-1,mensaje="'int' must be numeric");
        if (!is.matrix(int)) {
            if (length(int) != 2) {
                ssserreur(int,"When 'int' is not a matrix, it must be a numeric(2)");
            }
            if (is.nan(diff(int))) {ssserreur(int,"This is not an accepted interval");}
            if (diff(int)<0) { ssserreur(int,"'int' does not define an interval!");}
        } else {
            if (ncol(int)!=2) {
                ssserreur(int,"When 'int' is a matrix, it must comprise 2 columnes");
            }
            ru <- int[,2] - int[,1];
            if (any(is.nan(ru))) { ssserreur(int,"Some rows are not accepted as intervals");}
            if (any((ru<0))) {
                ssserreur(int,"Not all rows of 'int' define an interval");
            }
        }
    }
    # getting a uniform presentation
    if (!is.matrix(int)) { int <- matrix(int,ncol=2);}
    # preparing the result
    nbx <- length(x); nbint <- nrow(int);
    res <- matrix(NA,nbx,nbint);
    dimnames(res) <- list(names(x),dimnames(int)[[1]]);
    # degenerate case
    if (length(res)==0) { return(res);}
    # ancillary functions
    be0 <- function(x,int0) {
      if (is.finite(int0)) {
        ss <- sign(x-int0);
      } else {
        ss <- rep(-sign(int0),length(x));
        ss[x==int0] <- 0;
      }
      ss;
    }
    bel <- function(x,int) {
        be0(x,int[1]) + be0(x,int[2]);
    }
    # computation
    for (ii in sssbc(nrow(int))) {
        res[,ii] <- bel(x,int[ii,]);
    }
    # returning
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssbelong9 <- function(sch,ch,exact=FALSE,how="a",lower=FALSE,
                       monitor=sssrbsa0$monitor$v)
#TITLE  indicates inclusion of character string
#DESCRIPTION
# Checks if some string(s) belong(s) to a series of strings.
# When \samp{exact} is FALSE, returns TRUE if the 
# character string \samp{sch} is included at least once
# into the character string \samp{ch}.\cr
# \samp{sch} can be a vector, in that case, the check is made for each
# of its components. According to \samp{how} the returned
# value is vectorial or scalar. When \samp{sch} is zero length then
# \samp{TRUE} is returned. When \samp{ch} is zero length
# then \samp{FALSE} is returned except when \samp{length(sch)} is zero.
#DETAILS
# More possibilities exists when \samp{exact} is \samp{6} or \samp{8}.
# Namely that the substring must be at the end or at the beginning of the
# reference chain.
#KEYWORDS utilities
#INPUTS
#{sch} <<The character string(s) to be found.>>
#{ch}  <<The character string(s) to investigate.>>
#[INPUTS]
#{exact} << When exact, one component must
# be strictly identical, if not a subtring is sufficient.
# See also the \bold{Details} section for two other possibilities.>>
#{how} << Indicates what to do when \samp{length(sch)>1}. The choice are 
# \samp{v}: a logical vector gives back each check independently;
# \samp{1}: returns \samp{TRUE} when at least one of the component belongs
# to the series \samp{ch} and \samp{a} when all components must comply to get TRUE.>>
#{lower} << Must the comparisons being done after case lowering?>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{sssrbsa0$monitor$v} provided object as an example.>>
#VALUE A logical vector with length of \samp{sch}; or \samp{TRUE} or \samp{FALSE} when
#      \samp{how} is \samp{1} or \samp{a}.
#EXAMPLE
# sssbelong9('a','non');
# sssbelong9('o',c('non','oui'));
# sssbelong9('o',c('non','oui'),6);
# sssbelong9('o',c('non','oui'),8);
# sssbelong9('O',c("oui"));
# sssbelong9('O',c("oui"),lower=TRUE);
# sssbelong9(c('o','n'),c('non','oui'),6,how='v');
# sssbelong9(c('o','n'),c('non','oui'),8,how='v');
# sssbelong9(c("o","oui"),c('non','oui'));
# sssbelong9(c("o","oui"),c('non','oui'),how="v");
# sssbelong9(c("A[SEX]","A[AGE]"),c("A[AGE]","A[SEX]"),how="a")
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_08_28
#REVISED 14_07_31
#--------------------------------------------
{
  # degenerate cases
  if (length(sch)==0) { return(TRUE);}
  if (length( ch)==0) { return(FALSE);}
  #
  if (monitor$chk$v) {
      sssobject9(  sch,"character",c(1,Inf));
      sssobject9(   ch,"character",c(1,Inf));
      if (is.logical(exact)) {exact <- exact*10;}
      sssobject9(as.character(exact),"character",1,c(0,6,8,10));
      sssobject9(  how,"character",1);
      # because 'sssobject9' calls 'sssbelong9' for this type of check!
      if (!(how %in% c("v","1","a"))) {
          ssserreur(how,"Not accepted value for 'how'");
      }
    sssobject9(lower,"logical",1);
  }
  if (lower) {
    sch <- tolower(sch);
    ch <- tolower(ch);
  }
  #
  res <- logical(length(sch));
  #
  #
  for (ii in sssbf(sch)) {
    chaine <- sch[ii];
    if (exact== 6) {
      ch <- substr(ch,pmax(1,nchar(ch)-(nchar(chaine)-1)),nchar(ch));
    }
    if (exact== 8) {
      ch <- substr(ch,1,nchar(chaine));
    }
    if (exact== 0) {
      res[ii] <- (length(grep(chaine,ch,fixed=TRUE)) > 0);
    } else {
      res[ii] <- (sum(chaine==ch) > 0);
    }
  }
  # dealing with a synthetic question
  if (how == "1") { res <- any(res);}
  if (how == "a") { res <- all(res);}
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssform3repeat <- function(cha="-",nb=10,imp=FALSE,cr=imp)
#TITLE  prints a repeated given string
#DESCRIPTION
# Component by component,  prints the concatenation
# of the given string(s) repeated \samp{nb} times.
#DETAILS
#KEYWORDS print
#INPUTS
#[INPUTS]
#{cha} << The string to repeat>> 
#{nb} << Number of repetitions>> 
#{imp} << Printing when TRUE or returning (default)>>
#{cr} << Must a line feed be added?>>
#VALUE
# character string or printing according to \samp{imp}
#EXAMPLE
# sssform3repeat('-+',20,TRUE)
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_24
#REVISED 08_09_25
#--------------------------------------------
{
nb <- max(0,round(nb));
res <- "";
for (jbd in sssbc(nb)) { res <- paste0(res,cha);}
if (cr) { res <- paste0(res,"\n");}
if(!imp) { return(res);}
cat(res);
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssform3justify <- function(chaine,
                          nbc=8,
                          format=3,
                          tronc=TRUE,
                          carac=" ",
                       monitor=sssrbsa0$monitor$v)
#TITLE  formats a character string
#DESCRIPTION
# Formats character string(s).
# The main use of this function is to produce
# aligned columns lists while not printing
# them at the same time.
#DETAILS
# The justification is made component by component,
# no concatenation between them is done.
#KEYWORDS print
#INPUTS
#{chaine}<<the character string to be printed, can be a vector.>>
#[INPUTS]
#{nbc} << Desired number of characters for the result; when
#         \samp{chain} is a vector can be a vector of the same length>>
#{format} << Indicates the type of alignment:\cr
#   0 no aligment (no supplementary character added to reach \samp{nbc})\cr
#   1 to the left side\cr
#   2 centered\cr
#   3 to the right side>>
#{tronc} << If true, no more than
#     \samp{nbc} characters are returned and
# possibly the string is truncated. In that
# case, \samp{$} is introduced to indicate the fact.>>
#{carac} << Character to use for enlarging the string>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{sssrbsa0$monitor$v} provided object as an example.>>
#VALUE a character string
#EXAMPLE
# sssform3justify("vers")
# sssform3justify("versification",5)
# sssform3justify(letters[1:5],format=2,carac="+");
#REFERENCE
#SEE ALSO ssstext3preparation
#CALLING
#COMMENT
#FUTURE Improve the behavior when \samp{nchar(carac)>1}.
#AUTHOR J.-B. Denis
#CREATED 1999_05_25
#REVISED   14_08_27
#--------------------------------------------
{
# checking
if (monitor$chk$v) {
    sssobject9(chaine,c("character","numeric"),-1);
    sssobject9(nbc,"integer",c(1,Inf));
}
# the null case
if (length(chaine)==0) { return(character(0));}
# preparing
nbc[nbc < 1] <- 8;
if (length(nbc) < length(chaine)) {
    nnbc <- rep(nbc,length(chaine));
} else {
    nnbc <- nbc;
}
#
itr <- "$"; # truncation indicator
rres <- cchaine <- chaine;
for (rr in sssbf(rres)) {
    res <- rres[rr];
    nbc <- nnbc[rr];
    chaine <- cchaine[rr];
    # truncation
    if ( (nchar(res) > nbc) & tronc ) {
     if (format <= 1) {
      res <- substring(chaine,1,nbc-1);
      res <- paste0(res,itr);
      }
     else {
      if (format == 2) {
       otg <- (nchar(chaine) - nbc) %/% 2;
       res <- substring(chaine,1+otg);
       res <- substring(res,1,nbc-2);
       res <- paste0(itr,res,itr);
       }
      else {
       res <- substring(chaine,1+nchar(chaine)-nbc+1,
			nchar(chaine));
       res <- paste0(itr,res);
       }
      }
     }
    if ((nchar(res) < nbc) & (format != 0)) {
     if (format == 1) {
      while (nchar(res) < nbc) res <-
	     paste(res,"",collapse="",sep=carac);
      }
     else {
      if (format == 2) {
       raj <- (nbc - nchar(res)) %/% 2;
       if (raj > 0) {
	for (jbd in 1:raj) res <-
	 paste(res,"",collapse="",sep=carac);
	}
       while (nchar(res) < nbc) {
	res <- paste("",res,collapse="",sep=carac);
	}
       }
      else {
       while (nchar(res) < nbc) {
	res <- paste("",res,collapse="",sep=carac);
	}
       }
      }
     }
    rres[rr] <- res;
}
# returning
rres;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssget8comp7list <- function(lili,tata,
                       monitor=sssrbsa0$monitor$v)
#TITLE  returns components from a list structure
#DESCRIPTION
# This is not a end-user function at all! To understand it, one must
# have in mind how works the function \samp{sssexplore8list}.\cr
# Returns components from a list structure as a one level list.
# The list \samp{lili} must have been explored with \samp{sssexplore8list}
# and the branch(es) to return are indicated through their line numbers
#  (\samp{tata}) in the table it generates.
#DETAILS
# Names of the produced list are the stacked names of the initial list 
# \samp{lili}.
#KEYWORDS IO
#INPUTS
#{lili} << The list structure components of which have to be extracted.>>
#{tata} << The lines of the table provided by \samp{sssexplore8list}.>>
#[INPUTS]
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{sssrbsa0$monitor$v} provided object as an example.>>
#VALUE
# The resulting list with as many component as indicated rows.
#EXAMPLE
# uu <- list(A=1:3,
#            B=matrix(letters[1:20],nrow=5,dimnames=list(1:5,c("on","tw","th","fo"))),
#            C=list(a=1:3,b=letters,c=list(final=array(1:8,c(2,2,2)))));
# vv <- sssexplore8list(uu);
# sssget8comp7list(uu,vv[7,])[[1]];
##
# uu <- list(1:3,
#            matrix(letters[1:20],nrow=5,dimnames=list(1:5,c("on","tw","th","fo"))),
#            list(a=1:3,b=letters,c=list(final=array(1:8,c(2,2,2)))));
# vv <- sssexplore8list(uu);
# sssget8comp7list(uu,vv[7,])[[1]];
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
    sssobject9(lili,"list",-1,mensaje="lili must be a list");
    #
    sssobject9(tata,"character",-1,
               mensaje="'tata' must be a 'character'");
    if (!is.matrix(tata)) {
        sssobject9(tata,"character",length(nta),
                   mensaje="When not a matrix, 'tata' must have the same length that 'nta'");
    } else {
        if (ncol(tata) != length(nta)) {
            ssserreur(tata,"The 'tata' matrix must have got as many columns as the length of nta");
        }
        if (is.null(dimnames(tata)[[2]])) {
            ssserreur(tata,"The 'tata' matrix must have named columns");
        }
        if (length(union(dimnames(tata)[[2]],nta))>length(nta)) {
            ssserreur(list(tata,nta),"The 'tata' matrix must have named columns with 'nta'");
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
for (ii in sssbc(nrow(tata))) {
    coco <- paste("lili[[",
                  paste(strsplit(nunu[ii],sssrbsa0$sep0$v)[[1]],
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
sssdipa <- function(chemins,sep=.Platform$file.sep)
#TITLE  normalize directory paths
#DESCRIPTION
#  returns the paths contained into \samp{chemins} in
#  a normalized way. No check of existence of the path(s)
#  is performed, for that the function \samp{sssfidi9(sssdipa(...))} could be used.
#DETAILS
#  Multiple \samp{sep} are removed as well as any final \samp{sep}
#  with the exception of \samp{sep} being the unique path, that is
#  the root path in Linux operating system.
#KEYWORDS directory
#INPUTS
#{chemins} << \samp{character} supposed to contain directory paths.>>
#[INPUTS]
#{sep} << The separator to use.>>
#VALUE
# a character with same length as \samp{chemins} 
#EXAMPLE
# sssdipa(c("/","~","~/.bashrc",getwd(),"toto.txt"));
#REFERENCE
#SEE ALSO sssfidi9
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_23
#REVISED 14_06_25
#--------------------------------------------
{
  # checking the arguments
  sssobject9(chemins,"character");
  sssobject9(sep,"character",1);
  # initializing
  se2 <- paste0(sep,sep); lse <- nchar(sep);
  res <- chemins;
  # proceeding
  for (ii in sssbf(res)) {
    # removing multiple "sep"
    while (length(grep(se2,res[ii],fixed=TRUE))>0) {
      res[ii] <- gsub(se2,sep,res[ii],fixed=TRUE);
    }
    # dealing with the last character
    if (nchar(res[ii])>lse) {
      laca <- substr(res[ii],nchar(res[ii])-lse+1,nchar(res[ii]));
      if (laca == sep) { res[ii] <- substr(res[ii],1,nchar(res[ii])-lse);}
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
ssstext3preparation <- function(text,preparation="rbR",
                         content=c(1,length(text)),
                         ccom="#",
                         llength=50)
#TITLE Analyses and prepares a /text/ 
#DESCRIPTION from a text returns another text after some
# transformations.\cr
# Not intended for the standard user.
#DETAILS
#KEYWORDS 
#INPUTS
#{text}<< A \samp{character} vector containing the text
# to prepare.>>
#[INPUTS]
#{preparation} <<A \samp{character(1)} whose characters indicates the actions
# to perform (in this order):
#     \cr\samp{r} to remove the starting characters of \samp{ccom}.
#     \cr\samp{b} to remove 'blank' characters at both ends of lines.
#     \cr\samp{B} to remove repeated 'blank' characters within lines.
#     \cr\samp{R} to remove empty lines.
#     \cr\samp{w} to return the first word of the first line delimited with either blank, \samp{=}
#                 of \samp{<-}.
#     \cr \cr The following options are hierarchized (for instance \samp{j} is equivalent to \samp{cvVSj}).
#     \cr\samp{c} to concatanate lines (between lists if any) into a unique line;
#                 one space is added between two initial lines.
#     \cr\samp{v} to return the vector with one word one component from the concatenated line of \samp{c}.
#     \cr\samp{V} the same as \samp{v} but eliminated repeated words.
#     \cr\samp{S} the same as \samp{v} but sorting the found words.
#     \cr\samp{j} to gather the words obtained after \samp{v} into text components having less that
#                 \samp{llength} characters or only one word.>>
#{content} << Indicates which component of \samp{text} to prepare.
#             Usually \samp{numeric(2)} to indicate the interval of lines to consider.
#             when \samp{numeric(1)}, only this line. When \samp{0} or \samp{diff(content)<0} 
#             \samp{character(0)} is returned.>>
#{ccom} <<A \samp{character(1)} indicating which character(s) at the beginning
#         of lines must possibly be removed (the character(s), not the complete line). >>
#{llength} <<Maximum number of characters for a line (except when it comprises only one word).>>
#VALUE
# The transformed text, that is a \samp{character}.
#EXAMPLE
# ssstext3preparation(sssrbsa0$text2$v[1:3],preparation="j",llength=10)
# uu <- c("Il etait une fois un petit et rouge chaperon",
#         "qui voulait aller voir sa mere-grand");
# ssstext3preparation(uu,"j",llength=20);
# ssstext3preparation(uu,"j",llength=80);
# ssstext3preparation( c(" Je veux   voir  "," et re-voir  "),"rbBc")
# ssstext3preparation(c("# Je veux   voir  "," et re-voir  "),"rbBc")
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE Other functionalities could be added.
#AUTHOR J.-B. Denis
#CREATED 13_10_14
#REVISED 14_07_10
#--------------------------------------------
{
  # initializing
  res <- character(0);
  if (length(text)==0) { return(res);}
  # checking
  sssobject9(text,
             "character",
             mensaje="Bad 'text' argument");
  sssobject9(content,
             "numeric",len=1:2,con=c(0,Inf),
             mensaje="Bad 'content' argument (1)");
  if (content[1]==0)   { return(res);}
  if (diff(content)<0) { return(res);}
  if (length(content)!=2) { content <- rep(content[1],2);}
  sssobject9(content,"numeric",len=2,con=c(1,length(text)),
               mensaje="Bad 'content' argument (2)");
  #
  sssobject9(preparation,
             "character",len=1,
             mensaje="Bad 'preparation' argument");
  sssobject9(ccom,
             "character",len=1,
             mensaje="Bad 'ccom' argument");
  sssobject9(llength,
             "numeric",len=1,con=c(1,Inf),
             mensaje="Bad 'llength' argument");
  # restricting the treatment
  text <- text[content[1]:content[2]];
  ## removing starting tags
  if (sssbelong9("r",preparation)) {
    nbc <- nchar(ccom);
    if (nbc>0) {
      for (tt in sssbf(text)) {
        tex <- text[tt];
        if (substr(tex,1,nbc)==ccom) {
          text[tt] <- substr(tex,nbc+1,nchar(tex));
        }
      }
    }
  }
  ## removing ending blanks
  if (sssbelong9("b",preparation)) {
    for (tt in sssbf(text)) {
      tex <- text[tt];
      while (substr(tex,1,1)==" ") {
        tex <- substr(tex,2,nchar(tex));
      }
      while (substr(tex,nchar(tex),nchar(tex))==" ") {
        tex <- substr(tex,1,nchar(tex)-1);
      }
      text[tt] <- tex;
    }
  }
  ## removing repeated blanks
  if (sssbelong9("B",preparation)) {
    for (tt in sssbf(text)) {
      tex <- text[tt];
      while (sssbelong9("  ",tex)) {
        tex <- gsub("  "," ",tex);
      }
      text[tt] <- tex;
    }
  }
  res <- text;
  ## removing empty lines
  if (sssbelong9("R",preparation)) {
    for (tt in rev(sssbf(text))) {
      if (nchar(text[tt])==0) {
        text <- text[-tt];
      }
    }
  }
  res <- text;
  ## looking for the first word
  if (sssbelong9("w",preparation)) {
    if (length(text)== 0) {
      ssserreur(NULL,"No line found to get a first word");
    }
    tex <- text[1];
    n1 <- nchar(strsplit(tex," ",fixed=TRUE)[[1]][1]);
    n2 <- nchar(strsplit(tex,"<-",fixed=TRUE)[[1]][1]);
    n3 <- nchar(strsplit(tex,"=",fixed=TRUE)[[1]][1]);
    ftr <- min(n1,n2,n3);
    return(substr(tex,1,ftr));
  }
  ## concatenation
  if (sssbelong9("c",preparation)|
      sssbelong9("v",preparation)|
      sssbelong9("V",preparation)|
      sssbelong9("S",preparation)|
      sssbelong9("j",preparation)) {
    res <- paste(res,collapse=" ");
  }
  ## vectorisation
  if (sssbelong9("v",preparation)|
      sssbelong9("V",preparation)|
      sssbelong9("S",preparation)|
      sssbelong9("j",preparation)) {
    res <- strsplit(res," ",fixed=TRUE)[[1]];
  }
  ## eliminating redundancy
  if (sssbelong9("V",preparation)|
      sssbelong9("S",preparation)|
      sssbelong9("j",preparation)) {
    res <- unique(res);
  }
  ## sorting
  if (sssbelong9("S",preparation)|
      sssbelong9("j",preparation)) {
    res <- sort(res);
  }
  ## justification
  if (sssbelong9("j",preparation)) {
    if (length(res)>0) {
      rrr <- res[-1]; res <- res[1];
      for (cc in sssbf(rrr)) {
        if (nchar(res[length(res)])+1+nchar(rrr[cc]) <= llength) {
          res[length(res)] <- paste0(res[length(res)]," ",rrr[cc]);
        } else {
          res <- c(res,rrr[cc]);
        }
      }
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssform3names <- function(nbn,nom=character(0),prefix="",
                           upca=TRUE,nume=14,
                       monitor=sssrbsa0$monitor$v)
#TITLE  provides systematic names for items
#DESCRIPTION
# Provides systematic names for a series of items according to their 
# number and taking care of previous names.
#DETAILS
#KEYWORDS utilities
#INPUTS
#{nbn} <<Number of new item names to generate>>
#[INPUTS]
#{nom} << Already present names (to avoid identical names).>>
#{prefix} << Systematic prefix of all names to generate. Must
#                 comprise the dot, if one wants such a separator
#                 between it and the specific part of the name. 
#                 Of course can be 'underscore' or whatever else.>>
#{upca} << Indicates whether the letters constituting the new
#          names must be uppercase or not.>>
#{nume} << Its absolute value gives the number of the letter to use
#          when the alphabet is not sufficient. When negative, alphabet
#          is not considered as a first possibility. For instance 2 will
#          indicate "B" and the default is "N". When 0, no letter is considered.>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{sssrbsa0$monitor$v} provided object as an example.>>
#VALUE
# vector with \samp{nbn} different strings associated to new names
#EXAMPLE
# sssform3names(2);
# sssform3names(2,nume=-3);
# sssform3names(2,prefix="rbsa.");
# sssform3names(2,upca=FALSE);
# sssform3names(5,"D");
# sssform3names(5,"Y");
# sssform3names(30);
#REFERENCE
#SEE ALSO sssform3numbering
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_19
#REVISED 10_02_15
#--------------------------------------------
{
# checking
if (monitor$chk$v) {
    sssobject9(upca,"logical",1,mensaje="Argument 'upca' not accepted");
    sssobject9(nume,"integer",1,mensaje="Argument 'nume' not accepted");
    if (abs(nume)>26) {
        ssserreur(nume,"'nume' must be comprised between -26 and 26 to indicate a Letter");
    }
}
#
if (upca) { Letters <- LETTERS;
} else { Letters <- letters;}
#
if (sssvoid9(nbn)) { return(character(0));}
if (nbn < 1) { return(character(0));
} else {
    if (prefix != "") {
        # keeping only the names having got the prefix then
        # removing the prefixes from them (to be added further)
        decom <- sapply(strsplit(nom,prefix),function(ll){length(ll);});
        nom <- nom[decom == 2];
        nom <- sapply(strsplit(nom,prefix),function(ll){ll[2];});
    }
    # looking for the maximum letter in noms
    if ( length(nom) == 0 ) { mama <- 0;
    } else { mama <- max(1*outer(nom,Letters,"==") %*% matrix(1:26,ncol=1));}
    if ((nbn < (27-mama)) & (nume>=0)) {
        # adding letters 
        res <- Letters[mama+(1:nbn)];
    } else {
        # adding numbered nodes
        ajou <- 0; nu <- 1; res <- character(0);
        if (nume==0) { malettre <- "";}
        else { malettre <- Letters[abs(nume)];}
        while ( ajou < nbn ) {
          nono <- paste0(malettre,nu);
          if (all(nono != nom)) {
              ajou <- ajou + 1;
              res <- c(res,nono);
          }
          nu <- nu+1;
        }
    }
}
# adding the prefix
res <- paste0(prefix,res);
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssform3numbering <- function(nbn,type="n",bef="-",aft="-",
                           start=1+(type=="n"),
                           monitor=sssrbsa0$monitor$v)
#TITLE  provides a systematic numbering of items
#DESCRIPTION
# Provides systematic names for a series of items according their 
# number giving them an identical number of characters.
#DETAILS
# Alphabetical series are "aa","ab",...,"az","ba",... Then starting
# from the second element means to start from "ab". It is not possible
# to start from the 0th element.\cr
# Numerical series are "00","01","02",...,"09","10",... Then one
# can start from the zeroth element which is "00" but starting from
# the second element means starting from "01". This inconsistency
# may look not desirable, but it allows to follow the natural
# reasoning when working in any of the two logics.
#KEYWORDS utilities
#INPUTS
#{nbn} <<Number of item names to generate from \samp{start}.>>
#[INPUTS]
#{type} << Defines the type of numbering to use. There are
#          three possibilities: \samp{A} for uppercased letters,
#          \samp{a} for lowercased letters and \samp{n} for
#          arabic numbers.>>
#{bef} << To put before the number.>>
#{aft} << To put after  the number.>>
#{start} << The first number to issue (>0).>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{sssrbsa0$monitor$v} provided object as an example.>>
#VALUE
# character vector with \samp{nbn} different strings associated to names
#EXAMPLE
# sssform3numbering(27);
# sssform3numbering(27,start=1);
# sssform3numbering(100,"a");
# sssform3numbering(100,"A");
#REFERENCE
#SEE ALSO sssform3names
#CALLING
#COMMENT
#FUTURE
# Monitor the number of digits to obtain series of "Z01", "Z02",... ,"Z79", "Z80".
#AUTHOR J.-B. Denis
#CREATED 07_10_19
#REVISED 14_08_05
#--------------------------------------------
{
# checking
if (monitor$chk$v) {
    sssobject9(start,"integer",1,con=c(1,Inf),mensaje="sssform3numbering: argument 'start' not accepted");
    sssobject9(  nbn,"integer",1,con=c(1,Inf),mensaje="sssform3numbering: argument 'nbn' not accepted");
    sssobject9(type,"character",1,c("A","a","n"),mensaje="sssform3numbering: argument 'type' not accepted");
    if ((start==0) & (type != "n")) { ssserreur(list(start=start,type=type),
                                             "The two argument values are not compatible");}
}
#
# degenerate case
if (nbn < 1) { return(character(0));}
nbn <- nbn + start - 1;
#
# determining the number of digits and them
if (tolower(type)=="a") {
  nbd <- 26;
  if (type =="a") { digi <- letters;
  } else { digi <- LETTERS;}
} else {
  nbd <- 10;
  digi <- as.character(0:9);
}
nbp <- 1;
while (nbn > nbd^nbp) { nbp <- nbp + 1;}
#
# getting the numbers
res <- rep("",nbn);
for (ii in sssbc(nbp)) {
  jj <- rep(digi,times=nbd^(nbp-ii),each=nbd^(ii-1))[1:nbn];
  res <- paste0(jj,res);
}
#
if (start>1) {res <- res[-sssbc(start-1)];}
#
# surrounding
res <- paste0(bef,res,aft);
#
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssrbsa7list9 <- function(lili,monitor=sssrbsa0$monitor$v)
#TITLE checks whether a list is rbsa-compatible list
#DESCRIPTION
# To be a rbsa-list, a list must satisfy the following two properties:
# (i) all components and sub-components are named.
# (ii) all components and sub-components are either a list
# or a character vector/matrix/array (i.e. vma components); they are 
# the leafs of the rbsa-list.\cr
# To be handled by \samp{ssslist2file} or \samp{sssfile2list} functions, a list must
# rbsa-compatible.
#DETAILS
#KEYWORDS IO
#INPUTS
#{lili} << The list structure to be checked.>>
#[INPUTS]
#{monitor} << List of monitoring constants, see \samp{sssrbsa0$monitor$v} to
#             know its structure.>>
#VALUE
# \samp{TRUE} or \samp{FALSE} according to the results of the checks.
#EXAMPLE
# sssrbsa7list9(sssrbsa0$lis1$v);
# sssrbsa7list9(list(sssrbsa0$lis1$v));
# sssrbsa7list9(list(sssrbsa0$lis1$v,nu=NULL));
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_04_19
#REVISED 10_04_20
#--------------------------------------------
{
# checking
if (monitor$chk$v) {
    sssobject9(lili,"list",-1,mensaje="lili must be a list");
}
# exploring the list
eee <- sssexplore8list(lili);
# checking the presence on unamed components
#          and the types of the list
comment <- character(0);
for (ii in sssbc(nrow(eee))) {
    if ((eee[ii,"name"]=="<NA>")|(eee[ii,"name"]=="")) {
        comment <- c(comment,
                     paste("The component",eee[ii,"numbers"],
                           "has got no name:",
                           paste("'",eee[ii,"names"],"'",sep=""))
                    );
    }
    coco <- sssget8comp7list(lili,eee[ii,,drop=FALSE],monitor=monitor);
    if (length(coco) > 0) { coco <- coco[[1]];}
    if (!(is.list(coco) |
          is.numeric(coco) |
          is.character(coco) | 
          is.matrix(coco) | 
          is.array(coco))) {
        comment <- c(comment,
                     paste("The component (",eee[ii,"numbers"],
                           ") with name: '",eee[ii,"names"],
                           "is not list/vector/matrix/array")
                    );
    }
}
# preparing the result
res <- sssvoid9(comment);
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssdisplay8k <- function(constants,what="exhibit")
#TITLE  returns information about a /rbsa/ constant
#DESCRIPTION
# Exhibits a \samp{rbsa} constant 
# or returns a \samp{character} vector for the names and
# the definitions of \samp{constants} or a \samp{list} for
# the values.
#DETAILS
# A \samp{rbsa} type constant is a named list, each component being
# a list of two components: \samp{$d} for the definition which must be
# a character (concatenated if not a scalar) and \samp{$v} which
# can take any value.
#KEYWORDS misc helpful
#INPUTS
#{constants} << The \samp{rbsa} constant list (see the details for the description).>>
#[INPUTS]
#{what}    <<a character(1) indicating what must be returned. The acceptable
#           values are \samp{exhibit}, \samp{names}, \samp{definitions} and \samp{values}.
#           For user convenience, the argument is lowercased before checked,
#           and only the first character is taken into consideration.>>
#VALUE
# Information is displayed and nothing is returned for exhibition
# else a named \samp{character} or a named \samp{list}. 
#EXAMPLE
# sssdisplay8k(sssrbsa0)
# sssdisplay8k(sssrbsa0$monitor$v,"v")
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 14_05_21
#REVISED 14_06_03
#--------------------------------------------
{
  # checking
  sssobject9(what,"character",1,mensaje="Awaiting for a character(1)");
  what <- tolower(what);
  what <- substr(what,1,1);
  sssobject9(what,"character",1,c("d","v","n","e"),
          "Must be 'names', 'descriptions', 'values' or 'exhibit'");
  # looking if it is a constant or a list of constants
  nana <- names(constants);
  if (length(unique(c(nana,"v","d")))==2) {
    # must be a constant
    lcon <- list(k=constants);
  } else {
    # must be a list of constant
    lcon <- constants;
  }
  # filling values
  if (what == "v") {
    res <- vector("list",length(lcon));
    names(res) <- names(lcon);
    for (vv in sssbf(lcon)) {
      res[[vv]] <- lcon[[vv]]$v;
    }
  }
  # filling descriptions
  if (what == "d") {
    res <- vector("character",length(lcon));
    names(res) <- names(lcon);
    for (vv in sssbf(lcon)) {
      res[vv] <- paste(lcon[[vv]]$d,collapse=" / ");
    }
  }
  # filling names
  if (what == "n") {
    res <- names(lcon);
  }
  # displaying
  if (what == "e") {
    for (cc in sssbf(lcon)) {
      sssform3title(names(lcon)[cc],4);
      print(lcon[[cc]]$d);
      print(lcon[[cc]]$v);
    }
    res <- invisible();
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssrbsa0 <- 
#TITLE list of the /rbsa/ constants
#DESCRIPTION
# Just a list of constants. Each constant is defined with a name, a definition
# and a value. The initial values can be modified by the user. Some
# specifies the way the \pkg{rbsa} calculations and displays are made. Values
# definitions and names can be obtained with the function \samp{sssdisplay8k}.\cr
# An important role of the constant is to drive the behavior of many functions
# through \samp{sssrbsa0$monitor$v} which is itself a \samp{rbsa} constant.
#DETAILS
# It is a named list, one component for each constant.
# A named sublist is associated to each constant with two components: \samp{$d}
# for the definition and \samp{$v} for the value. Be aware that the value
# can be any object (vector, list, matrix, function,...)
#KEYWORDS misc helpful
#INPUTS
#[INPUTS]
#VALUE
# A list see the details for the description.
#EXAMPLE
# print(sssdisplay8k(sssrbsa0,"names"));
# print(sssdisplay8k(sssrbsa0));
# print(sssdisplay8k(sssrbsa0$monitor$v,"names"));
# print(sssdisplay8k(sssrbsa0$monitor$v));
# str(sssrbsa0$monitor$v);
#REFERENCE
#SEE ALSO sssrbsa7list9 sssdisplay8k
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_09_16
#REVISED 14_09_01
#--------------------------------------------
{
  # additional variables
  axfpx <- "graph";
  vma <- c("c","C","v","V","u","U","m","n","o","p","M","N","O","P","a","A","b","B");
  names(vma) <- vma;
  # definition of the different constants
  list(
       mwi=list(v=70,
                d="The width (nbr of characters) for printing paragraphs"),
       mfi=list(v=TRUE,
                d="Must the results be directed to files (if not to the screen)?"),
       mgr=list(v="pdf",
                d="Type of graphics files"),
       mnd=list(v=3,
                d="Number of decimals when printing"),
       mep=list(v=1,
                d="When printing an object: *e*m*p*hasize level"),
       fpx=list(v=axfpx,
                d="Prefix for the resulting files"),
       fou=list(v=paste(axfpx,"txt",sep="."),
                d="Standard file for text outputs"),
       fin=list(v=paste(axfpx,"int.txt",sep="."),
                d="Standard intermediary file"),
       tyl=list(v=c("A","a","n"),
                d="Types of bullets for standard lists"),
       l00=list(v="~",
                d="Symbol for the null bullets (see sssread8list)"),
                # due to the use of easyp3cut, no nesting parenthesis are allowed!
                # also opening and closing must be different
       cpt=list(v= matrix(c("{{","}}",
                            "(|","|)",
                            "[" ,"]" ,
                            "<<",">>"),ncol=2,
                          byrow=TRUE,dimnames=list(c("nodes","rounding","variables","vectors"),
                                                   c("opening","closing"))),
                d="Different closing parentheses as a dimnamed matrix"),
       nch1=list(v=structure("1", .Names = "A"),
                 d="Example 1 of a named character objects"),
       nnu1=list(v=structure(1, .Names = "A"),
                d="Example 1 of a named numeric objects"),
       lis1=list(v=list(A=1:3,
                        B=matrix(letters[1:20],nrow=5,dimnames=list(1:5,c("on","tw","th","fo"))),
                        C=list(a=1:3,b=letters,c=list(final=array(1:8,c(2,2,2))))),
                 d="Example1 of a list objects"),
       dfr1=list(v=data.frame(F1=factor(rep(1:3,each=4)),F2=factor(rep(1:4,3))),
                 d="Example1 of data.frame object"),
       dfr2=list(v=data.frame(SEX=factor(c("M","M","F","F","F","M")),
                              AGE=20+5*(1:6),
                              HGT=185-1:6),
                 d="Example2 of data.frame object"),
       dfr3=list(v=data.frame(SEX=factor(c(rep("M",50),rep("F",50))),
                              AGE=round(40+sin(1:100)*20),
                              HGT=c(round(170+cos(1:50)*17),round(155+cos(1:50)*16)),
                              WGT=c(round(75+cos(50:1)*15),round(60+cos(50:1)*20))),
                d="Example3 of data.frame object"),
       #
       vma=list(v=vma,
                d="Different types of vma (see ssstext2vma for details)"),
       sep0=list(v=" ",
                 d="Basic string separator"),
       sep1=list(v="//",
                 d="Second string separator"),
       sep2=list(v=";",
                 d="Third string separator"),
       tag1=list(v=matrix(c("<<",">>",
                            "[[","]]",
                            "((","))"),
                          ncol=2,byrow=TRUE),
                 d="Standard tagging for text file associated to list"),
       tag2=list(v=matrix(c("[[","]]",
                            "((","))"),
                          ncol=2,byrow=TRUE),
                d="Alternative tagging for text file associated to list"),
       # 
       monitor=list(v=list(chk=list(v=TRUE, d="must arguments be checked?"),
                           pau=list(v=FALSE, d="must call to ssspause be effective in some functions?"),
                           war=list(v=FALSE,d="must warnings be considered as fatal errors"),
                           fat=list(v=TRUE, d="must fatal errors be considered as fatal errors"),
                           ind=list(v=2,    d="size for indentation")),
                    d="global variable to monitor the behavior of some /rbsa/ functions"),
       tgs=list(v=list(
                  deb=list(v="<<<",t="Start",d="Starting point"),
                  mil=list(v="---",t="Middle",d="Intermediary point"),
                  fin=list(v=">>>",t="End",d="Ending point")
                 ),
                d="Example of tagging for text"),
       text1=list(v=c(paste0(rep(c(1:9,0),1),collapse=""),
                      paste0(rep(c(1:9,0),2),collapse=""),
                      paste0(rep(c(1:9,0),3),collapse=""),
                      paste0(rep(c(1:9,0),4),collapse=""),
                      paste0(rep(c(1:9,0),3),collapse=""),
                      paste0(rep(c(1:9,0),2),collapse=""),
                      paste0(rep(c(1:9,0),1),collapse="")),
                  d="text example number 1"),
       text2=list(v=c("Once upon a time there was a sweet little girl.",
                      "Everyone who saw her liked her, but most of all her grandmother",
                      "Once she gave her a little cap made of red velvet.",
                      "Because it suited her so well, and she wanted to wear it all the time,",
                      "she came to be known as Little Red Riding Hood.",
                      "One day her mother said to her: ",
                      "Come Little Red Riding Hood. Here is a piece of cake and a bottle of wine.",
                      "Take them to your grandmother.",
                      "She is sick and weak, and they will do her well.",
                      "Mind your manners and give her my greetings.",
                      "Behave yourself on the way, and do not leave the path,",
                      "or you might fall down and break the glass,",
                      "and then there will be nothing for your sick grandmother."),
                  d="text example number 2"),
       text3=list(v=c("<<< A gentle introduction!",
                      "---",
                      "A quite interesting first part",
                      "---",
                      "The core of the story (1)",
                      "The core of the story (2)",
                      "The core of the story (3)",
                      "---",
                      "Some concluding remarks.",
                      ">>>"),
                  d="text example number 3 (tagged)"),
       text4=list(v=c("Every paragraph must be followed ",
                      "with an empty line",
                      "",
                      "A quite interesting first part",
                      "---",
                      "---",
                      "",
                      "{#} << First item>>",
                      "{#} <<Second item>>",
                      "{#} << Third item",
                      "       can comprise several",
                      "       lines!>>",
                      "{#} <<Fourth item>>",
                      "",
                      "Some concluding remarks."),
                  d="text example number 4 (tagged)"),
       text5=list(v=c("Every paragraph must be followed ",
                      ")BULLETS( * + -",
                      "* FIRST LEVEL 1",
                      " + Second Level 1",
                      "   - third level 1",
                      "   - third level 2",
                      " + Second Level 2",
                      " + Second Level 3",
                      "* FIRST LEVEL 2",
                      " + Second Level 1",
                      " + Second Level 2",
                      "   - third level 1",
                      "   - third level 2",
                      "* FIRST LEVEL 3"),
                  d="text example number 5 (tagged)")
    );
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssobject9 <- function(x,typ,len=-1,con=numeric(0),
                       mensaje=NULL,fatal=TRUE,na.rm=FALSE,
                       cla=NULL,speci=NULL)
#TITLE  checks the type, the length and the content of some standard object
#DESCRIPTION
#  checks the type, the length and the content
# of some standard object.
# If not correct, a message and a fatal error are issued.
# \samp{NA} are detected and considered as wrong or not.\cr
# When \samp{typ} is 'directory' or 'file', \samp{x} must be
# a character containing path(s) of directories or files.
#DETAILS
# 'integer' has not got the meaning in \samp{is.integer} R
# standard function. 'null' must be understood as
# resulting TRUE with function \samp{sssvoid9}.
#KEYWORDS error
#INPUTS
#{x} <<object to be checked.>>
#{typ} << Character vector indicating the accepted types
# for the object to be checked.
# See the code for the list of accepted types, among
# them 'null', 'integer', 'numeric', 'character',
# 'logical', 'list', 'any', 'function', 'data.frame',
# 'matrix','file','directory',...
# As indicated, 'any' implies that no
# check of the type is performed.>>
#[INPUTS]
#{len} <<If \samp{length(len)==1}, the exact length
# to be checked, if not must be of length two for
# the possible range of \samp{length(x)}. When -1,
# no check on the length is performed.\cr
# For data.frame, it is the number of columns.
# When \samp{na.rm} the check is made after
# removing the NA values.>>
#{con} << If \samp{length(con)>0}, some check about
# the content of \samp{x} is done for some of the 
# types. More precisely for (integer, numeric): all
# values must belong to the interval \samp{[con[1],con[2]]}
# and for (character), the set of possible \samp{character(1)}
# is described by \samp{con}.>>
#{mensaje} << Some additional message to be
#            issued before stopping in case of error.>>
#{fatal} << what to do when discovering
#           an inconsistency ? TRUE: this function prints the message
#           and stops; FALSE: this function returns
#           the message as a character.>>
#{na.rm} << Must the \samp{NA} values be removed before checking?
#           This is implemented only for the types integer, numeric,
#           character and logical.>>
#{cla} << The class to which the object is supposed to belong. When \samp{NULL}
# no check is made with respect to the class.>>
#{speci} << Some additional specific check for some object types.
#           The following possibilities are implemented:\cr
#           - \samp{NULL} when not used,\cr
#           - \samp{"named"} to indicate that a vector must be named,\cr
#           - a \samp{matrix} with two rows and two column for matrix objects
#             to indicate intervals for the row number (first
#             row) and the column number (second row); \samp{NA} values means
#             no check,\cr
#           - a \samp{matrix} with \samp{ndim+1} rows and two columns for
#             array objects to indicate intervals for the number of
#             dimensions and the size of each of them; \samp{NA} values means
#             no check.
#         >>
#VALUE
# When everything is OK returns a \samp{character(0)}.
# If not according to the argument \samp{fatal} print an error message
# and stops, or just returns the message as a warning.
#EXAMPLE
# sssobject9(letters,c("numeric","character"),c(20,30),mensaje="!OK");
# sssobject9(structure("A",.Names="a"),"ncharacter",-1,LETTERS);
# sssobject9("A","ncharacter",-1,mensaje="BAD",fatal=FALSE);
# sssobject9(structure("A",.Names="a"),"character",-1,letters,"bad",fatal=FALSE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE (i) improves the treatment of 'NA's; (ii) allows the check of forbidden values
#AUTHOR J.-B. Denis
#CREATED 09_05_02
#REVISED 14_08_16
#--------------------------------------------
{
# checking the arguments and preparation
# accepted types by sssobject9
typena <- c("integer","numeric","character",
           "logical","ninteger","nnumeric",
           "ncharacter","nlogical",
            "file","directory");
# In typena are those types from which NA values 
# can be removed
types <- c(typena,"list","function","any",
           "data.frame","matrix","array","null");
type <- match(typ,types);
if (all(is.na(type))) {
    ssserreur(list(types,typ),
           "OBJECT9: the proposed type was not in the list!"
          );
}
res <- character(0);
# dealing with possible NAs
narm <- 0;
if (na.rm) { if (all(!is.na(match(typ,typena)))) { 
    nu <- !is.na(x);
    narm <- length(x) - length(nu);
    x <- x[nu];
}}
#
# the null case as 'x'
if (is.null(x)) {
    if ("null" %in% typ) { return(character(0));
    } else { 
        res <- paste("The proposed object is NULL not in '",
                     paste(typ,collapse="/"),"'");
	if (!is.null(mensaje)) { res <- c(res,mensaje);}
        cat(paste0(rep("~",60),collapse=""),"\n");
	cat("From sssobject9: in fact 'x' is '",
	    deparse(substitute(x)),"'\n");
	print.default(x);
	if (fatal) {ssserreur("Message:",paste(res));}
    }
}
if (all(!is.na(match(typ,typena)))) { if (any(is.na(x))) { 
    ssserreur(typ,"OBJECT9: 'x' is or comprises 'NA' which were not removed!");
}}
# possible type for the check
type <- types[type[!is.na(type)]];
if (!is.numeric(len)) {
    ssserreur(len,"OBJECT9: 'len' must be NUMERIC of size one or two");
}
if ((length(len)==0)|(length(len)>2)) {
    ssserreur(len,"OBJECT9: 'len' must be numeric of size ONE or TWO");
}
# class check
if (!is.null(cla)) {
  if (!(class(x)%in%cla)) {
    res <- c(res,paste("The class is ",class(x)," not in /",
                       paste(cla,collapse="/"),"/",sep=""));
  }
}
# processing for the length
if (length(len)==1) {
    if (len>=0) { if (length(x)!=len) {
        if (narm>0) { res <- c(res,paste(narm,"components were removed"));}
        res <- c(res,paste("OBJECT9: 'x' is not of length ",len));
    }}
} else {
    if ((length(x)<len[1])|(length(x)>len[2])) {
        if (narm>0) { res <- c(res,paste(narm,"components were removed"));}
        res <- c(res,paste("OBJECT9: 'x' has got a length outside:",paste(len,collapse=":")));
    }
}
# processing for the type
if (!("any" %in% type)) {
    ty <- FALSE;
    for (tt in type) {
        #
        if (tt=="integer")   { if (is.numeric(x)) {
            if (all(x==round(x))) {
                ty <- TRUE;
            }
        }}
        if (tt=="ninteger")   { if (is.numeric(x)) {
            if (all(x==round(x))) { if (length(names(x))>0) {
                ty <- TRUE;
            }}
        }}
        #
        if (tt=="numeric")   { if (is.numeric(x))   {
            ty <- TRUE;}
        }
        if (tt=="nnumeric")   { if (is.numeric(x)) {
            if (is.character(attributes(x)$names)) { ty <- TRUE;}
        }}
        #
        if (tt=="character") { if (is.character(x)) {
            ty <- TRUE;}
        }
        if (tt=="ncharacter")   { if (is.character(x)) {
            if (is.character(attributes(x)$names)) { ty <- TRUE;}
        }}
        #
        if (tt=="logical")   { if (is.logical(x))   {
            ty <- TRUE;}
        }
        if (tt=="nlogical")   { if (is.logical(x)) {
            if (is.character(attributes(x)$names)) { ty <- TRUE;}
        }}
        #
        if (tt=="function")  { if (is.function(x))  { ty <- TRUE;}}
        if (tt=="list")      { if (is.list(x))      { ty <- TRUE;}}
        if (tt=="data.frame"){ if (is.data.frame(x)){ ty <- TRUE;}}
        if (tt=="matrix")    { if (is.matrix(x))    { ty <- TRUE;}}
        if (tt=="array")     { if (is.array(x))     { ty <- TRUE;}}
        if (tt=="null")      { if (sssvoid9(x))        { ty <- TRUE;}}
        #
        if (tt=="file") {
          if (all(sssfidi9(x) == "f")) { ty= TRUE;
          } else { res <- "Not all components were detected as file(s)";}
        }
        if (tt=="directory") {
          if (all(sssfidi9(x) == "d")) { ty= TRUE;
          } else { res <- "Not all components were detected as directory(ies)";}
        }
    }
    if (!ty) {
        res <- c(res,paste("Among type = ",paste(type,collapse="/")));
        rr <- "The class of 'x' is '";
        if (is.character(attributes(x)$names)) {
            rr <- c(rr,"named ");
        }
        rr <- c(rr,paste0(class(x),"'!"));
        res <- c(res,paste(rr,collapse=""));
        res <- c(res,"OBJECT9: 'x' does not belong to any of these!");
    }
}
#
# proceding for the content
if (identical(res,character(0))) { if (length(con)>0) {
    if (is.numeric(x)) {
        if (length(con) >= 2) {
            cond <- (x >= con[1]) & (x <= con[2]);
            if (!all(cond)) { res <- c(res,"Does not belong to the prescribed interval"); }
        } else {
            ssserreur(list(typ,con),
                   paste("OBJECT9:",
                         "For this 'typ', non empty 'con'",
                         "must define an interval with at",
                         "least two numerical values"));
        }
    }
    #
    if (is.character(x)) {
        cond <- (x %in% con);
        if (!all(cond)) { res <- c(res,"Does not belong to the prescribed values"); }
    }
}}
#
# checking the specific requirement
if (!is.null(speci)) {
  # for vectors
  if (is.vector(x)) {
    if (speci=="named") {
      if (is.null(names(x))) {
        res <- c(res,"This vector was required to be named and isn't.");
      }
    } else {
      ssserreur(speci,"For vectors, the only valid value for 'speci' is 'named'");
    }
  }
  # for matrices
  if (is.matrix(x)) {
    if (is.matrix(speci)) {
      if (all(dim(speci)==2)) {
        if (!any(is.na(speci[1,]))) {
          if (abs(sssinterv7belonging(nrow(x),speci[1,]))>1) {
            inter <- paste(speci[1,],collapse="-");
            res <- c(res,paste("The number of rows is not in the specified",inter,"interval"));
          }
        }
        if (!any(is.na(speci[2,]))) {
          if (abs(sssinterv7belonging(ncol(x),speci[2,]))>1) {
            inter <- paste(speci[2,],collapse="-");
            res <- c(res,paste("The number of columns is not in the specified",inter,"interval"));
          }
        }
      } else {
        ssserreur(speci,"For matrices, the only valid value for speci is a matrix of dimension '2x2'");
      }
    } else {
      ssserreur(speci,"For matrices, the only valid value for speci is a matrix of dimension '2x2'");
    }
  }
  # for arrays
  if (is.array(x) & !(is.matrix(x))) {
    if (is.matrix(speci)) {
      if (ncol(speci)==2) {
        if (!any(is.na(speci[1,]))) {
          if (abs(sssinterv7belonging(length(dim(x)),speci[1,]))>1) {
            inter <- paste(speci[1,],collapse="-");
            res <- c(res,paste("The number of dimensions is not in the specified",inter,"interval"));
          }
        }
        for (dd in sssbf(dim(x))) { if (nrow(speci)>dd) {
          if (!any(is.na(speci[dd+1,]))) {
            if (abs(sssinterv7belonging(dim(x)[dd],speci[dd+1,]))>1) {
              res <- c(res,paste("The dimension size number",dd,"is not in the specified",
                                 paste(speci[dd+1,],collapse="-"),"interval"));
            }
          }
        }}
      } else {
        ssserreur(speci,"For arrays, the only valid value for speci is a matrix with '2' columns");
      }
    } else {
      ssserreur(speci,"For arrays, the only valid value for speci is a matrix with '2' columns");
    }
  }
}
#
# returning
if (!identical(res,character(0))) {
    if (!is.null(mensaje)) { res <- c(res,mensaje);}
    if (fatal) {
        cat(paste(rep("~",60),collapse=""),"\n");
	cat("From sssobject9: in fact 'x' is '",
	    deparse(substitute(x)),"'\n",sep="");
	print.default(x);
        print(res);
        ssserreur(NULL,"FATAL ERROR from sssobject9");
    }
}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssbc <- function(nb)
#TITLE  sequences for easy looping
#DESCRIPTION
# This function returns \samp{1:nb} when \samp{nb > 0} and
#         \samp{numeric(0)} otherwise.
# Quite useful to prevent starting
# a loop of length nought
#DETAILS
# nb is rounded before use
#ALIAS looping
#KEYWORDS iteration helpful
#INPUTS
#{nb}    <<length of the loop>>
#[INPUTS]
#VALUE
# \samp{1:nb} if \samp{nb > 0}
# else \samp{numeric(0)}.
#EXAMPLE
# sssbc(0);
# sssbc(5);
# sssbc(pi);
# sssbc(4*pi);
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
# sssbc for BouCle (loop in French)
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_18
#REVISED 09_04_05
#--------------------------------------------
{
if (is.null(nb)) {return(numeric(0));}
if (length(nb)!=1) {
    ssserreur(nb,"sssbc deals only with scalar nb");
}
if (nb > 0) {return(1:max(1,round(nb)));}
numeric(0);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssbd <- function(n1,n2)
#TITLE  sequence for insertions
#DESCRIPTION
# This function returns \samp{n1:n2} when \samp{n1<=n2} and
#         \samp{numeric(0)} otherwise.
# Quite useful when some insertion must be done within
# a sequence
#DETAILS
#ALIAS
#KEYWORDS iteration helpful
#INPUTS
#{n1}    <<first element>>
#{n2}    <<second element>>
#[INPUTS]
#VALUE
# \samp{n1:n2} if \samp{n1<n2}
# else \samp{numeric(0)}.
#EXAMPLE
# xx <- 1:5;
# for (ii in 1:6) { print(c(xx[sssbd(1,ii-1)],10,xx[sssbd(ii,5)]));}
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
# sssbc for BouCle (loop in French)
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_12
#REVISED 11_01_12
#--------------------------------------------
{
if (n1 <= n2) {return(n1:n2);}
numeric(0);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sssbf <- function(x)
#TITLE  sequence for looping over an object
#DESCRIPTION
# This function returns \samp{1:length(x)} when \samp{length(x) > 0} and
#         \samp{numeric(0)} otherwise.
# Quite useful to prevent starting
# a loop of length nought
#DETAILS
#KEYWORDS iteration helpful
#ALIAS
#INPUTS
#{x}    <<vector>>
#[INPUTS]
#VALUE
# \samp{1:length(x)} if \samp{length(x) > 0}
# else \samp{numeric(0)}.
#EXAMPLE
# sssbf(0);
# sssbf(5);
# sssbf(character(0));
# sssbf(letters);
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
# sssbf for Boucle For the elements of an object
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_09_05
#REVISED 08_09_05
#--------------------------------------------
{
if (length(x) > 0) { return(1:length(x));}
numeric(0);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
ssserreur <- function(x,...,w=FALSE,monitor=sssrbsa0$monitor$v)
#TITLE  issues an error message and concludes accordingly
#DESCRIPTION
# when called this function prints x, then displays a message before stopping 
# the process except if it is a warning or if the general constant
# \samp{monitor$fat$v} is true.
#DETAILS
#KEYWORDS error
#INPUTS
#{x} <<object to be printed before the message. When \samp{sssvoid9(x)}
#      nothing is printed. When it is a list, all components
#      of the list are successively printed.>>
#{\dots}<<pieces of message to display after pasting>>
#[INPUTS]
#{w} << Indicates if it is a simple warning>> 
#{monitor} << List of monitoring constants, see \samp{sssrbsa0$monitor} to
#             know its structure.>>
#VALUE
# nothing is returned
#EXAMPLE
# ssserreur(matrix(1:4,2),"This matrix is not symmetric",w=TRUE)
# ssserreur("Are you sure of it?",w=TRUE);
#
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_03
#REVISED 13_10_01
#--------------------------------------------
{
 cat(paste(rep("~",60),collapse=""),"\n");
if (!sssvoid9(x)) {
    if (is.list(x)) {
        for (i in 1:length(x)) {
             cat(paste(rep("~",40),collapse=""),"\n");
            cat("<< Displaying ",deparse(substitute(x[[i]]))," >>\n")
            print(x[[i]]);
        }
    } else {
        cat("<< Displaying ",deparse(substitute(x))," >>\n")
        print(x);
    }
}
mensaje <- paste(...);
cat("<<<<< MESSAGE >>>>>\n");
print(mensaje);
if (w) {
    cat("/rbsa/","SIMPLE WARNING:\n");
} else {
    on.exit(traceback());
    cat("/rbsa/","ERREUR FATALE\n");
    cat(paste0(rep("~",60),collapse=""),"\n");
    if (monitor$fat$v) { stop("stopped by rbsa");}
}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
