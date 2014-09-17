
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
text2list <- function(text,
                      tags=rbsa0$tag1$v,
                      sep=rbsa0$sep0$v,
                      rsep="no_action",
                      stag=c("/","/"),
                      comment="#",
                      monitor=rbsa0$monitor$v
                     )
#TITLE  transforms a character into a list (of lists) of characters, and conversely
#ALIAS
#DESCRIPTION
# from a conveniently character vector comprising tags, returns nested lists.
# Most often such a character is obtained by reading a file with
# the function \samp{file2text}, this explains why it is convenient
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
# when starting a new list, a character vma (see \samp{text2vma}
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
#           [sub]list. These two correspond to \samp{stag[c(1,3)]} of \samp{list2file} function.>>
#{comment} << Set a characters indicating that the line is a comment line when found
#             in the first positions.>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{rbsa0$monitor$v} provided object as an example.>>
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
# text2list(text);
# list2text(text2list(text));
#REFERENCE
#SEE ALSO text2file file2list
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
    object9(text,"character",-1,mensaje="text must be a character");
    #
    object9(tags,"character",c(2,Inf),mensaje="tags must be a CHARACTER matrix with two columns");
    if (!is.matrix(tags)) {erreur(tags,"tags must be a character MATRIX of two columns");}
    if (ncol(tags)!=2) {erreur(tags,"tags must be a character matrix OF TWO COLUMNS");}
    if (length(unique(tags[,1]))!= nrow(tags)) { erreur(tags,"Opening tags are not unique!");}
    #
    object9(sep,"character",1,mensaje="sep must indicate one string character");
    if (nchar(sep) == 0) { erreur(sep,"sep must not be an empty string");}
    #
    object9(rsep,"character",1,mensaje="rsep must be a character(1)");
    #
    object9(stag,"character",2,mensaje="stag must be a character of length two");
    if (any(nchar(stag) == 0)) { erreur(stag,"stag must not have zero length components");}
    if (length(grep(" ",stag)) > 0) { erreur(stag,"stag must not comprise spaces");}
    #
    object9(comment,"character",1,mensaje="'comment' must be a character of length one");
}
#
# removing the comment lines
if (!void9(comment)) {
  text <- filter8text(text,comment,remove=TRUE,
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
#          and \samp{rbsa0$vma$v["c","v","V","w","m",...]} one of the type of vma character
#          is expected. 
nbtag <- 0;
for (i in bf(text)) {
    lev <- 0;
    # looking for a tag
    for (j in bc(nrow(tags))) { if (lev == 0) {
        pot <- form3crop(text[i],tags[j,1],"");
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
                typing <- rbsa0$vma$v["v"];
                text[i] <- put[2];
            } else {
                # getting the possible explicit separator
                if (substr(put[2],1,nchar(stag[1]))==stag[1]) {
                    # looking for the separator
                    pit <- form3crop(put[2],stag[1]);
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
                tta <- which(pyt==rbsa0$vma$v);
                if (length(tta)==0) {
                    # default type
                    typing <- rbsa0$vma$v["v"];
                } else {
                    # explicite type
                    typing <- names(rbsa0$vma$v)[tta];
                }
            }
        tagged <- rbind(tagged,c(i,lev,put[1],"???",ssep,typing));
        } # end of nchar(put[1]) < nchar(pot)
    } # end of lev > 0
} # end of i in bf(text)
#
#
# in case of no list
if (nrow(tagged) == 0) { return(vector("list",0));}
# detecting the final lists
tagged[,"leaf"] <- "yes";
for (ii in bc(nrow(tagged)-1)) {
    lev1 <- as.numeric(tagged[  ii,"lev"]);
    lev2 <- as.numeric(tagged[ii+1,"lev"]);
    if (lev2-lev1==1) { tagged[ii,"leaf"] <- "no";}
}
#
#   matrix 'tagged' is now available
#
# checking the sequential progress of the levels
for (ii in bc(nrow(tagged)-1)) {
    lev1 <- as.numeric(tagged[  ii,"lev"]);
    lev2 <- as.numeric(tagged[ii+1,"lev"]);
    if ((lev2-lev1) > 1) {
        erreur(tagged,"The progression of levels is not accepted: all levels must be introduced");
    }
}
# checking that no leaf tags are followed by another tag
for (ii in bc(nrow(tagged)-1)) { if (tagged[ii,"leaf"]=="no") {
    actuel <- as.numeric(tagged[  ii,"num"]);
    suivan <- as.numeric(tagged[ii+1,"num"]);
    if ((suivan-actuel) != 1) {
        erreur(list(tagged,ii),"In line 'ii', a 'no-leaf' tags is not immediately followed by another tag (only terminal levels can have contents)");
    }
}}
#
if (tagged[1,"lev"] != "1") {
    erreur(tagged,"The first level must be 1");
}
# replacing the introduced NA
for (ii in bf(text)) { if (is.na(text[ii])) { text[ii] <- "";}}
#
#
# constructing the character strings associated to each leaf list
#
#
oul1 <- which(tagged[,"leaf"]=="yes");
oul2 <- as.numeric(tagged[oul1,"num"]);
lulu <- character(length(oul1));
oul2 <- c(oul2,length(text)+1);
for (ss in bf(lulu)) {
    leaf <- oul1[ss];
    ssep <- tagged[leaf,"sep"];
    for (sss in oul2[ss]:(oul2[ss+1]-1)) {
        nou <- form3crop(text[sss]," ");
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
for (ll in bc(nrow(tagged))) {
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
        chacha <- text2vma(choc,tagged[ll,"type"],rbsa0$sep1$v,monitor=monitor);
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
list2text <- function(lili,
                      tags=rbsa0$tag1$v,
                      stag=c("/",";","/"),
                      comment="#",
                      comments=character(0),
                       monitor=rbsa0$monitor$v
                     )
#TITLE  transforms a list into a character
#DESCRIPTION
# The reverse operation of \samp{text2list}. The list must be a
# rsbsa-list, that is complying
# some properties : all components of the [sub-]lists must be either
# a list or a [named] character vector/matrix/array. The number of nested list
# must not be greater than the number of rows in matrix \samp{tags}.
# Every list component must be named.\cr
# The idea is to get a character compatible with \samp{text2list} to produce back
# the object \samp{lili}.\cr
# Some comments are added to the content of the list by the function itself and/or
# according to the whish of the user, this is why an escaping character
# for comments is asked.
#DETAILS
# The character strings of the structure must not comprise
# the \samp{rbsa0$sep0$v} constant but this global constant can be
# conveniently modified. The same for \samp{rbsa0$sep1$v}.\cr
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
#            see the \samp{rbsa0$monitor$v} provided object as an example.>>
#VALUE
# The resulting character.
#EXAMPLE
# list2text(rbsa0$lis1$v);
# text2list(list2text(rbsa0$lis1$v));
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
    object9(lili,"list",-1,mensaje="lili must be a list");
    #
    object9(tags,"character",c(2,Inf),mensaje="tags must be a CHARACTER matrix with two columns");
    if (!is.matrix(tags)) {erreur(tags,"tags must be a character MATRIX of two columns");}
    if (ncol(tags)!=2) {erreur(tags,"tags must be a character matrix OF TWO COLUMNS");}
    if (length(unique(tags[,1]))!= nrow(tags)) { erreur(tags,"Opening tags are not unique!");}
    #
    object9(stag,"character",3,mensaje="stag must be a character of length three");
    if (any(nchar(stag) == 0)) { erreur(stag,"stag must not have zero length components");}
    if (length(grep(" ",stag[c(1,3)])) > 0) { erreur(stag,"stag must not comprise spaces in first and/or third position");}
    #
    object9(comment,"character",1,mensaje="'comment' must be a character of length one");
    #
    object9(comments,"character",-1,mensaje="'comment' must be a character of any length");
}
# 
# preparing
res <- character(0);
#
# issuing the starting comments
res <- c(res,comment);
res <- c(res,paste(comment,"  This character was created on",now("d"),"by list2file of rbsa package"));
res <- c(res,paste(comment,"  from the object :",deparse(substitute(x))));
res <- c(res,comment);
res <- c(res,comment);
for (ii in bf(comments)) {
    res <- c(res,paste(comment,comment,rbsa0$sep0$v,rbsa0$sep0$v,comments[ii]));
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
    liliex <- explore8list(lili);
    # checking the list-vma nature 
    if(!all(liliex[,"classes"] %in% c("list",
                                      "logical","integer","numeric","character",
                                      "matrix","array"))) {
        erreur(liliex,"The 'lili' list is not composed of list/vector/matrix/array");
    }
    # checking the existence of names for all levels
    if (any(is.na(liliex[,"name"]))) {
        erreur(liliex,"Not all 'lili' components have got a name");
    }
    # checking the levels
    xlev <- max(as.numeric(liliex[,"depth"]));
    if (xlev > nrow(tags)) {
        erreur(list(liliex,tags),"'lili' has got too many levels for the proposed 'tags'");
    }
    #
    # ordering the table
    uv <- order(liliex[,"numbers"]);
    # writing down the file
    for (ii in uv) {
        niv <- as.numeric(liliex[ii,"depth"]);
        if (niv == 1) { res <- c(res,paste0(comment,form3repeat("=",20)));}
        if (niv == 2) { res <- c(res,comment);}
        rrr <- paste0(tags[niv,1],liliex[ii,"name"],tags[niv,2]);
        if (liliex[ii,"classes"] != "list") {
            coco <- get8comp7list(lili,liliex[ii,],monitor=monitor)[[1]];
            caca <- vma2text(coco,rbsa0$sep1$v);
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
