
###########################################
###########################################
########
#((((((( NEW S4 CLASS des
########
###########################################
###########################################

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8des <- function(object)
#TITLE  checks a /des/
#DESCRIPTION
#   This function checks a /des/ objects
#DETAILS
# It is the validity method for /des/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The des object to be validated.>>
#[INPUTS]
#VALUE
# TRUE when the object seems acceptable
# else a character describing the error(s)
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_08_31
#REVISED 10_06_23
#--------------------------------------------
{
    if (class(object)!="des") {
        erreur(NULL,paste("This object is not of class 'des' but '",class(object),"'",sep=""));
    }
    res <- character(0);
    if(length(object@name)!=1) {res <- c(res,"des@name must be a character of length one");}
    if(length(object@orig)>=2) {res <- c(res,"des@orig must not be a vector");}
    if(length(object@time)>=2) {res <- c(res,"des@time must not be a vector");}
    if(length(object@defi)>=2) {res <- c(res,"des@defi must not be a vector");}
    if(length(object@role)>=2) {res <- c(res,"des@role must not be a vector");}
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=rbsb.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


###########################################
# description
setClass("des", representation(
    name="character",    # name of the object being described
    orig="character",    # scalar giving the origin
    time="character",    # scalar giving the time of creation/modification
    defi="character",    # scalar giving the definition
    role="character",    # scalar giving the role of the structure
    comm="character"     # free vector, a component = a paragraph
                         ),
                prototype=list(
    name="rbsb",
    orig=paste("Created by rbsb"),
    time="unknown",
    defi="undefined",
    role=character(0),
    comm=character(0)),
                validity=valid8des
        );
#

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8des <- function(x,...,what="ndr",empha=1)
#TITLE  prints a des object
#DESCRIPTION
#   This function prints in a /des/ object.
#DETAILS
# Global constant \code{rbsb.mwi} is used to justify the paragraphes.
# It is the specific print for /des/ objects.
#KEYWORDS classes
#INPUTS
#{x} <<The \code{des} object to print.>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.>>
#{what} <<the fields to print:
#          \code{a}=all fields,
#          \code{n}=name,
#          \code{d}=definition,
#          \code{o}=origin,
#          \code{t}=time,
#          \code{r}=role,
#          \code{c}=comments.>>
#{empha} <<Emphasize level of printing;
#           between 0 and 10>>
#VALUE
# nothing but a print is performed
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
# Add a beginnnig way (giving only the first characters of
# each slot for not too large reports.
#AUTHOR J.-B. Denis
#CREATED 08_08_21
#REVISED 10_09_15
#--------------------------------------------
{
# some checking
che <- valid8des(x);
if (!identical(che,TRUE)) {
    erreur(che,"/des/ is not valid");
}
empha <- round(max(0,min(10,empha)));
nemph <- empha -2;
what <- tolower(what);
if (expr3present("a",what)) { what <- "ndortc";}
# preparing the titles
tt <- c("Name","Definition","Origin","Time","Role","Comment(s)");
# preparing the constant according to the emphasis
if (empha == 0) {
    sep=0; ed <- form3repeat(" ",2*nemph);
    nbs <- 5; sou <- " ";
}
if (empha == 1) {
    sep=0; ed <- form3repeat(" ",2*nemph);
    nbs <- 5; sou <- " ";
}
if (empha == 2) {
    sep=0; ed <- form3repeat(" ",2*nemph);
    nbs <- 5; sou <- " ";
}
if (empha == 3) {
    sep=0; ed <- form3repeat(" ",2*nemph);
    nbs <- 5; sou <- "-";
}
if (empha == 4) {
    sep=1; ed <- form3repeat(" ",2*nemph);
    nbs <- 5; sou <- "==";
}
if (empha == 5) {
    sep=1; ed <- form3repeat(" ",2*nemph);
    nbs <- 20; sou <- "==";
}
if (empha == 6) {
    sep=2; ed <- form3repeat(" ",2*nemph);
    nbs <- 2; sou <- form3repeat(" ",50,FALSE,TRUE);
}
if (empha >= 7) {
    sep=2; ed <- form3repeat(" ",2*nemph);
    nbs <- 3; sou <- form3repeat("=",50,FALSE,TRUE);
}
#
# printing
if (expr3present("n",what)) {
if (!isempty(x@name)) {
if (x@name != "") {
    form3titre(c(tt[1],x@name),nemph);
}}}
if (expr3present("d",what)) {
if (!isempty(x@defi)) {
if (x@defi != "") {
    if (!isempty(x@name)) {
        tt[2] <- paste("<",tt[2]," of '",x@name,"':>",sep="");
    }
    form3paragraphe(c(tt[2],x@defi),nemph,
                    wid=rbsb.mwi,sep=sep,ed=ed);
}}}
if (expr3present("o",what)) {
if (!isempty(x@orig)) {
if (x@orig != "") {
    form3paragraphe(c(tt[3],x@orig),nemph,
                    wid=rbsb.mwi,sep=sep,ed=ed);
}}}
if (expr3present("t",what)) {
if (!isempty(x@time)) {
if (x@time != "") {
    form3paragraphe(c(tt[4],x@time),nemph,
                    wid=rbsb.mwi,sep=sep,ed=ed);
}}}
if (expr3present("r",what)) {
if (!isempty(x@role)) {
if (x@role != "") {
    form3paragraphe(c(tt[5],x@role),nemph,
                    wid=rbsb.mwi,sep=sep,ed=ed);
}}}
if (expr3present("c",what)) {
if (!isempty(x@comm)) {
if ((x@comm[1] != "")|(length(x@comm)>1)) {
    form3paragraphe(c(tt[6],x@comm),max(0,nemph),
                    wid=rbsb.mwi,sep=sep,ed=ed);
}}}
form3repeat(sou,nbs,TRUE);
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#
#

setMethod("print",signature(x = "des"), print8des);




###########################################
###########################################
########
#((((((( NEW S4 CLASS faux
########
###########################################
###########################################


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8faux <- function(object)
#TITLE  checks a /faux/
#DESCRIPTION
#   This function checks a /faux/ objects
#DETAILS
# It is the validity method for /faux/ objects.
#KEYWORDS error
#INPUTS
#{object} <<The faux object to validate.>>
#[INPUTS]
#VALUE
# TRUE when the object seems acceptable
# else a character describing the error(s)
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_08_31
#REVISED 10_06_23
#--------------------------------------------
{
    if (class(object)!="faux") {
        erreur(NULL,paste("This object is not of class 'faux' but '",class(object),"'",sep=""));
    }
    if ((object@level != 0) & (object@level != 1) & (object@level != 2)) {
        re1 <- "faux@level must be 0, 1 or 2";
    } else { re1 <- TRUE;}
    re2 <- valid8des(as(object,"des"));
    #
    res <- character(0);
    if (!identical(re1,TRUE)) { res <- c(res,re1);}
    if (!identical(re2,TRUE)) { res <- c(res,re2);}
    if (identical(res,character(0))) { res <- TRUE;}
    #
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

################################################################################
setClass("faux",
         representation(level="numeric"), # level of the error
         prototype(level=0),
         contains="des",                 # extension of a description
         validity=valid8faux
        );
################################################################################


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8faux <- function(x,...,what=2)
#TITLE  prints a faux object
#DESCRIPTION
#   This function prints in a interpreted way a /faux/ object.
#DETAILS
# Global constant \code{rbsb.mwi} is used to justify the paragraphes.
# It is the specific print for /faux/ objects.
#KEYWORDS error
#INPUTS
#{x} <<The faux object to be printed.>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.>>
#{what} <<the level of printing
#          0= only the definition,
#          1= the origin and the definition
#          2= everything.
#VALUE
# nothing but a print is performed
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# print(rbsb.fau1,what=2);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_01_21
#REVISED 09_09_29
#--------------------------------------------
{
#
# printing
#
what <- "ndo";
if (what==0) { what <- "d";}
if (what==2) { what <- "a";}
#
form3titre(paste("/faux/ with a level of",x@level));
print(as(x,"des"),what=what);
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "faux"),print8faux);


###########################################
###########################################
########
#((((((( NEW S4 CLASS daf
########
###########################################
###########################################

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8daf <- function(object)
#TITLE  checks a /daf/
#DESCRIPTION
#   This function checks /daf/ objects
#DETAILS
# It is the validity method for /daf/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The daf object to be validated.>>
#[INPUTS]
#VALUE
# TRUE when the object seems acceptable
# else a character describing the error(s)
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_10_01
#REVISED 10_06_23
#--------------------------------------------
{
    if (class(object)!="daf") {
        erreur(NULL,paste("This object is not of class 'daf' but '",class(object),"'",sep=""));
    }
    res <- character(0);
    # checking the 'what' parameter
    if (!(object@what %in% c("t","d","f","c","c2"))) {
        res <- c(res,paste(object@what,"'what' must be 't', 'd' or 'f'"));
    }
    # checking the other fields
    # to avoid error whe R is checking the package
  if (!is.null(rbsb.snp)) {  # to prevent R checking harassment
    rres <- NULL;   # to prevent R checking harassment
    if ((object@what=="t") |
        (object@what=="c") |
        (object@what=="c2")) {
        if (file.access(object@valu)<0) {
            res <- c(res,paste(object@valu,"No file of this name seems accessible."));
        }
    }
    if (object@what=="d") {
        if(!exists(object@valu)) {
            res <- c(res,paste(object@valu,"No data.frame under this name: should be a data.frame"));
        }
        coda <- paste("rres <- class(",object@valu,")");
        eval(parse(text=coda));
        if (rres!="data.frame") {
            res <- c(res,paste(list(object@valu,res),"A data.frame was expected under this variable"));
        }
    }
    if (object@what=="f") {
        if(!exists(object@valu)) {
            res <- c(res,paste(object@valu,"No object under this name: should be a function"));
        }
        coda <- paste("rres <- class(",object@valu,")");
        eval(parse(text=coda));
        if (rres!="function") {
            res <- c(res,paste(list(object@valu,res),"A function was expected under this variable"));
        }
    }
  } # to prevent R checking harassment
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=rbsb.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




###########################################
setClass("daf", representation(
         des="des", # description of the data frame
         what="character", # indication about the way the data.frame is
                           # available. 3 currently ways:
                           #  't' through a text file to be read
                           #  'd' through a data.frame itself
                           #  'f' through a function to be called without argument
         valu="character"  # the complete path of the text file when what=='t'
                           # the data.frame name when what=='d'
                           # the function name when what=='f'
                            ),
               prototype(des=new("des",name="rbsb",
                                 orig=paste("Created by rebastaba"),
                                 time=date(),
                                 defi="prototype",
                                 role=character(0),
                                 comm=character(0)),
                         what="d",
                         valu="rbsb.dfr0"),
               validity=valid8daf
        );


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8daf <- function(x,what="nr",whi=1:10)
#TITLE  prints a daf object
#DESCRIPTION
#   This function prints a /daf/ object.
#DETAILS
#  It is the specific print function for /daf/ objects.
#KEYWORDS print
#PKEYWORDS daf
#INPUTS
#{x} <<The daf object.>>
#[INPUTS]
#{what} << which part of the description to print
#                  (see \code{print8des}).>>
#{whi} << which rows to print?\cr
#          when -1: none is printed,\cr
#          when 0: all are printed,\cr
#          when a vector: indicates the numbers of rows to print
#                         (default = the first ten),\cr
#          when a value: the percentage to be printed
#                         (50 = half of the rows)
#       >>
#VALUE
#  returns nothing but a printing is performed
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# print(rbsb.daf2);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_04_23
#REVISED 10_02_11
#--------------------------------------------
{
# some checks
if (rbsb.mck) {
    che <- valid8daf(x);
    if (!identical(che,TRUE)) {
        erreur(che,"/daf/ is not valid");
    }
    check4tyle(what,rbsb.chara,1,message="'what' must be character(1)");
}
#
nsou <- 39;
# dealing with the daf.null case
if (identical(x,rbsb.daf0)) {
    form3repeat("-",nsou,TRUE);
    cat("<<< daf.null >>>\n");
    form3repeat("-",nsou,TRUE);
    return(invisible());
}
# loading the data.frame
values <- get8daf(x);
# looking for the rows to print
if (min(whi) < 0) {
    quelles <- numeric(0);
} else {
    nlig <- nrow(values);
    if ((identical(whi,0))|(nlig==0)) {
        quelles <- bc(nlig);
    } else {
        if (length(whi) == 1) {
            nli <- min(100,whi);
            quelles <- seq(1,nlig,length=max(1,round(nlig*nli/100)));
            quelles <- round(quelles);
        } else {
            quelles <- intersect(whi,bc(nlig));
            if (length(quelles) <= 0) { quelles <- 1:min(10,nlig);}
        }
    }
}
# printing the description
form3repeat("-",nsou,TRUE);
print(x@des,what);
#printing the values
form3repeat("-",nsou,TRUE);
if (length(quelles)>0) {
    print(values[quelles,]);
}
# returning
form3repeat("-",nsou,TRUE);
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "daf"), print8daf);

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
daf2list <- function(daf,simple=TRUE)
#TITLE  transforms a /daf/ into a list
#DESCRIPTION
#   This function transforms a /daf/ object
# into a standard list. The shortened way is used
# when \code{simple}.
#DETAILS
# By simple or shortened, it is meant that the list
# is given with a simple level of a \code{character(3)}
# (most frequent use).
#KEYWORDS misc
#PKEYWORDS daf
#INPUTS
#{daf} <<The daf object to be transformed.>>
#[INPUTS]
#{simple} <<indicates the way the list must be created.>>
#VALUE
#  the resulting list
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# daf2list(rbsb.daf1);
# daf2list(NULL);
# daf2list(rbsb.daf1,FALSE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_06_21
#REVISED 10_06_28
#--------------------------------------------
{
# degenerate case
if (isempty(daf)) { return(rbsb.lis0);}
# some checks
if (rbsb.mck) {
    che <- valid8daf(daf);
    if (!identical(che,TRUE)) {
        erreur(che,"/daf/ is not valid");
    }
}
#
if (simple) {
    res <- list(daf=c(daf@des@name,daf@what,daf@valu));
} else {
    res <- des2list(daf@des);
    res$what <- daf@what;
    res$valu <- daf@valu;
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
list2daf <- function(li)
#TITLE  transforms a list into a /daf/
#DESCRIPTION
#   This function transforms a list comprising
# the necessary components into a /daf/ object.
# The components of the list must be either the
# direct transcription of the slot hierarchy of /daf/
# or (shortened way) a component named \code{daf} comprising a
# \code{character(3)} with \code{des@name}, \code{what} and \code{valu} slots.
#DETAILS
# The list \code{li} can comprise more components
# which are ignored. If \code{li$daf} exists, it will
# have priority over the possible slot components.
#KEYWORDS misc
#PKEYWORDS daf
#INPUTS
#{li} <<The list to be transformed into a /daf/.>>
#[INPUTS]
#VALUE
#  returns the resulting /daf/ object.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# lu <- daf2list(new("daf",des=new("des",name="rbsb",
#                                 orig=paste("Created by rebastaba"),
#                                 time=date(),
#                                 defi="prototype",
#                                 role=character(0),
#                                 comm=character(0)),
#                         what="d",
#                         valu="rbsb.dfr0"));
# list2daf(lu);
# list2daf(list(daf=c("simple","d","rbsb.dfr0")));
# list2daf(rbsb.lis0);
#REFERENCE
#SEE ALSO daf2list
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_06_22
#REVISED 10_06_28
#--------------------------------------------
{
# degenerate case
if (isempty(li)) { return(rbsb.daf0);}
if (isempty(li$daf)) {
    # slot case
    if (rbsb.mck) {
        didi <- setdiff(slotNames("daf"),names(li));
	if (length(didi) > 1) {
	    erreur(names(li),"The components of this list does not allow to create a /daf/ through the slot case");
	}
    }
    #
    res <- new("daf",des=list2des(li$des),
		     what=li$what,
		     valu=li$valu);
} else {
    # simple case
    if (rbsb.mck) {
        check4tyle(li$daf,rbsb.chara,3,message="A character(3) was expected: name, what, valu");
    }
    res <- new("daf",des=char2des(li$daf[1]),
                     what=li$daf[2],
                     valu=li$daf[3]);
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
list2des <- function(li)
#TITLE  transforms a consistent list into a /des/ object
#DESCRIPTION
# Just analyzing the components of the list
# (consistent names have to be used) which are supposed
# to be character and tackle them to produce consistent
# slots of a /des/ object.
# The components of the list must be either the
# direct transcription of the slots of /des/
# or a component named \code{des} comprising a
# \code{character(1:Inf)} with des@name, des@orig, des@time,
# des@defi,des@role,des@comm in this order.
#DETAILS
#PKEYWORDS des
#KEYWORDS classes
#INPUTS
#{li} <<The list to be transformed into a des object.>>
#[INPUTS]
#VALUE
# The generated 'des' object
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# print(list2des(list(name="A",role="Just to see",comm="Not very interesting")));
# print(list2des(list(des=c("A","","","","Just to see","Not very interesting"))));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_01_06
#REVISED 10_06_28
#--------------------------------------------
{
if (isempty(li)) { return(rbsb.des0);}
#
if (isempty(li$des)) {
    # slot case
    if (rbsb.mck) {
	if (isempty(li$name)) {
	    erreur(names(li),"The components of this list does not allow to create a /des/ through the slot case: at least $name is compulsory");
	}
    }
    #
    if (is.null(li$orig)) { li$orig <- rbsb.cha0;}
    if (is.null(li$time)) { li$time <- rbsb.cha0;}
    if (is.null(li$defi)) { li$defi <- rbsb.cha0;}
    if (is.null(li$role)) { li$role <- rbsb.cha0;}
    if (is.null(li$comm)) { li$comm <- rbsb.cha0;}
    if (is.null(li$name)) {
        erreur(li,"the component 'name' is compulsory");
    }
    #
    res <- new("des",name=li$name,orig=li$orig,time=li$time,
                     defi=li$defi,role=li$role,comm=li$comm);
} else {
    # simple case
    if (rbsb.mck) {
        check4tyle(li$des,rbsb.chara,c(1,Inf),message="A character(1:Inf) was expected: name,... orig, time, defi, role, comm.");
    }
    name <- li$des[1];
    nbl <- length(li$des);
    if (nbl > 1) { orig <- li$des[2];} else { orig <- rbsb.cha0;}
    if (nbl > 2) { time <- li$des[3];} else { time <- rbsb.cha0;}
    if (nbl > 3) { defi <- li$des[4];} else { defi <- rbsb.cha0;}
    if (nbl > 4) { role <- li$des[5];} else { role <- rbsb.cha0;}
    if (nbl > 5) { comm <- li$des[-(1:5)];} else { comm <- rbsb.cha0;}
    res <- new("des",name=name,time=time,defi=defi,
                     role=role,comm=comm);
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
des2list <- function(des,simple=FALSE)
#TITLE  transforms a /des/ object into a list.
#DESCRIPTION
# Transforms a /des/ object into a list.  
# More or less the reverse of \code{list2des}.
# When \code{simple} the shortened way is used.
#DETAILS
# Simple way means that the description is given
# as a character vector, not as a list. See the
# proposed examples.
#PKEYWORDS des
#KEYWORDS classes
#INPUTS
#{des} <<The /des/ to be transformed into a list.>>
#[INPUTS]
#{simple} <<indicates the way the list must be created.>>
#VALUE
# The generated list
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# des2list(rbsb.des1);
# des2list(rbsb.des1,TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_03_08
#REVISED 10_06_28
#--------------------------------------------
{
# checking
if (!valid8des(des)) {
    str(des);
    erreur(NULL,"This /des/ is not valid!");
}
# filling
if (simple) {
    res <- list(des=des@name);
    if (!isempty(des@orig)) {
        res$des <- c(res$des,des@orig);
        if (!isempty(des@time)) {
            res$des <- c(res$des,des@time);
	    if (!isempty(des@defi)) {
		res$des <- c(res$des,des@defi);
	        if (!isempty(des@role)) {
		    res$des <- c(res$des,des@role);
	            if (!isempty(des@comm)) {
		        res$des <- c(res$des,des@comm);
                    }
                }
	    }
        }
    }
} else {
    res <- vector("list",0);
    if (!isempty(des@name)) { res$name <- des@name;}
    if (!isempty(des@orig)) { res$orig <- des@orig;}
    if (!isempty(des@time)) { res$time <- des@time;}
    if (!isempty(des@defi)) { res$defi <- des@defi;}
    if (!isempty(des@role)) { res$role <- des@role;}
    if (!isempty(des@comm)) { res$comm <- des@comm;}
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
char2des <- function(x)
#TITLE  transforms, if necessary, a single character into a /des/ object
#DESCRIPTION
# when x is not a \code{character(1)} or a /des/ an error is issued
#DETAILS
#PKEYWORDS des
#KEYWORDS utilities
#INPUTS
#{x} <<name for the /des/ to be created, or an already existing /des/.>>
#[INPUTS]
#VALUE
# a /des/ object (not modfyied when x was already a /des/.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# print(char2des("toto"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_03_03
#REVISED 09_09_29
#--------------------------------------------
{
# checking
if ((!is.character(x)) &
    (length(x)!=1)     &
    (!is(x,"des"))) {
    erreur(x,"Must be 'character(1)' or a 'des' object!");
}
# returning
if (is(x,"des")) { res <- x;
} else {
    res <- new("des",name=x,
               orig=paste("created by",rbsb.msi),
               time=now("d"));
}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
get8daf <- function(daf,wh=0,strict=FALSE,check=TRUE)
#TITLE  returns the data frame associated to a /daf/
#DESCRIPTION
# Returns the data frame associated to a /daf/. 
# The row numbers can be choosen with \code{wh}.
#DETAILS
# According to \code{daf@what} a different action is performed to 
# get the data frame:
#{f} << The named \code{daf@valu} function is used without argument.>>
#{d} << The named \code{daf@valu} data frame is used.>>
#{t} << The named \code{daf@valu} file is read with read.table.>>
#{c} << The named \code{daf@valu} file is read with read.csv.>>
#{c2} << The named \code{daf@valu} file is read with read.csv2.>>
# For \code{t}, \code{c} or \code{c2} cases, \code{header=TRUE}
# and \code{comment.char='#'}.
#KEYWORDS datasets
#INPUTS
#{daf} <<The /daf/ to be read.>>
#[INPUTS]
#{wh} << numeric giving the numbers of the observations
#           to be read. Zero means all observations.>>
#{strict} << When TRUE a fatal error is issued
#           if some asked observations does not exist. If
#           not the present ones are returned.>>
#{check} << Must the /daf/ object be checked?>>
#VALUE
# a data frame
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_05_11
#REVISED 10_03_08
#--------------------------------------------
{
# checking
if (check) {if (rbsb.mck) {valid8daf(daf);}}
if (rbsb.mck) {
    check4tyle(wh,"integer",-1,message="These are supposed to be the asked row numbers");
    if (min(wh) < 0) { erreur(wh,"Negative row number were asked.");}
}
# getting the values
if (daf@what == "t") {
    res <- read.table(daf@valu,header=TRUE,comment.char="#");
}
#
if (daf@what == "c") {
    res <- read.csv(daf@valu,header=TRUE,comment.char="#");
}
#
if (daf@what == "c2") {
    res <- read.csv2(daf@valu,header=TRUE,comment.char="#");
}
#
if (daf@what == "d") {
    coda <- paste("res <-",daf@valu);
    eval(parse(text=coda));
}
#
if (daf@what == "f") {
    coda <- paste("res <-" ,daf@valu,"()");
    eval(parse(text=coda));
}
# possible observations
opo <- bc(nrow(res));
# defining and checking the asked observations
if (identical(wh,0)) { wh <- opo;}
prendre <- intersect(wh,opo);
if (strict) {
    if (length(prendre) < length(wh)) {
        erreur(list(opo,wh),"Some asked observations were not found");
    }
}
# selecting the values
res <- res[prendre,,drop=FALSE];
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
char2vma <- function(cha,what=rbsb.vma["v"],
                     xsep=rbsb.sep1,nat="C")
#TITLE  transforms a character into a vector (or matrix, or array)
#DESCRIPTION
# from a \code{character} vector, returns a vector, or a matrix, or
# an array of characters with possibly names, or dimames. The information
# can be supplied in different ways for each of the three possibilities.
# It is advised to try the proposed examples.
#DETAILS
# The processing is done in character mode but the result can be
# transformed into numerical or logical values with the help of argument \code{nat}.
# \cr
# In fact \code{rbsb.vma} coding is used for the argument \code{what}. 
# This allows to easily modify the coding.
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{cha} << The character to transform.>>
#[INPUTS]
#{what} << Indicates which structure to return: either
# a vector, a matrix or an array.
# \cr \cr
#   For vectors, the possibilities are c/C/u/v/U/V :
#    \cr\code{rbsb.vma["c"]} for a no named character(1); collapsing
#             is done with \code{rbsb.sep0}.
#    \cr\code{rbsb.vma["C"]} for a no named character() of any length
#             (components are separated with \code{xsep} whiwh are
#              removed from the result); collapsing
#             is done with \code{rbsb.sep0}.
#    \cr\code{rbsb.vma["v"]} or \code{rbsb.vma["u"]} for a no named vector;
#    \cr\code{rbsb.vma["V"]} for a named vector with
#          all names before the values; then an even number
#          of components must be provided.
#    \cr\code{rbsb.vma["U"]} for a named vector with
#          names interlaced with the value (name_i, value_i); then an even number
#          of components must be provided.
# \cr \cr
#   For matrices, the possibilities are m/n/o/p/M/N/O/P:
#    \cr\code{rbsb.vma["m"]} for a no named matrix given by row, two adjacent rows
#          being separated with \code{xsep} sequence, introduced as one of the
#          component of \code{cha}, then for a 2x3 matrix, the length of \code{cha}
#          will be 6+2 = 8.
#    \cr\code{rbsb.vma["n"]} for a matrix with only the columns names. The expected sequence is
#          the names of columns, then the values as for \code{rbsb.vma["m"]}; then for a 2x3
#          matrix, the length of \code{cha} will be 3+1+8=12.
#    \cr\code{rbsb.vma["o"]} for a matrix with only rows named. The expected sequence is
#          name of row, values of rows... Then 2x3 will imply a length of 8+2=10.
#    \cr\code{rbsb.vma["p"]} when names for columns and rows, in a mixed way... Then 2x3 will imply
#          a length of 14.
#    \cr
#     When \code{rbsb.vma["M"],rbsb.vma["N"],rbsb.vma["O"],rbsb.vma["P"]},
#          the same but the matrix will be transposed after
#          being read; said in another way, the values are given by columns.
# \cr \cr
#   For arrays, the possibilities are a/A/b/B:
#    \cr\code{rbsb.vma["a"]} for a no named array, the dimensions, \code{xsep}, the values in
#    the classical order (varying the first index the fastest). 2x3 will give
#    a length of 2+1+6=9.
#    \cr\code{rbsb.vma["A"]} for a dimnamed array, the dimensions, \code{xsep}, the dimnames of each
#    dimension in the right order also separated and finished with \code{xsep}. 
#    2x3 will gives a length of 2+1+2+1+3+1+6=16.
#    \cr\code{rbsb.vma["b"]} for a named dimensions array, the dimensions, \code{xsep}, the names for the
#    dimension in the right order not separated and finished with \code{xsep}. 
#    2x3 will gives a length of 2+1+2+1+6=12.
#    \cr\code{rbsb.vma["B"]} for a named and dimnamed array, the dimensions, \code{xsep}, the names for the
#    dimension in the right order not separated and finished with \code{xsep}, then the dimnames separated
#    before the values. 
#    2x3 will gives a length of (2+1)+(2+1)+(2+1+3+1)+(6)=19.
# >>
#{xsep} << Character sequence used to separate the character vector into blocks
#    giving information about the structure (see the examples).>>
#{nat} << Nature of the returned structure. Can be \code{C} for character, \code{N}
#         for numeric or \code{L} for logical.>>
#VALUE a vector or a matrix or an array according to the inputs
#EXAMPLE
# rbsb3k("reset"); # For R checking convenience
## vectors
# char2vma(letters,"c");
# char2vma(letters,"C",xsep="e");
# char2vma(letters);
# char2vma(letters,"V");
# char2vma(letters,"u");
# char2vma(c(LETTERS,letters),rbsb.vma["V"]);
# char2vma(c("A","a","B","b","C","c"),rbsb.vma["U"]);
## matrices
# char2vma(c(1:3,"//",4:6),rbsb.vma["m"]);
# char2vma(c(1:3,"//",4:6),rbsb.vma["M"]);
# char2vma(c(LETTERS[1:3],"//",1:3,"//",4:6),rbsb.vma["n"]);
# char2vma(c(LETTERS[1:3],"//",1:3,"//",4:6),"N");
# char2vma(c("a",1:3,"//","b",4:6),"o");
# char2vma(c(c(LETTERS[1:3],"//","a",1:3,"//","b",4:6)),rbsb.vma["p"]);
## arrays
# char2vma(c(2:4,"//",1:24),"a");
# char2vma(c(2:4,"//","one","two","//",LETTERS[1:3],"//",
#          letters[1:4],"//",1:24),"A");
# char2vma(c(2:4,"//","one","two","//",LETTERS[1:3],"//",
#          letters[1:4],"//",1:24),"A",nat="N");
#REFERENCE
#SEE ALSO  \code{vma2char}
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_03_28
#REVISED 10_08_13
#--------------------------------------------
{
# flexibility
if (isempty(cha)) { cha <- rbsb.cha0;}
# of use
ssep <- which(cha==xsep);
# checking
if (rbsb.mck) {
    check4tyle(cha,rbsb.chara,-1,message="cha must be a character");
    #
    check4tyle(what,rbsb.chara,1,
               message="what (character(1)) must indicate the type of desired output");
    if (!(what %in% rbsb.vma)) {
        erreur(what,"'what' not in the list of possible values...");
    }
    #
    check4tyle(xsep,rbsb.chara,1,message="must indicate the character string of separation");
    #
    if (what %in% rbsb.vma[c("V","U")]) {
        if ((length(cha) %% 2) != 0) {
            erreur(list(cha,what),"Here the length of 'cha' must be even");
        }
    }
    #
    if (what %in% rbsb.vma[c("n","N","m","M","o","O")]) {
        if (length(ssep) >= 1) {
            nbc <- ssep[1] - 1;
            nbr <- (length(cha)+1) / (nbc+1);
            if ((nbr!=round(nbr)) | (length(cha) != (nbc*nbr+nbr-1))) {
                erreur(list(what,length(cha),nbc,nbr),"Dimensions not consistent");
            }
        }
    }
    #
    if (what %in% rbsb.vma[c("p","P")]) {
        if (length(ssep) >= 1) {
            nbc <- ssep[1] + 1;
            X <- 2 + length(cha);
            if ((X %% nbc) != 0) {
                erreur(list(what,length(cha),nbc),"Dimensions not consistent");
            }
        }
    }
    #
    if (what %in% rbsb.vma[c("a","A","b","B")]) { 
	# an array must be returned
        if (length(ssep)==0) {
            erreur(list(cha,xsep),"For arrays, dims must be provided");
        }
        didi <- cha[1:(ssep[1]-1)];
        didi <- as.numeric(didi);
        if (sum(is.na(didi))>0) {
            erreur(list(cha,xsep),"Dimensions are not numeric !");
        }
        if (any(didi<0)) {
            erreur(didi,"Negative dimensions were provided");
        }
        if (any(didi!=round(didi))) {
            erreur(didi,"Non integer dimensions were provided");
        }
        X <- length(didi) + 1 + prod(didi);
        if (what %in% rbsb.vma[c("A","B")]) { X <- X + sum(didi) + length(didi); }
        if (what %in% rbsb.vma[c("b","B")]) { X <- X + length(didi) + 1; }
	if (length(cha) != X) {
            erreur(list(cha,what,xsep),"Inconsistency for an array");
	}
    }
    #
    check4tyle(nat,rbsb.chara,1,message="'nat' must be a character(1)");
}
#
res <- rbsb.cha0;
#
if (what %in% rbsb.vma["c"]) {
    # a single character must be returned
    # rbsb.sep0 is used as collapsor
    res <- paste(cha,collapse=rbsb.sep0);
}
#
if (what %in% rbsb.vma["C"]) {
    # a character must be returned
    # rbsb.sep0 is used as separator
    res <- paste(cha,collapse=rbsb.sep0);
    res <- strsplit(res,xsep,fixed=TRUE)[[1]];
    if (length(res)>1) { for (ii in 2:length(res)) {
        res[ii  ] <- form3decadre(res[ii  ],rbsb.sep0,"",1);
        res[ii-1] <- form3decadre(res[ii-1],"",rbsb.sep0,1);
    }}
}
#
if (what %in% rbsb.vma[c("v","V","u","U")]) {
    # a vector must be returned
    if (what %in% c(rbsb.vma[c("v","u")])) {
        res <- cha;
    } else {
        nb <- floor(length(cha)/2);
        if (what == rbsb.vma["V"]) { nam <- rep(c(TRUE,FALSE),each=nb);
        } else { nam <- rep(c(TRUE,FALSE),nb); }
    res <- cha[!nam];
    names(res) <- cha[nam];
    }
}
#
if (what %in% rbsb.vma[c("m","M","n","N","o","O","p","P")]) {
    # a matrix must be returned
    nbc <- ssep[1] - 1;
    cha <- c(cha,xsep);
    if (what %in% c(rbsb.vma[c("p","P")])) {
        cha <- c(" ",cha);
    }
    if (what %in% rbsb.vma[c("p","P")]) { add <- 1;
    } else { add <- 0; }
    cha <- matrix(cha,ncol=nbc+1+add,byrow=TRUE);
    cha <- cha[,-ncol(cha),drop=FALSE];
    if (what %in% rbsb.vma[c("m","M")]) {
        res <- cha;
    }
    if (what %in% rbsb.vma[c("n","N")]) {
        res <- cha[-1,,drop=FALSE];
        dimnames(res) <- list(NULL,cha[1,,drop=FALSE]);
    }
    if (what %in% rbsb.vma[c("o","O")]) {
        res <- cha[,-1,drop=FALSE];
        dimnames(res) <- list(cha[,1,drop=FALSE],NULL);
    }
    if (what %in% rbsb.vma[c("p","P")]) {
        res <- cha[-1,-1,drop=FALSE];
        dimnames(res) <- list(cha[-1,1,drop=FALSE],cha[1,-1,drop=FALSE]);
    }
    if (what %in% rbsb.vma[c("M","N","O","P")]) {
        res <- t(res);
    }
}
#
if (what %in% rbsb.vma[c("a","A","b","B")]) {
    if (length(ssep) == 0) { erreur(cha,"For array, dimensions must be provided");}
    # an array must be returned
    didi <- cha[1:(ssep[1]-1)];
    didi <- as.numeric(didi);
    nbdi <- length(didi);
    #
    if (what == rbsb.vma["a"]) { vvv <- cha[-(1:ssep[1])];}
    if (what == rbsb.vma["A"]) { vvv <- cha[-(1:ssep[1+nbdi])];}
    if (what == rbsb.vma["b"]) { vvv <- cha[-(1:ssep[2])];}
    if (what == rbsb.vma["B"]) { vvv <- cha[-(1:ssep[2+nbdi])];}
    #
    res <- array(vvv,dim=didi);
    #
    if (what %in% rbsb.vma[c("A","B")]) {
        ndi <- vector("list",0);
        for (jj in bf(didi)) {
            jjj <- jj + (what == rbsb.vma["B"]);
            if (ssep[jjj+1]-ssep[jjj]>1) {
                ndi[[jj]] <- cha[(ssep[jjj]+1):(ssep[jjj+1]-1)];
            }
        }
        dimnames(res) <- ndi;
    }
    if (what %in% rbsb.vma["b"]) {
        ndi <- vector("list",0);
        for (jj in bf(didi)) {
            ndi[[jj]] <- 1:didi[jj];
        }
        dimnames(res) <- ndi;
    }
    if (what %in% rbsb.vma[c("b","B")]) { 
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
vma2char <- function(x,xsep=rbsb.sep1)
#TITLE  transforms a vector (or matrix, or array) into a character
#DESCRIPTION
# from a vector, or a matrix, or
# an array, returns a \code{character} vector. More or less the 
# inverse function of \code{char2vma}.
#DETAILS
# When some dimnames exist, the possible missing
# ones will be added.
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{x} << The object to transform.>>
#[INPUTS]
#{xsep} << \code{character(1)} to be use for the separations.>>
#VALUE a list with two components: \code{[[1]]} the coded character vector and
# \code{[[2]]} the type according to \code{char2vma}.
#EXAMPLE
# rbsb3k("reset"); # For R checking convenience
## vectors
# vma2char(letters);
# x <- letters; names(x) <- LETTERS;
# xx <- vma2char(x);
# char2vma(xx[[1]],xx[[2]]);
# vma2char(character(0));
## matrices
# x <- matrix(1:20,4);
# vma2char(x);
# dimnames(x) <- list(letters[1:4],LETTERS[1:5]);
# vma2char(x);
# x1 <- matrix(NA,3,0);
# xx1 <- vma2char(x1);
# char2vma(xx1[[1]],xx1[[2]]);
# dimnames(x1) <- list(c("i","ii","iii"),NULL);
# xx1 <- vma2char(x1);
# char2vma(xx1[[1]],xx1[[2]]);
## arrays
# x <- array(1:24,2:4);
# vma2char(x);
# dimnames(x) <- list(1:2,c("i","ii","iii"),c("I","II","III","IV"));
# vma2char(x,xsep="|||");
# x0 <- array(NA,c(3,0,2));
# dimnames(x0) <- list(1:3,NULL,c("i","ii"));
# xx0 <- vma2char(x0);
# char2vma(xx0[[1]],xx0[[2]]);
#REFERENCE
#SEE ALSO  \code{vma2char}
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_03_29
#REVISED 10_06_29
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    #
    check4tyle(xsep,rbsb.chara,1,message="must indicate the character string of separation");
    #
    if (!is.vector(x) &
        !is.matrix(x) &
        !is.array(x)) {
        erreur(class(x),"'x' must be a vector or a matrix or an array!");
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
        res[[2]] <- rbsb.vma["a"];
    } else {
        nna <- dimnames(x);
        for (hh in bf(nna)) {
            if (is.null(nna[[hh]])) {
                nna[[hh]] <- bc(dim(x)[hh]);
            }
        }
        if (is.null(names(nna))) {
            for (ii in bf(nna)) {
                res[[1]] <- c(res[[1]],nna[[ii]],xsep);
            }
        res[[2]] <- rbsb.vma["A"];
        } else {
            res[[1]] <- c(res[[1]],names(nna),xsep);
            for (ii in bf(nna)) {
                res[[1]] <- c(res[[1]],nna[[ii]],xsep);
            }
        res[[2]] <- rbsb.vma["B"];
        }
    }
    res[[1]] <- c(res[[1]],as.character(x));
} else {
    if (is.matrix(x)) {
        # dealing with a matrix
        if (is.null(dimnames(x))) {
            res[[1]] <- character(0);
            for (ii in bc(nrow(x))) {
                res[[1]] <- c(res[[1]],x[ii,],xsep);
            }
            res[[1]] <- res[[1]][-length(res[[1]])];
            res[[2]] <- rbsb.vma["m"];
        } else {
            nna <- dimnames(x);
            for (hh in bf(nna)) {
                if (is.null(nna[[hh]])) {
                    nna[[hh]] <- bc(dim(x)[hh]);
                }
            }
            res[[1]] <- c(as.character(nna[[2]]),xsep);
            for (ii in bc(nrow(x))) {
                res[[1]] <- c(res[[1]],nna[[1]][ii],as.character(x[ii,]),xsep);
            }
            res[[1]] <- res[[1]][-length(res[[1]])];
            res[[2]] <- rbsb.vma["p"];
        }
    } else {
        # dealing with a simple vector
        if (is.null(names(x))) {
            res[[1]] <- as.character(x);
            res[[2]] <- rbsb.vma["v"];
        } else {
            res[[1]] <- c(names(x),as.character(x));
            res[[2]] <- rbsb.vma["V"];
        }
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
is8rbsblist <- function(lili)
#TITLE checks whether a list is rbsb-compatible list or not
#DESCRIPTION
# A rbsb-list must satisfy the following two properties:
#{i} <<All components and sub-components being named.>>
#{ii} <<All components and sub-components being either a list
# or a character vector/matrix/array (vma components are called
# leafs of the rbsb-list).>>\cr
# Only such lists are handled by \code{list2file} and \code{file2list}
# functions.
#DETAILS
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{lili} << The list structure to be checked.>>
#[INPUTS]
#VALUE
# TRUE/FALSE according to the case.
# Also a global variable \code{rbsb.is8rbsblist} is created
# to give the result through a \code{des} object.
#EXAMPLE
# rbsb3k("reset");
# is8rbsblist(rbsb.lis1);
# is8rbsblist(list(rbsb.lis1));
# print(rbsb.is8rbsblist,what="c");
# is8rbsblist(list(rbsb.lis1,nu=NULL));
# print(rbsb.is8rbsblist,what="c");
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
if (rbsb.mck) {
    check4tyle(lili,"list",-1,message="lili must be a list");
}
# exploring the list
eee <- explore8list(lili);
# checking the presence on unamed components
#          and the types of the list
comment <- rbsb.cha0;
for (ii in bc(nrow(eee))) {
    if ((eee[ii,"name"]=="<NA>")|(eee[ii,"name"]=="")) {
        comment <- c(comment,
                     paste("The component",eee[ii,"numbers"],
                           "has got no name:",
                           paste("'",eee[ii,"names"],"'",sep=""))
                    );
    }
    coco <- get8listcomp(lili,eee[ii,,drop=FALSE]);
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
if (isempty(comment)) {
    res <- TRUE;
    dede <- new("des",name="result",
                orig="from is8rbsblist",
                time=now(),
                comm="This was a good rbsb-list.");
} else {
    res <- FALSE;
    dede <- new("des",name="result",
                orig="from is8rbsblist",
                time=now(),
                comm=c("This was NOT a good rbsb-list.",
                       comment));
}
# producing the answer
assign("rbsb.is8rbsblist",dede,pos=".GlobalEnv");
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


###########################################
###########################################
########
#((((((( NEW S4 CLASS nom
########
###########################################
###########################################

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8nom <- function(object)
#TITLE  checks a /nom/
#DESCRIPTION 
#   This function checks /nom/ objects
#DETAILS
# It is the validity method for /nom/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The nom object to be validated.>>
#[INPUTS]
#VALUE
# TRUE when the object seems acceptable
# else a character describing the error(s)
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_09_30
#REVISED 10_06_23
#--------------------------------------------
{
    if (class(object)!="nom") {
        erreur(NULL,paste("This object is not of class 'nom' but '",class(object),"'",sep=""));
    }
    res <- character(0);
    if (!("x" %in% slotNames(object))) {
        res <- c(res,"This supposed /nom/ has no slot @x!");
        return(res);
    }
    #
    # except when empty a named list is expected for @x
    if (length(object@x)>0) {
        if (is.null(names(object@x))) {
            res <- c(res,"slot x of nom must be a NAMED list");
        } 
    }
    # no duplicatd node names
    if (sum(duplicated(names(object@x)))>0) {
        res <- c(res,"Node names must be unique");
    }
    # no empty node names
    if ("" %in% names(object@x)) {
        res <- c(res,"Node names must not be empty");
    }
    # for the variable names
    for (jbd in bf(names(object@x))) {
        # at least one variable 
        if (length(object@x[[jbd]])==0) {
            res <- c(res,"A node (jbd) has got no variable name, '' must be introduced at minimum");
        }
        # no duplication within a node
        if (sum(duplicated(object@x[[jbd]]))>0) {
            res <- c(res,"Some of these variable names are duplicated");
        }
        # no empty node except when unique
        if (("" %in% object@x[[jbd]])&(length(object@x[[jbd]])>1)) {
            res <- c(res,"Variable names must not be empty except when the variable is unique");
        }
    }
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=rbsb.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

###########################################
setClass("nom", representation(
         x="list" # list of variable names of each node, the component names
                   # are the node names when there is no variable name,
		   # must be "" NOT character(0) for the length giving
                   # giving the number of variables in any cases...
                              ),
               prototype(x=vector("list",0)),
               validity=valid8nom
        );


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8nom <- function(x,...,what="v",type=0)
#TITLE  prints the node-variable names
#DESCRIPTION 
# prints the node-variable names of x (a 'nom' object)
#DETAILS
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<nom object>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.>>
#{what} << what to print ('v'=node:variable names;
#          'nv'=node[variable] names;
#         'n' only node names)
#{type} << type of printing: 
#              (0) everything on the same line;
#              (1) a node, a line.>>
#VALUE
# nothing a printing is issued
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# print(rbsb.nom2);
# print(rbsb.nom2,what='nv');
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_04_08
#REVISED 10_06_10
#--------------------------------------------
{
# some checks
if (rbsb.mck) {
    #
    che <- valid8nom(x);
    if (!identical(che,TRUE)) {
        erreur(che,"This /nom/ is not valid");
    }
    #
    types <- 0:1;
    if (!(type %in% types)) {
        erreur(list(types,type),"Bad argument");
    }
    #
    forma <- c("v","n","nv");
    if (!(what %in% forma)) {
        erreur(list(forma,what),"Bad argument");
    }
}
#
if (nbnv(x) > 0) {
    for (ii in names(x@x)) {
        if (what=="n") {
            cat(" ",ii);
        }
        if (what=="v") {
            cat(" ",ii);
            cat(form3liste(x@x[[ii]],none=character(0),OPA="[",CPA="]",opa="",cpa="",sep="|"));
        }
        if (what=="nv") {
            cat("  ");
            if (identical(x@x[[ii]],"")) { cat(ii);
            } else {
                cat(form3liste(x@x[[ii]],none=character(0),OPA="",CPA="",opa=paste(ii,"[",sep=""),cpa="]",sep=","));
            }
        }
    if (type==1) { if (ii!=names(x@x)[nbnv(x)]) { cat("\n");}}
    }
}
cat("\n");
#
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
length8nom <- function(x)
#TITLE  returns the length of a 'nom' object
#DESCRIPTION 
# provides the number of nodes of a \code{/nom/}.
#DETAILS
#KEYWORDS misc
#PKEYWORDS 
#INPUTS
#{x}<<the 'nom' object to be measured.>>
#[INPUTS]
#VALUE
# The number of nodes in 'x'
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# length(rbsb.nom1);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_04_29
#REVISED 09_04_29
#--------------------------------------------
{
# returning
length(x@x);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "nom"), print8nom);
setMethod("length",signature(x = "nom"), length8nom);


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
x2nom <- function(x)
#TITLE  returns the "nom" slot of an object
#DESCRIPTION 
# when 'x' is a 'nom' object returns it. Else
# checks if \code{x@nom} exists and returns it, if not
# a fatal error is issued.
#DETAILS
#KEYWORDS misc
#PKEYWORDS nom
#INPUTS
#{x} <<the bn/gn/dn/gn/.. or nom object>>
#[INPUTS]
#VALUE
# a 'nom' object
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# identical(rbsb.nom0,x2nom(rbsb.nom0));
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_04_20
#REVISED 09_09_29
#--------------------------------------------
{
if (!is(x,"nom")) {
    if (!("nom" %in% slotNames(x))) {
        str(x);
        erreur(slotNames(x),"'x' is not a 'nom' object or does not have got such a slot!");
    }
    x <- x@nom;
}
# returning
x;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
char2nom <- function(nova)
#TITLE  transforms a character into a 'nom' object
#DESCRIPTION 
# returns a \code{/nom/} whose names/variables comes from \code{nova}.
# Repetitions are possible and eliminated.
#DETAILS
# nodes and variables/nodes are sorted.
#KEYWORDS misc
#PKEYWORDS nom
#INPUTS
#{nova} <<character.>>
#[INPUTS]
#VALUE
# a 'nom' object with nodes possibly comprising covariables
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# print.default(char2nom(LETTERS[1:4]));
# print(char2nom(c("C[b]","A","B[one]","C[b]","C[a]","B[two]")));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_05_06
#REVISED 10_07_06
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(nova,"character",-1);
}
## preparing
nova <- sort(unique(nova));
nono <- nv2nod(nova);
nova <- nv2var(nova);
## getting the x slot
no <- sort(unique(nono));
# first the node names
xx <- as.list(rep("",length(no)));
names(xx) <- no;
# second the variate names
for (nn in no) {
    ou <- which(nn==nono)
    vv <- nova[ou];
    xx[[nn]] <- sort(vv);
}
res <- new("nom",x=xx);
if (rbsb.mck) {check4valid(valid8nom(res));}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nom2char <- function(nom,of="v",sort=FALSE)
#TITLE  transforms a /nom/ into a character
#DESCRIPTION 
# returns a character of the variables or node from a \code{/nom/}.
#DETAILS
# Notice that \code{char2nom(nom)} is equivalent to \code{nv2ion(0,nom,"v")@nvn}.
#KEYWORDS misc
#PKEYWORDS nom
#INPUTS
#{nom} <<The \code{nom} object to be transformed.>>
#[INPUTS]
#{of} <<'v' to return the set of variables; 'n' to return the set 
#       of involved nodes.>>
#{sort} <<Must the nodes and names be sorted?>>
#VALUE
# a \code{character}.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# nom2char(rbsb.nom3);
# nom2char(rbsb.nom3,"n");
#REFERENCE
#SEE ALSO nom2char
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_07_06
#REVISED 10_09_27
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4valid(valid8nom(nom));
    if (!(of %in% c("v","n"))) {
        erreur(of,"Must be 'v' for 'variables' or 'n' for 'nodes'");
    }
}
## preparing
if (of == "n") {
    # node names 
    res <- names(nom@x);
} else {
    # variable names
    res <- character(0);
    for (ii in bf(nom@x)) {
        nn <- names(nom@x)[ii];
        if(isvide(nom@x[[ii]])) {
            res <- c(res,nn);
        } else {
            res <- c(res,paste(nn,rbsb.cpt["variables","opening"],
                                  nom@x[[ii]],
                                  rbsb.cpt["variables","closing"],sep=""));
        }
    }
}
#
if (sort) { res <- sort(res);}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
list2nom <- function(lili)
#TITLE  transforms a list into a /nom/
#DESCRIPTION 
# simply returns \code{nom} with its unique slot \code{@x}
# being equal to \code{lili}. This
# function was introduced for consistency with other
# objects and to prepare future evolutions
#DETAILS
# According to \code{rbsb.mck} the produced object is checked.
#KEYWORDS misc
#PKEYWORDS nom
#INPUTS
#{lili} <<The \code{list} object to be transformed.>>
#[INPUTS]
#VALUE
# The resulting \code{nom}.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# list2nom(rbsb.nom3@x);
#REFERENCE
#SEE ALSO nom2list
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_07_07
#REVISED 10_07_07
#--------------------------------------------
{
## preparing
res <- new("nom",x=lili);
# checking
if (rbsb.mck) {
    check4valid(valid8nom(res));
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nom2list <- function(nom)
#TITLE  transforms a /nom/ into a list
#DESCRIPTION 
# simply returns \code{nom@x} which is a list. This
# function was introduced for consistency with other
# objects and to prepare future evolutions
#DETAILS
#KEYWORDS misc
#PKEYWORDS nom
#INPUTS
#{nom} <<The \code{nom} object to be transformed.>>
#[INPUTS]
#VALUE
# The resulting \code{list}.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# nom2list(rbsb.nom3);
#REFERENCE
#SEE ALSO list2nom
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_07_07
#REVISED 10_07_07
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4valid(valid8nom(nom));
}
## preparing
res <- nom@x;
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sort8nom <- function(nom,by="Aa")
#TITLE  sorts a /nom/ according to various ways
#DESCRIPTION 
# Nodes and variables of the \code{nom} are sorted
# according the alphabet, the number of variables.
#DETAILS
# Compatible criteria can be used simultaneously.
#KEYWORDS misc
#PKEYWORDS nom
#INPUTS
#{nom} <<The \code{nom} object to be sorted.>>
#[INPUTS]
#{by} <<A \code{character(1)} indicating the kind of sorting
#       by the letters in it. \code{a} for sorting the variables
#       within the node in alphabetical order. \code{A} for the nodes
#       and/or \code{S} with respect to their numbers of variates.>>
#VALUE
# The resulting \code{nom}.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# sort8nom(rbsb.nom3,"S");
# print(sort8nom(rbsb.nom7,"Aa"));
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_07_07
#REVISED 10_07_08
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4valid(valid8nom(nom));
    check4tyle(by,"character",1,message="'by' must be 'character(1)'");
}
## preparing
on <- bf(nom@x);
if (expr3present("S",by)) { on <- order(sapply(nom@x,length));}
if (expr3present("A",by)) { on <- order(names(nom@x));}
res <- vector("list",length(nom@x));
names(res) <- names(nom@x)[on];
al <- expr3present("a",by);
for (ii in bf(nom@x)) {
    iii <- names(res)[ii];
    if (!al) { res[[ii]] <- nom@x[[iii]];
    } else { res[[ii]] <- sort(nom@x[[iii]]);}
}
res <- new("nom",x=res);
if (rbsb.mck) { check4valid(valid8nom(res));}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
compare8nom <- function(noma,nomb,level="v")
#TITLE  compares two /nom/s
#DESCRIPTION 
# compares two /nom/s either at the node or 
# at the variable level.
#DETAILS
#KEYWORDS misc
#PKEYWORDS nom
#INPUTS
#{noma} <<The first \code{nom} to be compared.>>
#{nomb} <<The second \code{nom} to be compared.>>
#[INPUTS]
#{level} <<'v' or 'n' to indicate if the comparaison 
#          must be made at the 'variable' or 'node' level.>>
#VALUE
# A list of three /nom/ named \code{$a_b}, \code{$b_a} and
# \code{$a.b} giving the respectively the set differences and the 
# intersection of the choosen items.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# compare8nom(rbsb.nom2,rbsb.nom3);
# compare8nom(rbsb.nom2,rbsb.nom3,"n");
# compare8nom(rbsb.nom5,rbsb.nom3);
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_07_08
#REVISED 10_07_08
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4valid(valid8nom(noma));
    check4valid(valid8nom(nomb));
    check4tyle(level,"character",1,message="'level' must be 'character(1)'");
    if (!(level %in% c("v","n"))) {
        erreur(level,"'level' must be 'v' or 'n'");
    }
}
# taking care of the level
if (level == "n") {
    noma <- nom2nom(noma);
    nomb <- nom2nom(nomb);
}
# making the set operations
itema <- nom2char(noma);
itemb <- nom2char(nomb);
res <- vector("list",0);
res[["a_b"]] <- char2nom(setdiff(itema,itemb));
res[["b_a"]] <- char2nom(setdiff(itemb,itema));
res[["a.b"]] <- char2nom(intersect(itema,itemb));
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nom2nom <- function(nom,what="n")
#TITLE  transform a /nom/.
#DESCRIPTION 
# Reduces a /nom/ to its node.
#DETAILS
# For the moment, this is the only possiblility;
# further on more ideas can occur.
#KEYWORDS misc
#PKEYWORDS nom
#INPUTS
#{nom} <<The \code{nom} object to be transformed.>>
#[INPUTS]
#{what} <<\code{character(1)} indicating the kind of 
#       transformation. \code{n} for removing all the
#       the possible variables, leaving only the nodes.>>
#VALUE
# The resulting \code{nom}.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# print(nom2nom(rbsb.nom7));
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_07_08
#REVISED 10_07_08
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4valid(valid8nom(nom));
    check4tyle(what,"character",1,message="'by' must be 'character(1)'");
}
## preparing
if (expr3present("n",what)) {
    res <- lapply(bf(nom@x),function(x){"";});
    names(res) <- names(nom@x);
    res <- new("nom",x=res);
}
# checking
if (rbsb.mck) { check4valid(valid8nom(res));}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nv2nod <- function(vvn)
#TITLE  transforms complete variable names into node names
#DESCRIPTION 
# No ckeck is made about the existence of the node names
#DETAILS
# the syntax analysis is minimal, looking for a square bracket
# and eliminating evething from that point !
#KEYWORDS misc
#PKEYWORDS node
#INPUTS
#{vvn} <<vector of variable names>>
#[INPUTS]
#VALUE
# vector of deduced node names
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# nv2nod("aa");                   # "aa" is returned
# nv2nod(c("a[e]","az[ee]","b")); # c("a","az","b") is returned
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
# This function has been added to get the parents of an alk
# whithout reference to a gn/bn.
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_01_15
#REVISED 10_09_22
#--------------------------------------------
{
sep <- rbsb.cpt["variables","opening"];
un <- function(x) {x[1];}
res <- sapply(strsplit(vvn,sep,fixed=TRUE),un);
if (length(res) == 0) { res <- character(0);}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nv2var <- function(nvn)
#TITLE  returns the variable name from a node[variable] name
#DESCRIPTION 
# Just removing the node name and square brackets.
# \code{nvn} can be a vector. In case there is no variable name,
# the standard "" is returned
#DETAILS
# Variable name can be numeric and is returned as such
#KEYWORDS misc
#PKEYWORDS name var
#INPUTS
#{nvn} << character of the complete variable name>>
#[INPUTS]
#VALUE
# variable name without node name
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# nv2var(c("A[5]","B"));
#REFERENCE
#SEE ALSO va3va
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_01_03
#REVISED 10_09_22
#--------------------------------------------
{
sop <- rbsb.cpt["variables","opening"];
scl <- rbsb.cpt["variables","closing"];
#
res <- character(0);
for (i in bf(nvn)) {
    nv <- nvn[i];
    if (length(grep(sop,nv,fixed=TRUE)) == 0) {
        # no variable name
        res <- c(res,"");
    } else {
        if ((grep(sop,nv,fixed=TRUE) != 1) ||
            (grep(scl,nv,fixed=TRUE) != 1)) {
            cat("you provide (",nv,") as complete node[variable] name\n",sep="");
            erreur(NULL,"BUT it does not comprise the corresponding parenthesis!");
        }
        nv <- strsplit(nv,sop,fixed=TRUE)[[1]][2];
        nv <- strsplit(nv,scl,fixed=TRUE)[[1]][1];
        res <- c(res,nv);
    }
}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nbnv <- function(x,what=-1)
#TITLE  number of nodes/variables for a /nom/ or an /object@nom/
#DESCRIPTION
# According to 'what', returns the number of 
# nodes/variables for a \code{nom} or an object 
# comprising a slot \code{@nom}.
#DETAILS
# no check is performed.
#KEYWORDS misc
#PKEYWORDS node nb gn dn nom
#INPUTS
#{x} <<the bn/gn/dn/gn or nom object>>
#[INPUTS]
#{what} << 
#  -1: returns the number of nodes
#   0: returns the total number of variables
#   i: returns the number of variables of the i-th node.
# (For convenience 'n' is translated as -1 and 'v' is
# translated as 0).>>
#VALUE
# The number of nodes or variables, accordingly to what
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# nbnv(rbsb.nom2); # number of nodes
# nbnv(rbsb.nom2,0); # number of variates  
#COMMENT
# Remember that in most cases the number of
# variables of a dn associated to a bn is one
# more due to the scoring ">?<" variable.\cr
# This function is a fusion of former nbnd and
# nbva functions.
#AUTHOR J.-B. Denis
#CREATED 07_06_13
#REVISED 09_11_25
#--------------------------------------------
{
#
# checking
if (rbsb.mck) {
    check4tyle(what,c("integer","character"),1,message="Bad 'what' in 'nbnv'!");
}
x <- x2nom(x);
if (what=="n") { what <- -1;}
if (what=="v") { what <-  0;}
if (!is.numeric(what)) { erreur(what,"'what' is not acceptable!");}
if (what==-1) {
    res <- length(names(x@x));
} else {
    nvs <- sapply(x@x,length);
    if (length(nvs)==0) {nvs <- numeric(0);}
    if (what==0) {
        res <- sum(nvs);
    } else {
        if ((what < -1)|(what > length(nvs))) {
            erreur(list(x,what),"'what' is not a node number of the nom 'x'");
        } else {
            res <- nvs[what];
        }
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
and4nom <- function(nom,nod,var="")
#TITLE  adds one node to a /nom/
#DESCRIPTION
# returns \code{nom} after adding it a new node.
#DETAILS
#KEYWORDS misc
#PKEYWORDS nom node
#INPUTS
#{nom} <<The /nom/ to be completed.>>
#{nod} <<Name for the new node (character(1)).>>
#[INPUTS]
#{var} <<Name(s) for the variable of the new node.>>
#VALUE
# The /nom/ completed
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# print(rbsb.nom2);
# print(and4nom(rbsb.nom2,"D","z"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_05_06
#REVISED 09_05_06
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4valid(valid8nom(nom));
    check4tyle(nod,"character", 1,message="The node name (only one) was expected.");
    check4tyle(var,"character",c(1,Inf),message="The variable name(s) was expected [At least one].");
    if (nod %in% nanv(nom,"n")) {
        erreur(list(nom,nod),"The node you proposed already exists.");
    }
    if (length(unique(var))!=length(var)) {
        erreur(var,"You proposed duplicated variable names for the same node.");
    }
}
# addition
nom@x[[nod]] <- var;
# returning
nom;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rmnd4nom <- function(nom,nod)
#TITLE  removes one node to a /nom/
#DESCRIPTION
# removes one (and only one) node to a \code{/nom/}.
#DETAILS
#KEYWORDS misc
#PKEYWORDS nom node
#INPUTS
#{nom} <<The /nom/ to be restricted.>>
#{nod} <<Name of the name to be removed (character(1)).>>
#[INPUTS]
#VALUE
# The reducede /nom/
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# print(rmnd4nom(rbsb.nom2,"A"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_05_06
#REVISED 09_05_06
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4valid(valid8nom(nom));
    check4tyle(nod,"character", 1,message="The node name (only one) was expected.");
    if (!(nod %in% nanv(nom,"n"))) {
        erreur(list(nom,nod),"The node you proposed does not exist in that /nom/.");
    }
}
# removing
qui <- which(names(nom@x)!=nod);
nom@x <- nom@x[qui];
# returning
nom;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nanv <- function(x,what=-1)
#TITLE  returns the names of nodes/variables of x2nom(nom)
#DESCRIPTION
# According to 'what', returns the names of 
# nodes/variables for \code{nom} or \code{nom@nom}.
#DETAILS
# As it is a very basic function, no check is 
# made about the arguments.
#KEYWORDS misc
#PKEYWORDS nom node variable
#INPUTS
#{x} <<the object to consider>>
#[INPUTS]
#{what} << 
#  -2: returns all names of the variables (without the node name)
#  -1: returns the names of the nodes
#   0: returns all names of the variables (including the node name)
#   i: returns the names of the variables of the i-th node.
# (For convenience 'n' is translated as -1 and 'v' is
# translated as 0).>>
#VALUE
# The names of nodes or variables, accordingly to \code{what}.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# nanv(rbsb.nom2)    # node names
# nanv(rbsb.nom2,0)  # node[variable] names
# nanv(rbsb.nom2,-2) # variable names
#COMMENT
# Remember that in most cases the number of
# variables of a dn associated to a bn is one
# more due to the scoring ">?<" variable.\cr
# This function is consistent with nbnv.
#AUTHOR J.-B. Denis
#CREATED 09_05_06
#REVISED 09_11_25
#--------------------------------------------
{
# no checking
#
sep <-  rbsb.cpt["variables",];
#
x <- x2nom(x);
if (what=="n") { what <- -1;}
if (what=="v") { what <-  0;}
if (!is.numeric(what)) { erreur(what,"'what' is not acceptable!");}
if (what==-1) {
    res <- names(x@x);
} else {
    if (what==0) {
        res <- character(0);
        for (ii in bf(x@x)) {
            if (all(x@x[[ii]]=="")) {
                res <- c(res,names(x@x)[ii]);
            } else {
                res <- c(res,
                         paste(names(x@x[ii]),
                               sep[1],
                               x@x[[ii]],
                               sep[2],
                               sep="")
                        );
            }
        }
    } else {
	if (what==-2) {
	    res <- character(0);
	    for (ii in bf(x@x)) {
		res <- c(res,x@x[[ii]]);
	    }
	} else {
	    if ((what < -1)|(what > length(x@x))) {
		erreur(list(x,what),"'what' is not a node number of the nom 'x'");
	    } else {
		ii <- what;
		if (all(x@x[[ii]]=="")) {
		    res <- names(x@x)[ii];
		} else {
		    res <- paste(names(x@x[ii]),
				 sep[1],
				 x@x[[ii]],
				 sep[2],
				 sep="");
		}
	    }
	}
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nv3nom <- function(na,nom)
#TITLE  detects the existence of names into x2nom(nom)
#DESCRIPTION
# Detects the existence of 'na' as a valid node names or
# a valid variable names included into \code{nom}
# or \code{nom@nom}.
#DETAILS
# In case of identical variable and node names, the node
# will be retained: \code{nv3nom("a",new("nom",x=list(a="a")))}
# returns 1, not -1.
#KEYWORDS misc
#PKEYWORDS node
#INPUTS
#{na} <<name vector to be looked for>>
#{nom}    <<nom object to make the correspondence.>>
#[INPUTS]
#VALUE
# a named (with \code{na}) numeric vector of \code{length(na)}.
# Its values are:
#   -i when a variable name of the ith node
#    0 when no correspondance;
#    1 when a node name with variable(s);
#    2 when a node[variable] name;
#    3 when a node name without variable.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# nv3nom(c("C[1]","C[10]","2","C","B","a"), rbsb.nom3);
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_04_16
#REVISED 10_09_22
#--------------------------------------------
{
# transforming
nom <- x2nom(nom);
# some checking
if (rbsb.mck) {
    check4valid(valid8nom(nom));
    check4tyle(na,rbsb.chara,-1);
}
# preparing
res <- rep(0,length(na));
names(res) <- na;
vari <- nanv(nom,-2);
node <- nanv(nom,-1);
nova <- nanv(nom, 0);
if (length(nom)>0) {
    # looking for single variables
    var <- which(apply(outer(na,vari,"=="),1,sum)==1);
    res[var] <- -1;
    # looking for nodes
    nod <- which(apply(outer(na,node,"=="),1,sum)==1);
    res[nod] <- 1;
    # looking for node[variable]
    nov <- which(apply(outer(na,nova,"=="),1,sum)==1);
    res[nov] <- 2
    # refining the nodes and variables alone
    for (ii in bf(res)) {
        nam <- names(res)[ii];
        if (res[ii] == 1) {
            if (all(nom@x[[nam]]=="")) {
                # this is a single node
                res[ii] <- 3;
            }
        }
        if (res[ii] == -1) {
            # a single variable looking for its node
            vou <- which(nam==vari)[1];
            nou <- nv2nod(nova)[vou];
            res[ii] <- - which(nou==names(nom@x));
        }
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8ion <- function(object)
#TITLE  checks a /ion/
#DESCRIPTION (ba)
#   This function checks /ion/ objects
#DETAILS
# It is the validity method for /ion/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The ion object to be validated.>>
#[INPUTS]
#VALUE
# TRUE when the object seems acceptable
# else a character describing the error(s)
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_10_01
#REVISED 10_06_23
#--------------------------------------------
{
    if (class(object)!="ion") {
        erreur(NULL,paste("This object is not of class 'ion' but '",class(object),"'",sep=""));
    }
    res <- character(0);
    #
    # each slot must have the same lenght
    nl <- length(object@nn);
    if (length(object@vn) != nl) { res <- c(res,"length of @vn  is different from this of @nn");}
    if (length(object@nvn)!= nl) { res <- c(res,"length of @nvn is different from this of @nn");}
    if (length(object@nk) != nl) { res <- c(res,"length of @nk  is different from this of @nn");}
    if (length(object@ij) != nl) { res <- c(res,"length of @ij  is different from this of @nn");}
    if (length(object@vk) != nl) { res <- c(res,"length of @vk  is different from this of @nn");}
    #
    # all variate names must be different
    if (length(object@nvn) != length(unique(object@nvn))) {
        res <- c(res,paste(object@nvn,"repetitions between variate names"));
    }
    
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=rbsb.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

###########################################
# see the comment into nv2ion for some explanation...
#
setClass("ion", representation(
         nn="character",  # node names
         vn="character",  # variable names
         nvn="character", # node[variable] names
         nk="numeric",    # node numbers
         ij="numeric",    # variable numbers within node
         vk="numeric",    # variable numbers
         iden="character" # identification of the inputs
                              ),
               prototype(nn=character(0),vn=character(0),nvn=character(0),
                         nk=numeric(0),ij=numeric(0),vk=numeric(0),
                         iden=character(0)),
               validity=valid8ion
        );


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
length8ion <- function(x)
#TITLE  returns the length of a 'ion' object
#DESCRIPTION (ba)
# provides the length of a \code{/ion/}.
#DETAILS
#KEYWORDS misc
#PKEYWORDS 
#INPUTS
#{x}<<the 'ion' object to be measured.>>
#[INPUTS]
#VALUE
# The number of items in 'x'
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# length(rbsb.ion0);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_04_28
#REVISED 09_04_28
#--------------------------------------------
{
# returning
length(x@nn);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8ion <- function(x,how="n")
#TITLE  prints a 'ion' object
#DESCRIPTION (ba)
# print associated to a \code{/ion/} object.
#DETAILS
#KEYWORDS print
#PKEYWORDS 
#INPUTS
#{x}<<the 'ion' object to be printed.>>
#[INPUTS]
#{how} << the way to make the printing:
#               'n' for node names
#               'v' for variable names
#               'i' for node numbers
#               'j' for nested variable numbers
#               'k' for variable numbers
#               'a' for everything.>>
#VALUE
# Nothing but a printing is issued
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# print(rbsb.ion0);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_04_28
#REVISED 09_04_28
#--------------------------------------------
{
# some checks
che <- valid8ion(x);
if (!identical(che,TRUE)) {
    erreur(che,"This /ion/ is not valid");
}
#
if (expr3present("a",how)) { how<-"nvijk";}
if (length(x)==0) { return(invisible());}
# constituting the necessary matrix
nr <- character(0); ma <- matrix(0,0,length(x@nn));
if (expr3present("n",how)) {
    nr <- c(nr,"nn");
    ma <- rbind(ma,x@nn);
}
if (expr3present("v",how)) { if (length(x@vn)>0) {
    nr <- c(nr,"vn");
    ma <- rbind(ma,x@vn);
}}
if (expr3present("i",how)) {
    nr <- c(nr,"nk");
    ma <- rbind(ma,x@nk);
}
if (expr3present("j",how)) {
    nr <- c(nr,"k.j");
    ma <- rbind(ma,x@ij);
}
if (expr3present("k",how)) { if (length(x@vk)>0) {
    nr <- c(nr,"vk");
    ma <- rbind(ma,x@vk);
}}
dimnames(ma) <- list(nr,x@nvn);
print(ma);
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "ion"), print8ion);
setMethod("length",signature(x = "ion"), length8ion);


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nv2ion <- function(x,nom,kwhat="n",check=TRUE)
#TITLE  returns the /ion/ of a series of
# nodes/variables
#DESCRIPTION (ba)
# This is a central, basic and very general function for
# programming with /rebastaba/, so not 
# that easy to grasp. It is suggested to read the detail section.\cr
# From a series of nodes (or variables) indicated with 'x'
# returns their complete name/index descriptions under a 'ion' object.
# Checking can be desactivated, anyway it is of course conditional 
# to \code{rbsb.mck}...\cr
# Be aware that the proposed order is not respected, nodes/variables are
# sorted!
#DETAILS
# There are different ways to designate a subset of nodes/variables belonging
# to a /nom/ object. Let A[a], A[b], A[c], B, C[1], C[2] be the set of 
# variables of /nom/ \code{uu} given in the example section. Intuitively, we
# coud designate the first node in different ways: (1) as the first node, 
# (2) as the node of name 'A', (3) as the first three variables of \code{uu},
# (4) as the subset of variables 'A[a]', 'A[b]' and 'A[c]'... \code{nv2ion}
# using one of these ways (and more) returns equivalent ways of defining them.\cr
# Said in a different manner, it gives the simultaneous translation
# of one way in every known ways. This could be quite useful for exchanges
# between user and algorithms. Another properties is to give the subsets 
# in a unique way.\cr
# In fact \code{"-"} is \code{rbsb.all}.
#KEYWORDS misc
#PKEYWORDS node variable
#INPUTS
#{x} << indicates one or several subsets of nodes/variables of the second 
# argument (\code{nom}). When they are several subsets, \code{nv2ion} 
# deals with the union of them. The indication can
# be made (1) by names (\code{character}
# interpreted of nodes or variates according to the third argument \code{kwhat};
# (ii) a numeric matrix of two rows giving its columns the [node number, variable number];
# (iii) a numeric giving the index of nodes or variables according
# to the third argument \code{kwhat}.\cr
# See the description section for more insights.\cr
# Additional facilities are given by the extensions of '-'
# (when \code{x} is a character), 0 and -1 (when \code{x} is numeric).
# \code{"-"}, \code{matrix(c(0,-1),2)} and \code{0} are equivalent. 
# Notice that \code{matrix(c(0,0),2)} will keep on the developped set of
# variable names (not using '-').
# >>
#{nom}    <<nom object of reference.>>
#[INPUTS]
#{kwhat} << This argument is used in two different ways
#                 according its values. When 'x' is a single
#                 numeric it indicates if the user wants to specify
#                 a node ("n"/"N") or a variable ("v"). But when it is
#                 a node (either numeric or character), it indicates
#                 if the node is wanted ("n") of the set of variables
#                 of this node ("N").
#{check} <<( when TRUE checking of the argument consistence
#          is performed if rbsb.mck is also TRUE.>>
#VALUE
# A 'ion' object comprising names, indices and identifications.\cr
#{@nn} <<The node names.>>
#{@vn} <<The variable names.>>
#{@nvn}<<The node[variable] names.>>
#{@ij} <<The variable indices within each node.>>
#{@nk} <<The indices at the node level.>>
#{@vk} <<The indices at the variable level.>>
#{@iden} <<The identification of the \code{x} inputs by
# a character vector of the same length and containing
# either 'nn' (-> node as node), or 'nv' (-> node as variable set),
# or 'v' (-> variable level).>>
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# nv2ion(1,rbsb.nom2,"n");
# nv2ion(1,rbsb.nom2,"N");
# nv2ion(1,rbsb.nom2,"v");
# nv2ion(0,rbsb.nom2,"n");
# nv2ion("-",rbsb.nom2,"n");
# nv2ion("A",rbsb.nom2,"n");
# nv2ion("B",rbsb.nom2,"n");
# nv2ion("C",rbsb.nom3,"n");
#REFERENCE
#SEE ALSO Before using \code{nv2ion}, it is suggested to run the script \code{rsba.demo.ion.r}.
#CALLING
# uniqueness is attempted but redundancy is not avoided...
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_06_28
#REVISED 10_06_10
#--------------------------------------------
{
#=====================================================
# STEP 1: investigation of the arguments, checking, preparation
#===================================================== 
# some checking and preparation
if (check) {
    if (rbsb.mck) {check4valid(valid8nom(nom));}
    if (!(kwhat %in% c("n","N","v"))) {
        erreur(kwhat,"what must be 'n', 'N' or 'v'");
    }
}
#
# determining the case and the length of input
if (is.character(x)) {
    if (isvide(x)) { x <- character(0); }
    ca <- "cha"; na <- length(x);
} else {
   if (!is.numeric(x)) {
       erreur(x,"'x' must be numeric (or character)!");
   } else {
       if (is.matrix(x)) {
           if (nrow(x)!=2) {
               erreur(x,"When 'x' is a matrix, TWO rows are expected!");
           } else {
               ca <- "mat"; na <- ncol(x);
           }
       } else {
           ca <- "vec"; na <- length(x);
       }
   }
}
#
# na is the number of asked subsets
# cha is the type under which the subsets are defined
#     *vec*tor, *mat*rix, *cha*racter.
#
# preparing the result as a list
iden <- character(na);
maij <- matrix(NA,2,0);
if (na==0) { return(rbsb.ion0);}
#
lll <- c(0,cumsum(sapply(nom@x,length)));
names(lll) <- NULL;
#=====================================================
# STEP 2: processing each subset in a jbd loop
#===================================================== 
for (jbd in bc(na)) {
    # the ij notation is used as a common standard
    # that is all coding are first translated under the
    # c(i,j) format of the matrix columns, 'i' being the 
    # node number and 'j' the variable number.
    #=====================================================
    # STEP 2.A: the subset is translated into c(n,v) code
    #===================================================== 
    if (ca=="cha") {
        # under character specification
        xx <- x[jbd];
        if (xx==rbsb.all) {
            # the shortcut
            xxx <- c(0,-1);
	    # but this can be modified at the variable level
	    # with the use of 'kwhat'
            if (kwhat=="N") { xxx <- c(0,0);}
        } else {
	    # standard specification
	    nn <- nv2nv(xx);
	    no <- which(nn$nod==names(nom@x));
	    if (length(no)==0) {
                form3affiche(x);
		erreur(list(nom,nn),"Not accepted as node name in 'nom'");
	    }
	    if (xx==nn$nod) {
		# at the node level
		xxx <- c(no,-1);
		# but this can be modified at the variable level
		# with the use of 'kwhat'
		if (kwhat=="N") { xxx <- c(no,0);}
	    } else {
		# at the variable level
		if (no==0) {
		    xxx <- c(0,0);
		} else {
		   if (nn$var==rbsb.all) {
		       xxx <- c(no,0);
		   } else {
		       nv <- which(nn$var==nom@x[[nn$nod]]);
		       if (check) { if (length(nv)==0) {
			   erreur(list(nom,nn),"The variable name was not found");
		       }}
		       xxx <- c(no,nv);
		   }
		}
	    }
	}
    }
    if (ca=="mat") {
        # under matrix column specification
        xx <- x[,jbd];
        xxx <- xx;
    }
    if (ca=="vec") {
        # under scalar specification
        xx <- x[jbd];
        if (kwhat=="v") {
            # at the variable level
            if (xx==0) {
                xxx <- c(0,0);
            } else {
                kn <- sum(lll < xx);
                if (kn == 1) {
                    xxx <- c(1,xx);
                } else {
                    xxx <- c(kn,xx-lll[kn]);
                }
            }
        } else {
            # at the node level
            if (kwhat=="n") {
                # at the stric node level
                xxx <- c(xx,-1);
            } else {
                xxx <- c(xx,0);
            }
        }
    }
    #=====================================================
    # STEP 2.B: checking the obtained code
    #===================================================== 
    if (xxx[1] < -1) {
        erreur(x,"Non acceptable negative node number was found");
    }
    if (xxx[1] > length(nom@x)) {
        erreur(x,"Too high node number was found");
    }
    if (xxx[2] < -1) {
        erreur(x,"Non acceptable negative variable number was found");
    }
    if (xxx[1]>0) { if (xxx[2] > length(nom@x[[xxx[1]]])) {
        erreur(x,"Too high variable number was found");
    }}
    #=====================================================
    # STEP 2.C: performing the identification
    #===================================================== 
    if (xxx[2]==-1) { iden[jbd] <- "nn";}
    if (xxx[2]== 0) { iden[jbd] <- "nv";}
    if (xxx[2] > 0) { iden[jbd] <- "v";}
    if (iden[jbd]=="") {
        rapport("Bad identification in 'nv2ion'");
    }
    #=====================================================
    # STEP 2.D: expanding the different codes
    #===================================================== 
    if (xxx[1] == 0) {
        # nodes are repeated
        if (xxx[2] == 0) {
            # all variables
            xxx <- matrix(NA,2,0);
            for (ii in bf(nom@x)) {
                lon <- length(nom@x[[ii]]);
                xxx <- cbind(xxx,matrix(c(rep(ii,lon),bc(lon)),
                                        nrow=2,byrow=TRUE));
            }
        } else {
            # all nodes
            if (xxx[2] == -1) {
                xxx <- matrix(c(1:length(nom@x),rep(0,length(nom@x))),
                              2,byrow=TRUE);
            } else {
                rapport("nv2ion: unexpected variable specification");
            }
        }
    } else {
        # a specific node
        if (xxx[2] == 0) {
            xxx <- matrix(c(rep(xxx[1],length(nom@x[[xxx[1]]])),
                          1:length(nom@x[[xxx[1]]])),
                          2,byrow=TRUE);
        } else {
            xxx <- matrix(xxx,2);
        }
    }
    #=====================================================
    # STEP 2.E: cumulating into the (n,v) way
    #===================================================== 
    #
    maij <- cbind(maij,xxx);
    #
} # ending the loop (jbd in bc(na))
if (ncol(maij)==0) { return(rbsb.ion0);}
#=====================================================
# STEP 3: sorting and elimination from the matrix form
#=====================================================
maj <- 2*(10+max(maij[2,]));
sco <- maij[1,]*maj + maij[2,];
ord <- order(sco);
sco <- sco[ord];
qui <- c(TRUE,sco[-1]-sco[-length(sco)]>0);
maij <- maij[,ord[qui],drop=FALSE];
#=====================================================
# STEP 4: filling all the fields with the results
#===================================================== 
res <- list(nn=character(0),vn=character(0),nvn=character(0),
            ij=matrix(NA,2,0),
            nk=numeric(0),vk=numeric(0),iden=character(0));
res$ij <- maij;
for (sd in bc(ncol(maij))) {
    xxx <- res$ij[,sd];
    res$nn <- c(res$nn,names(nom@x)[xxx[1]]);
    res$nk <- c(res$nk,xxx[1]);
    #
    if (xxx[2]>0) {
        res$vn <- c(res$vn,nom@x[[xxx[1]]][xxx[2]]);
        res$vk <- c(res$vk,lll[xxx[1]]+xxx[2]);
    } else {
        res$vn <- c(res$vn,rbsb.all);
        res$vk <- c(res$vk,0);
    }
    #
    res$nvn <- paste(res$nn,
                     rbsb.cpt["variables","opening"],
                     res$vn,
                     rbsb.cpt["variables","closing"],
                     sep="");
}
# adaptation for unamed variables
res$nvn[res$vn==""] <- res$nn[res$vn==""];
#
# dealing with the special case of no nodes
if (length(res$nn) == 0) {
    res$vn <- res$nvn <- character(0);
    res$ij <- matrix(0,2,0);
    res$nk <- res$vk <- numeric(0);
}
#
# returning
new("ion",nn=res$nn,vn=res$vn,nvn=res$nvn,
          nk=res$nk,ij=res$ij[2,],vk=res$vk,
          iden=iden);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sort8ion <- function(ion,nom,sort="n",rm.redun=TRUE)
#TITLE  sorts a 'ion' object
#DESCRIPTION (ba) sorts a 'ion' object possibly eliminating
# the redundancies
#DETAILS
# The algorithm does note take care of \code{rbsb.who}, changing
# it implies changing this algorithm...
#KEYWORDS misc
#PKEYWORDS 
#INPUTS
#{ion}<<The 'ion' to be sorted.>>
#{nom}<<associated 'nom' structure to perform the sorting.>>
#[INPUTS]
#{sort} << the way to make the sorting
#               'n': according to 'nom'
#               'a'; according to the alphabet.>>
#{rm.redun} << Must the redundancies be removed?>>
#VALUE
# The sorted [reduced] 'ion'
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# uu <- rbsb.nom2;
# vv <- nv2ion(0,uu);
# print(sort8ion(vv,uu));
# print(sort8ion(vv,uu,"a"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_04_29
#REVISED 09_04_29
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4valid(valid8nom(nom));
    check4valid(valid8ion(ion));
}
# sorting
if (sort=="a") {
    # alphabetical sorting
    oo <- order(ion@nvn);
} else {
    # natural sorting
    oo <- order((nbnv(nom)+1)*ion@nk+ion@vk);
}
for (ii in slotNames(ion)) { if (length(slot(ion,ii))>0) {
    ax <- slot(ion,ii);
    slot(ion,ii) <- ax[oo];
}}
# eliminating the redundancies at the simple level
if (rm.redun) { if (length(ion)>1) {
    dd <- length(ion);
    rr <- c(ion@nvn[-1]!=ion@nvn[-dd],TRUE);
    if (sum(rr)<length(rr)) {
        for (ii in slotNames(ion)) {
            slot(ion,ii) <- slot(ion,ii)[rr];
        }
    }
}}
# eliminating the redundancies at the global level
rr <- numeric(0);
## finding all global nodes (forgetting rbsb.who)
glo <- which("-"==ion@vn);
## finding all nodes
ano <- unique(ion@nn);
## exploring them
for (no in ano) {
    ## for each component of it
    cno <- which(no==ion@nn);
    if (length(cno) > 1) {
        ## are there global nodes for it
        gno <- intersect(glo,cno);
        if (length(gno)>0) {
            ## only one is sufficient
            rr <- c(rr,setdiff(cno,gno[1]));
        }
       
    }
}
## removing the redundant ones
if (length(rr)>0) {
    kk <- setdiff(1:length(ion),rr);
    for (ii in slotNames(ion)) {
        slot(ion,ii) <- slot(ion,ii)[kk];
    }
}
# returning
ion;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nv2nv <- function(xx)
#TITLE  transforms node[variable] characters
# into node and variable characters.
#DESCRIPTION (ba)
# just removing possible square brackets to give
# back the node names ($nod) and the variable 
# names ($var)
#DETAILS
# In fact square brackets are the parentheses
# given by the constant \code{rbsb.cpt["variables",]}.
#KEYWORDS misc
#INPUTS
#{xx} <<The character to transform>>
#[INPUTS]
#VALUE
# a list with two components\cr
#{$nod} <<The node names.>>
#{$var} <<The variable names: '' when absent.>>
#{$vva} <<The variable names but node name when absent.>>
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# nv2nv(c("A","A[666]","")); # list(nod=c("A","A"),var=c("","666",""),vva=c("A","666","")
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_04_23
#REVISED 10_08_12
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(xx,"character",-1);
}
#
res <- list(nod=character(0),var=character(0),vvar=character(0));
cou1 <- rbsb.cpt["variables","opening"];
cou2 <- rbsb.cpt["variables","closing"];
#
for (jbd in bf(xx)) {
    x <- xx[jbd];
    if (x == "") {
        res$nod <- c(res$nod,"");
        res$var <- c(res$var,"");
        res$vva <- c(res$vva,"");
    } else {
        uu <- strsplit(x,cou1,fixed=TRUE);
        res$nod <- c(res$nod,uu[[1]][1]);
        if (x==uu[[1]][1]) {
            res$var <- c(res$var,"");
        } else {
            vv <- strsplit(uu[[1]][2],cou2,fixed=TRUE);
            if (uu[[1]][2]==vv[[1]][1]) {
                erreur(x,"Missing bracketing");
            } else {
                res$var <- c(res$var,vv[[1]][1]);
            }
        }
        #
        if (res$var[jbd]=="") { res$vva <- c(res$vva,res$nod[jbd]);
        } else { res$vva <- c(res$vva,res$var[jbd]);}
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
isempty <- function(x) 
#TITLE  tests the nullness of objects
#DESCRIPTION
# Returns TRUE is the structure is empty.\cr
# This trick was proposed because the \code{NULL} cannot replace
#  any kind of objects. Generally, constant finishing with \code{0} like
#  \code{rbsb.lis0}, \code{rbsb.dfr0}... are null objects.\cr
#  Notice that \code{isempty("")} is FALSE.
#DETAILS
# see the code to know the list.  
#PKEYWORDS
#KEYWORDS programming
#INPUTS
#{x}    <<object to be scrutinazed>>
#[INPUTS]
#VALUE
# TRUE when the object is considered as empty
# FALSE if not
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# isempty(numeric(0));
# isempty(NULL);
# isempty(rbsb.fau0);
# isempty("");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_15
#REVISED 10_09_23
#--------------------------------------------
{
  if (is.null(x))                      { return(TRUE);}
  if (length(x)==0)                    { return(TRUE);}
  if (identical(x,rbsb.log0))          { return(TRUE);}
  if (identical(x,rbsb.num0))          { return(TRUE);}
  if (identical(x,rbsb.cha0))          { return(TRUE);}
  if (identical(x,rbsb.lis0))          { return(TRUE);}
  if (identical(x,rbsb.fun0))          { return(TRUE);}
  if (identical(x,rbsb.dfr0))          { return(TRUE);}
  FALSE;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
isvide <- function(x) 
#TITLE  to avoid difficulty with is.null
#DESCRIPTION (ba)
# returns TRUE is the structure is null,
# empty or vide. For this last case, their list is given
#  by \code{rbsb.null} which is increased with new objects by the children
#  packages.\cr
# Any object being null for \code{isempty} is for \code{isvide}.  \cr
#  Notice that \code{isvide("")} is FALSE.
#DETAILS
# Have a look to the code itself.
#PKEYWORDS 
#KEYWORDS utilities
#PKEYWORDS utilities
#INPUTS
#{x}    <<object to be scrutinazed>>
#[INPUTS]
#VALUE
# TRUE when the object is considered as 
# not filled, FALSE if not.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# isvide(NULL);
# isvide(rbsb.fau1);
# isvide(rbsb.fau0);
# isvide("");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_15
#REVISED 10_09_27
#--------------------------------------------
{
  # initializing
  res <- FALSE;
  # covering 'isempty' null cases
  if (isempty(x)) { return(TRUE);}
  # covering some special non null cases
  if (is.function(x))   { return(FALSE);}
  if (is.data.frame(x)) { return(FALSE);}
  # looping over the null declared objects
  for (oo in rbsb.null) {
      tutu <- paste("if (identical(x,",oo,")) {res <- TRUE;}",sep="");
      eval(parse(text=tutu));
  }
  # looking for non significant character
  if (is.character(x)) { if (length(x)==1) { if (x=="")    { return(TRUE);}}}
  # at last null was not dectected
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
ustat <- function(wh="all")
#TITLE provides function computing usual statistics.
#DESCRIPTION 
# This function is aimed to be an argument for the function
# \code{msdistri}. It returns a list of two parallel components:
# (1) a vector a names of each statistics and
# (2) a list of functions computing the statistics.
#DETAILS
# The statistics are computed after removing the \code{NA}
# values.\cr
# See the code for more details.
#KEYWORDS
#PKEYWORDS plot
#INPUTS
#[INPUTS]
#{wh} <<Defines which statistics must be used.
# The different possibilities are:\cr
# "all" for all of the following;\cr "n" for number of
# non missing values;\cr "m" for the mean;\cr "s" for the
# standard deviation;\cr "M" for the median;\cr "i" for the minimum;\cr
# "a" for the maximum;\cr "2" for the 0.025 and 0.975
# quantiles and their difference;\cr
# "5" for the 0.05 and 0.95 quantiles and
# their difference;\cr "i" for the
# 0.25 and 0.75 quantiles and their difference.>>
#VALUE
# A named list, each component being a function taking
# a vector of numeric values as input and returning
# the corresponding statistics.
#EXAMPLE
# rbsb3k("RESET"); # For R checking compliance
# print(ustat("n"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_09_20
#REVISED 10_09_20
#--------------------------------------------
{
#
# checking
if (rbsb.mck) {
    check4tyle(wh,rbsb.chara,1);
}    
#
# interpreting the order
if (wh=="all") { wh <- "nmsMia25i";}
#
# preparing the result
res <- vector("list",0);
#
# creating the necessary functions
if (expr3present("n",wh)) {
    res$nb <- function(x) {
                  sum(!is.na(x));
                          }
    res$'%nb' <- function(x) {
                  round(sum(!is.na(x)) * 100 / length(x),0)
                          }
}
#
if (expr3present("m",wh)) {
    res$mean <- function(x) {
                  mean(x,na.rm=TRUE);
                          }
}
#
if (expr3present("s",wh)) {
    res$std.dev. <- function(x) {
                  sqrt(var(x,na.rm=TRUE));
                          }
}
#
if (expr3present("M",wh)) {
    res$median <- function(x) {
                  median(x,na.rm=TRUE);
                          }
}
#
if (expr3present("i",wh)) {
    res$minimum <- function(x) {
                  min(x,na.rm=TRUE);
                          }
}
#
if (expr3present("a",wh)) {
    res$maximum <- function(x) {
                  max(x,na.rm=TRUE);
                          }
}
#
if (expr3present("2",wh)) {
    res$'Q2.5%' <- function(x) {
                  quantile(x,0.025,na.rm=TRUE);
                          }
    res$'Q97.5%' <- function(x) {
                  quantile(x,0.975,na.rm=TRUE);
                          }
    res$'R95%' <- function(x) {
                  quantile(x,0.975,na.rm=TRUE) -
                  quantile(x,0.025,na.rm=TRUE);
                          }
}
#
if (expr3present("5",wh)) {
    res$'Q5%' <- function(x) {
                  quantile(x,0.05,na.rm=TRUE);
                          }
    res$'Q95%' <- function(x) {
                  quantile(x,0.95,na.rm=TRUE);
                          }
    res$'R90%' <- function(x) {
                  quantile(x,0.95,na.rm=TRUE) -
                  quantile(x,0.05,na.rm=TRUE);
                          }
}
#
if (expr3present("i",wh)) {
    res$'Q25%' <- function(x) {
                  quantile(x,0.25,na.rm=TRUE);
                          }
    res$'Q75%' <- function(x) {
                  quantile(x,0.75,na.rm=TRUE);
                          }
    res$'R50%' <- function(x) {
                  quantile(x,0.75,na.rm=TRUE) -
                  quantile(x,0.25,na.rm=TRUE);
                          }
}
#
#
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
look4k <- function(words,where=rbsb.pko,what="dv",
                   strict=TRUE,how=length(words),imp=TRUE)
#TITLE looks associated constants to some words
#DESCRIPTION 
# Looks among the constants defined in the different
# packages which seems relevant with respect to some
# proposed keywords.
#DETAILS
# The investigation is a fuzzy search with \code{agrep}
# values.\cr
#KEYWORDS
#PKEYWORDS search
#INPUTS
#{words} <<vector of keywords to be investigated.>>
#[INPUTS]
#{where} <<names of the packages to be investigated.>>
#{what} <<what must be returned? \code{d} for definition
#         and \code{v} for value.>>
#{strict} <<Must the search be an exact (versus fuzzy) search?>>
#{how} <<how many positive results (with respect to each keyword
#        must be displayed?>>
#{imp} <<must the result be displayed?
#        if not a list with the result is returned.>>
#VALUE
# Nothing but the result is displayed.
#EXAMPLE
# rbsb3k("RESET"); # For R checking compliance
# look4k("print");
# look4k("daf",what="");  
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_10_11
#REVISED 10_10_11
#--------------------------------------------
{
#
# to avoid R remards
chaine <- val <- tout <- rbsb.cha0;
#
# checking
if (rbsb.mck) {
    check4tyle(words,rbsb.chara,c(1,Inf));
    check4tyle(where,rbsb.chara,c(1,Inf));
    check4tyle(strict,rbsb.logic,1);
    check4tyle(how,rbsb.numer,1,c(1,length(words)));
}    
#
# preparing the result
re2 <- vector("list",0);
res <- numeric(0);
if (strict) { madi <- 0;
} else { madi <- 0.1;}
#
# investigating series of constants
for (kk in bf(where)) {
    toto <- paste("chaine <- ",where[kk],"3k('definitions');",sep="");
    eval(parse(text=toto));
    nako <- paste(where[kk],names(chaine),sep=".");
    rr <- rep(0,length(nako));
    names(rr) <- nako;
    # for a given series of constants, investigating each constant
    for (ko in bf(rr)) {
        cha <- paste(names(rr)[ko],chaine[ko],sep=" ");
        # applying to the proposed series of keywords
        for (ww in words) {
            toto <- paste("tout <- agrep(ww,cha,max.distance=madi,ignore.case=TRUE);");
            eval(parse(text=toto));
            if (length(tout)>0) { rr[ko] <- rr[ko] + 1;}
        }
    }
    # adding the discovery to the result
    res <- c(res,rr);
    #
    # extracting the positive constants
    for (ii in bf(rr)) { if (rr[ii] >= how) {
        uuu <- names(rr)[ii];
        toto <- paste("val <- ",uuu,";",sep="");
        eval(parse(text=toto));
        re2[[uuu]] <- list(definition = chaine[ii],value=val);
    }}
}
#
# printing
if (imp) {
    for (ii in bf(re2)) {
        form3title(names(re2)[ii],laft=0);
        if (expr3present("d",what)) {
             form3paragraphe(re2[[ii]]$definition);
        }
        if (expr3present("v",what)) {
            print(re2[[ii]]$value);
        }
    }
    return(invisible());
}
#
# returning the values
re2;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
