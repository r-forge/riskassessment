
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rs003k <- function(whi)
#TITLE (00) assigns the constants for the rs00 layer
#DESCRIPTION
# defines or returns the constants used within /rbsb00/. 
# The performed action depends on the argument.
#DETAILS
# All constant names start with 'rbsb.'.
# This solution was adopted to replace
# a set of global constants that I had difficulty
# to make acceptable with R packages standards.
# It is recommended not to modify these constants
# unless you are completely aware of the consequences.\cr
# The constants can be any object type.
#PKEYWORDS helpful
#KEYWORDS misc
#INPUTS
#{whi}    <<a character(1) indicating either to reset or
#           to return the names or the current values. The three
#           values are \code{RESET}, \code{reset}, \code{names}, \code{definitions} or \code{values}.>>
# >>
#[INPUTS]
#VALUE
# When \code{whi=="RESET"} nothing (but the assignments are
# performed for all layers: only \code{rs00} since it is the first one.).
# When \code{whi=="reset"} nothing (but the assignments of 
# the layer \code{rs00} are performed).
# When \code{whi=="names"} the names as a character vector.
# When \code{whi=="definitions"} the definitions as a named character vector.
# When \code{whi=="values"} the values through a named list.
#EXAMPLE
## First assign the standard values
# rs003k("reset");
# print(rbsb.msi);
## to get the short labels
# rs003k("names");
## to obtain the current values
# rs003k("values");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_09_16
#REVISED 10_04_01
#--------------------------------------------
{
# checking
if (!expr3present(whi,c("RESET","reset","names","definitions","values"))) {
    print.default(whi);
    stop("rs003k does not accept this argument");
}
#
#if (whi=="RESET") { }
#
# definition of the different constants
sc <- character(0);
sc["msi"] <- "The signature of rebastaba";
sc["min"] <- "Number of spaces for indentation";
sc["mba"] <- "Must batch be activated (= no pause after displaying a result)?";
sc["mck"] <- "Must systematic checks be done?";
sc["mwa"] <- "Must warning be simple warning?";
sc["mfa"] <- "Must fatal error be fatal?";
sc["mwi"] <- "The width (nbr of characters) for printing paragraphs";
sc["mfi"] <- "Must the results be directed to files (if not to the screen)?";
sc["mgr"] <- "Type of graphics files";
sc["mnd"] <- "Number of decimals when printing";
sc["mep"] <- "When printing an object: *e*m*p*hasize level";
sc["ffg"] <- "Last number of the graphics files";
sc["fpx"] <- "Prefix for the resulting files";
sc["fou"] <- "Standard file for text outputs";
sc["cpt"] <- "Different closing parentheses as a dimnamed matrix";
sc["cni"] <- "character(1) to designate the node under consideration";
sc["cac"] <- "character(1) to indicate something to be computed";
sc["log0"] <- "Null value for logical objects";
sc["num0"] <- "Null value for numeric objects";
sc["cha0"] <- "Null value for character objects";
sc["lis0"] <- "Null value for list objects";
sc["fun0"] <- "Null value for function objects";
sc["dfr0"] <- "Null value for data.frame objects";
sc["dfr1"] <- "Example1 of data.frame object";
sc["des0"] <- "Null value for des objects";
sc["fau0"] <- "Null value for faux objects";
sc["fau1"] <- "Example 1 of faux object";
sc["daf0"] <- "Null value for daf objects";
sc["smn"] <- "Minimum number of observations to compute statistics";
sc["sna"] <- "The different natures for random variates";
sc["spr"] <- "The different natures for random variates";
sc["snp"] <- "Properties of the different natures of random variates as a dimnamed matrix";
sc["vma"] <- "Different types of vma (see char2vma for details)";
sc["sep0"] <- "Basic string separator";
sc["sep1"] <- "Second string separator";
#
# returning the names
#
if (whi=="names") { return(names(sc));}
#
# returning the definitions
#
if (whi=="definitions") { return(sc);}
#
# returning the values
#
if (whi=="values") {
    res <- vector("list",0);
    for (ii in sjl(sc)) {
        noco <- names(sc)[[ii]];
        eee <- paste("res[[\"",noco,"\"]] <- rbsb.",noco,";",sep="");
        eval(parse(text=eee));
    }
    return(res);
}
#
# loading the standard values
#
if (tolower(whi)=="reset") {
#
    assign("rbsb.msi","/rbsb0.1-9/",pos=".GlobalEnv");
    assign("rbsb.min",3,pos=".GlobalEnv");
    assign("rbsb.mba", TRUE,pos=".GlobalEnv");
    assign("rbsb.mfa", TRUE,pos=".GlobalEnv");
    assign("rbsb.mck", TRUE,pos=".GlobalEnv");
    assign("rbsb.mwa", TRUE,pos=".GlobalEnv");
    assign("rbsb.mwi",  70,pos=".GlobalEnv");
    assign("rbsb.mfi",TRUE,pos=".GlobalEnv");
    assign("rbsb.mgr","pdf",pos=".GlobalEnv");
    assign("rbsb.mnd",3,pos=".GlobalEnv");
    assign("rbsb.mep",1,pos=".GlobalEnv");
    assign("rbsb.ffg",0,pos=".GlobalEnv");
    assign("rbsb.fpx",paste("rbsb",format(Sys.time(), "%y_%m_%d"),sep="."),pos=".GlobalEnv");
    assign("rbsb.fou",paste(rbsb.fpx,"txt",sep="."),pos=".GlobalEnv");
    # due to the use of easyp3cut, no nesting parenthesis are allowed!
    # also opening and closing must be different
    assign("rbsb.cpt", matrix(c("{{","}}",
                                                     "(|","|)",
                                                     "[" ,"]" ,
                                                     "<<",">>"),ncol=2,
                                  byrow=TRUE,dimnames=list(c("nodes","rounding","variables","vectors"),c("opening","closing"))),pos=".GlobalEnv");
    assign("rbsb.cni", "*Y*",pos=".GlobalEnv");
    assign("rbsb.cac", "acalculer",pos=".GlobalEnv");
    assign("rbsb.log0", logical(0),pos=".GlobalEnv");
    assign("rbsb.num0", numeric(0),pos=".GlobalEnv");
    assign("rbsb.cha0", character(0),pos=".GlobalEnv");
    assign("rbsb.lis0",    vector("list",0),pos=".GlobalEnv");
    assign("rbsb.fun0", function(...){invisible()},pos=".GlobalEnv");
    #assign("rbsb.dfr0",data.frame(matrix(NA,5,3)),pos=".GlobalEnv");
    assign("rbsb.dfr0", as.data.frame(matrix(0,0,0)),pos=".GlobalEnv");
    assign("rbsb.dfr1", data.frame(F1=factor(rep(1:3,each=4)),F2=factor(rep(1:4,3))),pos=".GlobalEnv");
    assign("rbsb.des0", new("des"),pos=".GlobalEnv");
    assign("rbsb.fau0", new("faux"),pos=".GlobalEnv");
    assign("rbsb.fau1", new("faux",orig="ici",defi="Fatal Error",comm=c("In fact it was a joke","No error occurred!","You must be relieved")),pos=".GlobalEnv");
    assign("rbsb.smn", 30,pos=".GlobalEnv");
    assign("rbsb.sna",c("conti","integ","cateo","categ","unkno"),pos=".GlobalEnv");
    assign("rbsb.spr",c("categoric","ordered","numeric"),pos=".GlobalEnv");
    assign("rbsb.snp", 
          matrix(c(FALSE,FALSE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE),5,3,dimnames=list(c("conti","integ","cateo","categ","unkno"),c("categoric","ordered","numeric"))
             ),pos=".GlobalEnv");
    assign("rbsb.daf0",new("daf",des=new("des"),
                                                 what="d",valu="rbsb.dfr0"),pos=".GlobalEnv");
    vma <- c("c","C","v","V","u","m","n","o","p","M","N","O","P","a","A");
    names(vma) <- vma;
    assign("rbsb.vma",vma,pos=".GlobalEnv");
    assign("rbsb.sep0"," ",pos=".GlobalEnv");
    assign("rbsb.sep1","//",pos=".GlobalEnv");
}
#
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

###########################################
###########################################
########
#((((((( NEW S4 CLASS des
########
###########################################
###########################################

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8des <- function(object)
#TITLE (00) checks a /des/
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
#REVISED 09_08_31
#--------------------------------------------
{
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
    orig=paste("Created by rs00"),
    time="unknown",
    defi="undefined",
    role=character(0),
    comm=character(0)),
                validity=valid8des
        );
#

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8des <- function(x,...,quoi="ndr",empha=1)
#TITLE (00) prints a des object
#DESCRIPTION
#   This function prints in a interpreted way a /des/ object.
#DETAILS
# Global constant rbsb.mwi is used to justify the paragraphes.
# It is the generic print for /des/ objects.
#KEYWORDS classes
#INPUTS
#{x} <<The ds object to print.>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.>>
#{quoi} <<(="dr") the fields to print:
#          a=all fields,
#          n=name,
#          d=definition,
#          o=origin,
#          t=time,
#          r=role,
#          c=comments.>>
#{empha} <<(=1) Emphasize level of printing;
#           between -2 and 5>>
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
#REVISED 08_10_15
#--------------------------------------------
{
# some checking
che <- valid8des(x);
if (!identical(che,TRUE)) {
    erreur(che,"/des/ is not valid");
}
empha <- round(max(-2,min(5,empha)));
quoi <- tolower(quoi);
if (expr3present("a",quoi)) { quoi <- "ndortc";}
# preparing the titles
tt <- c("Name","Definition","Origin","Time","Role","Comment(s)");
# preparing the constant according to the emphasis
if (empha == -2) {
    sep=0; ed <- form3repete(" ",2*empha);
    nbs <- 5; sou <- " ";
}
if (empha == -1) {
    sep=0; ed <- form3repete(" ",2*empha);
    nbs <- 5; sou <- " ";
}
if (empha == 0) {
    sep=0; ed <- form3repete(" ",2*empha);
    nbs <- 5; sou <- " ";
}
if (empha == 1) {
    sep=0; ed <- form3repete(" ",2*empha);
    nbs <- 5; sou <- "-";
}
if (empha == 2) {
    sep=1; ed <- form3repete(" ",2*empha);
    nbs <- 5; sou <- "==";
}
if (empha == 3) {
    sep=1; ed <- form3repete(" ",2*empha);
    nbs <- 20; sou <- "==";
}
if (empha == 4) {
    sep=2; ed <- form3repete(" ",2*empha);
    nbs <- 2; sou <- form3repete(" ",50,FALSE,TRUE);
}
if (empha == 5) {
    sep=2; ed <- form3repete(" ",2*empha);
    nbs <- 3; sou <- form3repete("=",50,FALSE,TRUE);
}
#
# printing
if (expr3present("n",quoi)) {
if (!isempty(x@name)) {
if (x@name != "") {
    form3titre(c(tt[1],x@name),empha);
}}}
if (expr3present("d",quoi)) {
if (!isempty(x@defi)) {
if (x@defi != "") {
    if (!isempty(x@name)) {
        tt[2] <- paste("<",tt[2]," of '",x@name,"':>",sep="");
    }
    form3paragraphe(c(tt[2],x@defi),empha,
                    wid=rbsb.mwi,sep=sep,ed=ed);
}}}
if (expr3present("o",quoi)) {
if (!isempty(x@orig)) {
if (x@orig != "") {
    form3paragraphe(c(tt[3],x@orig),empha,
                    wid=rbsb.mwi,sep=sep,ed=ed);
}}}
if (expr3present("t",quoi)) {
if (!isempty(x@time)) {
if (x@time != "") {
    form3paragraphe(c(tt[4],x@time),empha,
                    wid=rbsb.mwi,sep=sep,ed=ed);
}}}
if (expr3present("r",quoi)) {
if (!isempty(x@role)) {
if (x@role != "") {
    form3paragraphe(c(tt[5],x@role),empha,
                    wid=rbsb.mwi,sep=sep,ed=ed);
}}}
if (expr3present("c",quoi)) {
if (!isempty(x@comm)) {
if ((x@comm[1] != "")|(length(x@comm)>1)) {
    form3paragraphe(c(tt[6],x@comm),max(0,empha),
                    wid=rbsb.mwi,sep=sep,ed=ed);
}}}
form3repete(sou,nbs,TRUE);
# returning
invisible();
}

setMethod("print",signature(x = "des"), print8des);


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#
#


###########################################
###########################################
########
#((((((( NEW S4 CLASS faux
########
###########################################
###########################################


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8faux <- function(object)
#TITLE (00) checks a /faux/
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
#REVISED 09_09_30
#--------------------------------------------
{
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
print8faux <- function(x,...,print=2)
#TITLE (00) prints a faux object
#DESCRIPTION
#   This function prints in a interpreted way a /faux/ object.
#DETAILS
# Global constant rbsb.mwi is used to justify the paragraphes.
# It is the generic print for /faux/ objects.
#KEYWORDS error
#INPUTS
#{x} <<The faux object to be printed.>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.>>
#{print} <<(=1) the level of printing
#          0= only the definition,
#          1= the origin and the definition
#          2= everything.
#VALUE
# nothing but a print is performed
#EXAMPLE
# rs003k("RESET"); # needed only for R checking, to be forgotten
# print(rbsb.fau1,print=2);
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
quoi <- "ndo";
if (print==0) { quoi <- "d";}
if (print==2) { quoi <- "a";}
#
form3titre(paste("/faux/ with a level of",x@level));
print(as(x,"des"),quoi=quoi);
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
#TITLE (00) checks a /daf/
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
#REVISED 09_10_01
#--------------------------------------------
{
    res <- character(0);
    # checking the 'what' parameter
    #check4tyle(object@what,"character",1,"For /df/ Slot 'what' must be character(1)");
    if (!(object@what %in% c("t","d","f","c","c2"))) {
        res <- c(res,paste(object@what,"'what' must be 't', 'd' or 'f'"));
    }
    # checking the other fields
    # to avoid error whe R is checking the package
  if (!is.null(rbsb.snp)) {  # to prevent R checking harassment
    rres <- NULL;   # to prevent R checking harassment
    #check4tyle(object@valu,"character",1,"For /df/ Slot 'valu' must be character(1)");
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
print8daf <- function(x,whi=1:10,quoi="nr")
#TITLE (00) prints a daf object
#DESCRIPTION
#   This function prints in a daf object.
#DETAILS
#KEYWORDS print
#PKEYWORDS daf
#INPUTS
#{x} <<The daf object.>>
#[INPUTS]
#{quoi} <<(="nr"), which part of the description to print
#                  (see print.ds).>>
#{whi} <<(=1:10), which rows to print?\cr
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
# rs003k("RESET"); # needed only for R checking, to be forgotten
# print(new("daf",des=new("des",name="rbsb",
#                                 orig=paste("Created by rebastaba"),
#                                 time=date(),
#                                 defi="prototype",
#                                 role=character(0),
#                                 comm=character(0)),
#                         what="d",
#                         valu="rbsb.dfr0"));
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
    check4tyle(quoi,"character",1,"'quoi' must be character(1)");
}
#
nsou <- 39;
# dealing with the daf.null case
if (identical(x,rbsb.daf0)) {
    form3repete("-",nsou,TRUE);
    cat("<<< daf.null >>>\n");
    form3repete("-",nsou,TRUE);
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
        quelles <- sj(nlig);
    } else {
        if (length(whi) == 1) {
            nli <- min(100,whi);
            quelles <- seq(1,nlig,length=max(1,round(nlig*nli/100)));
            quelles <- round(quelles);
        } else {
            quelles <- intersect(whi,sj(nlig));
            if (length(quelles) <= 0) { quelles <- 1:min(10,nlig);}
        }
    }
}
# printing the description
form3repete("-",nsou,TRUE);
print(x@des,quoi);
#printing the values
form3repete("-",nsou,TRUE);
if (length(quelles)>0) {
    print(values[quelles,]);
}
# returning
form3repete("-",nsou,TRUE);
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "daf"), print8daf);


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sj <- function(nb)
#TITLE (00) trick for loops
#DESCRIPTION
# returns 1:nb when nb >0 and
#         numeric(0) otherwise.
# Quite useful to prevent starting
# a loop of length nought
#DETAILS
#PKEYWORDS helpful
#KEYWORDS iteration
#INPUTS
#{nb}    <<integer>>
#[INPUTS]
#VALUE
# 1:nb if nb > 0
# else numeric(0)
#EXAMPLE
# sj(0);
# sj(5);
#REFERENCE
#SEE ALSO sjl
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_18
#REVISED 09_04_05
#--------------------------------------------
{
if (is.null(nb)) {return(numeric(0));}
if (length(nb)!=1) {
    erreur(nb,"sj deals only with scalar nb");
}
if (nb > 0) {return(1:max(1,round(nb)));}
numeric(0);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sjl <- function(x)
#TITLE (00) trick for loops
#DESCRIPTION
# returns 1:length(x) when length(x) >0 and
#         numeric(0) otherwise.
# Quite useful to prevent starting
# a loop of length nought
#DETAILS
#PKEYWORDS helpful
#KEYWORDS iteration
#INPUTS
#{x}    <<vector>>
#[INPUTS]
#VALUE
# 1:length(x) if length(x) > 0
# else numeric(0)
#EXAMPLE
# sjl(0);
# sjl(5);
# sjl(character(0));
# sjl(letters);
#REFERENCE
#SEE ALSO sj
#CALLING
#COMMENT
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
form3liste <- function(vcara,none="-",
                       OPA="{",CPA="}",
                       opa="(",cpa=")",sep="+",
                       imp=FALSE,cr=imp)
#TITLE (00) formats a series of names
# or whatever
#DESCRIPTION returns a scalar character of the names
#  surrounded by any kind of parenthesis and
#  separated with the same separator.
#DETAILS
#PKEYWORDS format
#KEYWORDS print
#INPUTS
#{vcara}<<Character vector to be considered.>>
#[INPUTS]
#{none}<< The internal result if vcara is length zero.>>
#{OPA}<< The opening parenthesis to surround the entire list.>>
#{CPA}<< The closing parenthesis to surround the entire list.>>
#{opa}<< The opening parenthesis to surround each name.>>
#{cpa}<< The closing parenthesis to surround each name.>>
#{sep}<< The symbol to separate each name.>>
#{imp}<< Must the result be printed (with cat) or returned?>>
#{cr}<< Must a line feed be added?>>
#VALUE
# A character string or nothing when imp is TRUE
#EXAMPLE
# form3liste(letters[1:4])
#REFERENCE
#SEE ALSO form3etsil
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_06_25
#REVISED 08_10_30
#--------------------------------------------
{
vcara <- as.character(vcara);
if (length(vcara) == 0) { res <- none;
} else {
    for (hd in sjl(vcara)) {
        hdc <- paste(opa,vcara[hd],cpa,sep="");
        if (hd == 1) { res <- hdc;
        } else { res <- paste(res,sep,hdc,sep="");}
    }
}
res <- paste(OPA,res,CPA,sep="");
if (cr) { res <- paste(res,"\n",sep="");}
if(!imp) { return(res);}
cat(res);
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3etsil <- function(cara,none="-.-",
                       OPA="{",CPA="}",
                       opa="(",cpa=")",sep="+")
#TITLE (00) inverse function of form3liste
# or whatever
#DESCRIPTION returns a vector character of the names
#  from a character string generated by form3liste. For
# the moment, sep cannot be an empty string.\cr
# Of course, it is implicetly supposed that the
# inversion is unambiguous.
#DETAILS
# The consistence of the different parenthesis and 
# separators is checked: an error is issued when
# they does not correspond.\cr
# Important: the possible \\n added by form3liste
# is not taken into account and must be removed
# before calling this function.
#PKEYWORDS format
#KEYWORDS print
#INPUTS
#{cara}<<Character to be considered.>>
#[INPUTS]
#{none}<< idem as in form3liste.>>
#{OPA}<< idem as in form3liste.>>
#{CPA}<< idem as in form3liste.>>
#{opa}<< idem as in form3liste.>>
#{cpa}<< idem as in form3liste.>>
#{sep}<< idem as in form3liste.>>
#VALUE
# A character vector or nothing when imp is TRUE
#EXAMPLE
# uu <- form3liste(letters[1:4]);
# form3etsil(uu);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_10_30
#REVISED 08_10_30
#--------------------------------------------
{
# checking
check4tyle(cara,"character",1);
# removing the external braces
n1 <- nchar(OPA); n2 <- nchar(CPA);
if (n1 > 0) {
    ax <- substr(cara,1,n1);
    if (ax!=OPA) {
        erreur(c(OPA,ax),"Non consistent OPA and cara arguments.");
    }
    cara <- substr(cara,1+n1,nchar(cara));
}
if (n2 > 0) {
    nn <- nchar(cara);
    ax <- substr(cara,nn-n2+1,nn);
    if (ax!=CPA) {
        erreur(c(CPA,ax),"Non consistent OPA and cara arguments.");
    }
    cara <- substr(cara,1,nn-n2);
}
# removing the separator and constituting the vector
if (cara==none) { res <- character(0);
} else {
    # splitting
    if (nchar(sep)==0) {
        erreur(cara,"Sorry by the function does not act properly for empty separators");
    }
    res <- strsplit(cara,sep,fixed=TRUE)[[1]];
    # removing the internal braces
    n1 <- nchar(opa); n2 <- nchar(cpa);
    for (hd in sjl(res)) {
        rr <- res[hd];
        if (n1 > 0) {
            ax <- substr(rr,1,n1);
            if (ax!=opa) {
                erreur(c(opa,ax),"Non consistent opa and cara arguments.");
            }
            rr <- substr(rr,1+n1,nchar(rr));
        }
        if (n2 > 0) {
            nn <- nchar(rr);
            ax <- substr(rr,nn-n2+1,nn);
            if (ax!=cpa) {
                erreur(c(cpa,ax),"Non consistent cpa and cara arguments.");
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
expr3extrait <- function(cara,opa="(",cpa=")")
#TITLE (00) extracts the contents of parentheses from a character
#DESCRIPTION returns a vector character with the 
#  contents of different parentheses.
#DETAILS
# Parenthesis are defined with 'opa' and 'cpa' arguments
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{cara}<<Character to be considered.>>
#[INPUTS]
#{opa}<< opening tag.>>
#{cpa}<< closing tag.>>
#VALUE
# A character vector or nothing when there is no parentheses
#EXAMPLE
# rs003k("reset"); # for R checking convenience
# expr3extrait('avant (8h) ce n_est pas l_heure, plus tard que (9h) non plus')
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_12_17
#REVISED 09_03_06
#--------------------------------------------
{
i.cara <- cara
# checking
check4tyle(opa,"character",c(1,Inf));
check4tyle(cpa,"character",c(1,Inf));
# extracting
res <- character(0);
essaie <- TRUE;
jbd <- 0;
while (essaie) {
    inu <- strsplit(cara,opa,fixed=TRUE)[[1]][1];
    # this double check about inu is strange
    if (is.na(inu)) { essaie <- FALSE;
    } else {
      if (inu==cara) { essaie <- FALSE;
      } else {
          cara <- form3decadre(cara,paste(inu,opa,sep=""),"");
          uti <- strsplit(cara,cpa,fixed=TRUE)[[1]][1];
          if (is.na(uti)) {
              erreur(list(i.cara,opa,cpa),"Non accepted case: wrong syntax of your input? Most common's a bad stopping tag");
          }
          if (uti==cara) { essaie <- FALSE;
          } else {
              jbd <- jbd + 1;
              res[jbd] <- uti;
              cara <- form3decadre(cara,paste(uti,cpa,sep=""),"");
          }
      }
    }  
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
expr3present <- function(sch,ch,exact=FALSE)
#TITLE (00) does sch belongs to ch?
#DESCRIPTION
# When \code{exact} is FALSE, returns TRUE if the 
# character string sch is included at least one
# time into the character string ch.\cr
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{sch} <<(\code{character(1)}) the character string to be found.>>
#{ch}  <<(\code{character}) the character string(s) to investgate.>>
#[INPUTS]
#{exact} <<(=FALSE) When exact, one component must
# be strictly identical, if not a subtring is sufficient.
#VALUE TRUE or FALSE
#EXAMPLE
# expr3present('a','non');
# expr3present('o',c('non','oui'));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_08_28
#REVISED 09_09_21
#--------------------------------------------
{
# checking is not conditionned with rbsb.mck
# since this function is called by rs003k which
# defines this constant!
#
check4tyle(sch,"character", 1);
check4tyle( ch,"character",-1);
#
if (exact) {
    res <- sum(sch==ch) > 0;
} else {
    res <- length(grep(sch,ch)) > 0;
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3justifie <- function(chaine,
                          nbc=8,
                          format=3,
                          tronc=TRUE,
                          carac=" ")
#TITLE (00) formats a character string
#DESCRIPTION
# The aim of this function is to produce
# aligned columns lists while printing
# the rows not a the same time.
#DETAILS
#PKEYWORDS
#KEYWORDS print
#INPUTS
#{chaine}<<the character string to be printed>>
#[INPUTS]
#{nbc} << Desired number of characters for the result>>
#{format} << Indicates the type of alignment:\cr
#   0 no aligment (no supplementary character added to reach nbc)\cr
#   1 to the left side\cr
#   2 centered\cr
#   3 to the right side>>
#{tronc} << If true, no more than
#     "nbc" characters are returned and
# possibly the string is truncated. In that
# case, $ is added for indicating the fact.>>
#{carac} << Character to use for enlarging the string>>
#VALUE a character string
#EXAMPLE
# form3justifie("vers")
# form3justifie("versification",5)
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 1999_05_25
#REVISED   09_09_22
#--------------------------------------------
{
# to forgive to form3paragraphe
if (length(nbc) != 1) { nbc <- 8;}
if (nbc < 2) { nbc <- 8;}
# checking
check4tyle(chaine,c("character","numeric"),c(0,1));
if (length(chaine)==0) { chaine ="";}
#
itr <- "$"; # truncation indicator
res <- chaine;
# truncation
if ( (nchar(res) > nbc) & tronc ) {
 if (format <= 1) {
  res <- substring(chaine,1,nbc);
  res <- paste(res,itr,sep="");
  }
 else {
  if (format == 2) {
   otg <- (nchar(chaine) - nbc) %/% 2;
   res <- substring(chaine,1+otg);
   res <- substring(res,1,nbc);
   res <- paste(itr,res,itr,sep="");
   }
  else {
   res <- substring(chaine,1+nchar(chaine)-nbc,
                    nchar(chaine));
   res <- paste(itr,res,sep="");
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
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3repete <- function(cha="-",nb=10,imp=FALSE,cr=imp)
#TITLE (00) prints a repeated given string
#DESCRIPTION
# Without adding breaking line characters, prints
# "nb" times a given string of characters
#DETAILS
#PKEYWORDS
#KEYWORDS print
#INPUTS
#[INPUTS]
#{cha} << The string to repeat>> 
#{nb} << Number of repetitions>> 
#{imp} << Printing when TRUE or returning (default)>>
#{cr} << Must a line feed be added?>>
#VALUE
# character string or printing according to imp
#EXAMPLE
# form3repete('-+',20,TRUE)
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
for (jbd in sj(nb)) { res <- paste(res,cha,sep="");}
if (cr) { res <- paste(res,"\n",sep="");}
if(!imp) { return(res);}
cat(res);
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3encadre <- function(chaine,bef="(*)_",aft="",imp=FALSE,cr=imp)
#TITLE (00) surrounds a character string
#DESCRIPTION
# Adds before and after some characters to a character string
#DETAILS
#PKEYWORDS
#KEYWORDS print
#INPUTS
#{chaine} <<The character string to frame.>>
#[INPUTS]
#{bef} << What to add before>> 
#{aft} << What to add after>> 
#{imp} << Printing when TRUE or returning (default)>>
#{cr} << Must a line feed be added?>>
#VALUE
# character string or printing according to imp
#EXAMPLE
# form3encadre('IMPORTANT','<<< ',' >>>');
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_08_21
#REVISED 08_08_21
#--------------------------------------------
{
res <- paste(bef,chaine,aft,sep="");
if (cr) { res <- paste(res,"\n",sep="");}
if(!imp) { return(res);}
cat(res);
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3decadre <- function(chaine,bef=" ",aft=bef,mxm=Inf)
#TITLE (00) remove character before and after a character string
#DESCRIPTION
# remove 'bef's before and 'aft's after a character string.
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{chaine} <<The character string to refine. 
#           Must be of length one.>>
#[INPUTS]
#{bef} << What to repeatedly remove before.>> 
#{aft} << What to repeatedly remove after.>>
#{mxm} << Maximum number of characters to be removed.>>
#VALUE
# character string after removing
#EXAMPLE
# rs003k("RESET"); # needed only for R checking, to be forgotten
# form3decadre('IMPORTANT','IM',' ANT');
# form3decadre('   OUF ',' ','',1);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_12_10
#REVISED 10_04_07
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(chaine,"character",c(0,1),"form3decadre: Vector are not accepted for 'chaine'");
    check4tyle(bef,"character",1,"form3decadre: Vector are not accepted for 'bef'");
    check4tyle(aft,"character",1,"form3decadre: Vector are not accepted for 'aft'");
    check4tyle(mxm,"numeric",1,"form3decadre: mxm must be numeric(1)");
}
# null case
if (length(chaine) == 0) { return(chaine);}
# removing at the beginning of the string
lb <- nchar(bef);
if (lb>0) {
    nbr <- 0;
    repeat {
        deb <- substr(chaine,1,lb);
        if ((deb == bef) & (nbr < mxm)) {
            chaine <- substring(chaine,lb+1);
            nbr <- nbr+1;
        } else { break;}
    }
}
# removing at the end of the string
la <- nchar(aft);
if (la>0) {
    nbr <- 0;
    repeat {
        lc <- nchar(chaine);
        fin <- substr(chaine,lc-la+1,lc);
        if ((fin == aft) & (nbr < mxm)) {
            chaine <- substring(chaine,1,lc-la);
            nbr <- nbr+1;
        } else { break;}
    }
}
# returning
chaine;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3ind <- function(niv=1,cr=TRUE,ele=" ")
#TITLE (00) provides indentation of different levels
#DESCRIPTION
# The level of indentation is given by \code{niv*rbsb.min} times
# the character ele. When cr, a line feed is made at first.
#DETAILS
#PKEYWORDS
#KEYWORDS print
#INPUTS
#[INPUTS]
#{niv} << Level of indentation.>>
#{cr} << Must a line feed provided first?>>
#{ele} << String to use.>>
#VALUE
# a scalar string
#EXAMPLE
# rs003k("RESET"); # needed only for R checking, to be forgotten
# cat(form3ind(2),"Bien `a vous","\n");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_10
#REVISED 08_09_10
#--------------------------------------------
{
niv <- round(max(min(8,niv),0));
res <- "";
if (cr) { res <- paste(res,"\n",sep="");}
res <- paste(res,form3repete(ele,niv*rbsb.min),sep="");
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3names <- function(nbn,nom=character(0),prefix="",
                           upca=TRUE,nume=14)
#TITLE (00) provides systematic names for nodes
#DESCRIPTION
# Provides systematic names for nodes according the 
# number of nodes taking care of not duplicating
# former nodes.
#DETAILS (see the code)
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{nbn} <<Number of new node names to generate>>
#[INPUTS]
#{nom} << Already present names (to avoid identical names).>>
#{prefix} << Systematic prefix of all names to generate. Must
#                 comprise the dot, if one wants such a separator
#                 between it and the specific part of the name. 
#                 Of course can be 'underscore' or whatever else.>>
#{upca} << Indicates whether the letters constituting the new
#          names must be uppercase or not.>>
#{nume} << Its absolute value gives the number of the letter to use
#          when Letters are not sufficient. When negative, alphabet
#          is not considered as a first possibility.>>
#VALUE
# vector with nbn different strings associated to new names
#EXAMPLE
# rs003k("RESET"); # needed only for R checking, to be forgotten
# form3names(2);
# form3names(2,nume=-3);
# form3names(2,prefix="rbsb.");
# form3names(2,upca=FALSE);
# form3names(2,"Z");
# form3names(30);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_19
#REVISED 10_02_15
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(upca,"logical",1,"Argument 'upca' not accepted");
    check4tyle(nume,"integer",1,"Argument 'nume' not accepted");
    if ((abs(nume)<1) | (abs(nume)>26)) {
        erreur(nume,"'nume' must be comprised between 1 and 26 to indicate a Letter");
    }
}
#
if (upca) { Letters <- LETTERS;
} else { Letters <- letters;}
#
if (isempty(nbn)) { return(character(0));}
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
    if ((nbn < (27-mama)) & (nume>0)) {
        # adding letters 
        res <- Letters[mama+(1:nbn)];
    } else {
        # adding numbered nodes
        ajou <- 0; nu <- 1; res <- character(0);
        while ( ajou < nbn ) {
          nono <- paste(Letters[abs(nume)],nu,sep="");
          if (all(nono != nom)) {
              ajou <- ajou + 1;
              res <- c(res,nono);
          }
          nu <- nu+1;
        }
    }
}
# adding the prefix
res <- paste(prefix,res,sep="");
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3line <- function(len=50,pat="-_-",wid=3,
                      gind="",pat2="_-_",
                      imp=TRUE)
#TITLE (00) print a separator line from a given pattern
#DESCRIPTION
# The line can be composite with the width arguement.
# The pattern can comprise more than one character.
# General indentation is possible (gind).
# Even lines can have a different pattern(pat2).
#DETAILS
# contrary to form3repete 'backslash'n are introduced including
# at the end.
#PKEYWORDS
#KEYWORDS print
#INPUTS
#[INPUTS]
#{len} << Line length (without the general indentation.>> 
#{pat} << Pattern to use.>> 
#{wid} << Number of elementary lines.>> 
#{gind} << Pattern to use for the general indentation.>> 
#{pat2} << Pattern to use for the even lines.
#         If NULL, the pattern is common to all lines.>> 
#{imp} << Printing when TRUE or returning (FALSE)>>
#VALUE
# character string or printing according to imp
#EXAMPLE
# form3line();
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_07_31
#REVISED 08_07_31
#--------------------------------------------
{
if (is.null(pat2)) { pat2 <- pat; }
if (wid <= 0) { res <- character(0);
} else {
    res <- "";
    if (nchar(pat)<=0) { pat <- "-";}
    nbp <- ceiling((len*wid)/nchar(pat));
    ch1 <- paste(rep(pat, nbp),collapse="");
    ch2 <- paste(rep(pat2,nbp),collapse="");
    po1 <- po2 <- 1;
    for (jbd in 1:wid) {
        if ((jbd %% 2) == 0) {
            li <- substr(ch2,po2,po2+len-1);
            po2 <- po2 + len;
        } else {
            li <- substr(ch1,po1,po1+len-1);
            po1 <- po1 + len;
        }
        res <- paste(res,gind,li,"\n",sep="");
    }
}
if(!imp) { return(res);
} else {cat(res);}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3file <- function(chaine,file="",append=TRUE)
#TITLE (00) adds a character vector into a file
#DESCRIPTION
# The aim of this function is generate files
# from a character vector. In the framework
# of rebastaba, the production of files
# to be dealt by external programs.
#DETAILS
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{chaine}<<the character string to be output>>
#[INPUTS]
#{file} <<("") file where to write it down.>>
#{append} << Indicates if appending or not.>>
#VALUE nothing but the file is written
#EXAMPLE
# form3file(letters,"useless.txt")
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_02_13
#REVISED 08_02_13
#--------------------------------------------
{
if (length(chaine) > 0) {
    if (file!="") {sink(file=file,append=append);}
    for (jbd in 1:length(chaine)) {
        cat(chaine[jbd],"\n",sep="");
    }
    if (file!="") {sink();}
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3titre <- function(tit,empha=2,indent=2+2*empha,imp=TRUE)
#TITLE (00) prints or prepares a title
#DESCRIPTION
# prints a character string "tit"
# with more or less emphasis
#DETAILS
#PKEYWORDS
#KEYWORDS print
#INPUTS
#{tit}<<the title to print>>
#[INPUTS]
#{empha} << Level of emphasize.\cr
#         -1: single line without carriage return\cr
#          0: single line\cr
#          1: underlined\cr
#          2: underlined and overlined\cr
#          3: 2 + 1 line before\cr
#          4: 3 + 1 line after\cr
#          5: 2 + 2 lines before and after>>
#{indent} << Number of spaces to introduce before the title>>
#{imp} << Printing is performed and nothing is returned.
#                If FALSE, the character string is returned 
#                (including possible new lines)>>
#VALUE
# either nothing or a character string according to imp
#EXAMPLE
# form3titre("Some Title");
#REFERENCE
#SEE ALSO
#CALLING {form3repete}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_24
#REVISED 10_03_08
#--------------------------------------------
{
empha <- round(max(-1,min(6,empha)));
if (empha == -1) { tit <- paste("<",tit,">",sep="")}
if (empha == 0) { tit <- paste("(*)",tit,"(*)",sep="")}
long <- nchar(tit);
# indentation
if (indent > 1) { bb <- form3repete(" ",round(indent));
} else { bb <- "";}
if (empha < 0) { titi <- paste(bb,tit," ",sep="");
} else { titi <- paste(bb,tit,"\n",sep="");}
# line
lin <- paste(bb,form3repete("=",long),"\n",sep="");
# assembling
if (empha ==-1) { res <- titi;}
if (empha == 0) { res <- titi;}
if (empha == 1) { res <- paste(titi,lin,sep="");}
if (empha == 2) { res <- paste(lin,titi,lin,sep="");}
if (empha == 3) { res <- paste("\n",lin,titi,lin,sep="");}
if (empha == 4) { res <- paste("\n",lin,titi,lin,"\n",sep="");}
if (empha == 5) { res <- paste("\n\n",lin,titi,lin,"\n\n",sep="");}
if (empha == 6) {
    res <- paste("\n\n",
                 bb,"+",form3repete("-",long+2),"+\n",
                 bb,"| ",tit," |\n",
                 bb,"+",form3repete("-",long+2),"+\n",
                 "\n\n",
                 sep="");
}
# returning
if (!imp) { return(res);}
cat(res,sep="");
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3paragraphe <- function(texte,titre=-1,
                            wid=60,fli=NULL,sep=1,
                            jus=1,tronc=TRUE,
                            ed="  ",ef="",
                            imp=TRUE)
#TITLE (00) prints or prepares paragraphes
#         from a character vector.
#         Each component is supposed to be a
#         paragraph but the first one can be
#         considered as a title.
#DESCRIPTION
# prints or prepares a character string "texte"
# as a small formatted text.
#DETAILS
#PKEYWORDS
#KEYWORDS print
#INPUTS
#{texte}<<The text to print (character vector).>>
#[INPUTS]
#{titre} << When > -2 indicates that the first
#           component is a title (if not a
#           simple paragraph). Then the value
#           of titre gives the emphasize to put
#           on the title. Notice that the title is
#           not spletted in several lines as are
#           the other components according to 'wid'
#           value.>>
#{wid} << The width (in characters) without
#        including indentation and frames.>>
#{fli} << When NULL, the first line of
#        each paragraph (in fact the second if there is 
#        a title) is issued as a standard line. If not,
#        fli[1] spaces are added before and the considered
#        width is fli[2] (not including the added spaces).
#        Also this means that your already wrote fli[3]
#        characters on the first line {this last possibility
#        can be used only when there are no title and for the 
#        the first component.
#        For instance, a French paragraph will be issued
#        with fli = c(5,wid,0). The possibility of modifying
#        wid for the first line can be of use when adding
#        the name of an item first.>>
#{sep} << Number of lines separating each paragraph.>>
#{jus} << Type of justification (1=left, 2=centred,
#             3=left).>>
#{tronc} << Must troncation be done when a word
#          is greater than the proposed wid?>>
#{ed} << Framing at the beginning of each line.>>
#{ef} << Framing at the end of each line.>>
#{imp} << Printing is performed and nothing is returned.
#                If FALSE, the character string is returned 
#                (including possible new lines)>>
#VALUE
# either nothing or a character string according to imp
#EXAMPLE
# rs003k("RESET"); # needed only for R checking, to be forgotten
# form3paragraphe(c("My Title","My important first sentence.","Last one!"));
#REFERENCE
#SEE ALSO
#CALLING {form3repete}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_08_01
#REVISED 09_09_29
#--------------------------------------------
{
sep <- min(max(0,sep),5);
if (titre>-2) {
    # the possible title
    res <- form3titre(texte[1],empha=titre,indent=nchar(ed),imp=imp);
    if (imp) { cat(form3repete("\n",sep));
    } else { res <- c(res,form3repete("\n",sep));}
    texte <- texte[-1];
} else { res <- character(0);}
#
for (i in sjl(texte)) {
    # paragraph after paragraph
    mots <- strsplit(texte[i]," ")[[1]];
    nlig <- 0;
    while (length(mots) > 0) {
        # the paragraph is not empty
        nlig <- nlig+1;
        # is it the first line and must it be different?
        spfl <- ((nlig==1)&(!isempty(fli)));
        if (spfl) {
            check4tyle(fli,"numeric",c(3,Inf));
            wiid <- fli[2];
            lili <- form3repete(" ",fli[1]);
            trop <- fli[3];
        } else {
            wiid <- wid;
            #lili <- character(0);
            lili <- "";
            trop <- 0;
        }
        if (isempty(wiid)) { wiid <- 60;}
	while ( ((length(mots) > 0) &&
		 (nchar(paste(lili,collapse=" ")) < (wiid-nchar(mots[1])))
		) || (
		 (length(lili) == 0)
		)
	      ) {
	    lili <- c(lili,mots[1]);
	    mots <- mots[-1];
	}
	#cat("<<",length(mots),">>\n");
        lili <- paste(lili,collapse=" ");
        #cat("{{",nchar(lili),lili,"}}\n");
        lili <- form3justifie(lili,wid-trop,jus,tronc);
        if (spfl) { lili <- paste(lili,ef,"\n",sep="");
        } else { lili <- paste(ed,lili,ef,"\n",sep="");}
	if (imp) { cat(lili);
	} else {
	    res <- c(res,lili);
	    lili <- character(0);
	}
    }
    if (imp) { cat(form3repete("\n",sep));
    } else { res <- c(res,form3repete("\n",sep));}
}
# returning
if (!imp) { return(res);}
cat(res);
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3affiche <- function(x,pau=FALSE,cat=FALSE,...)
#TITLE (00) displays with its name the object x
#DESCRIPTION everything is in the title
#DETAILS
#PKEYWORDS
#KEYWORDS print
#INPUTS
#{x}<<The object to print.>>
#[INPUTS]
#{pau} << Must a pause be performed after the display?>>
#{cat} << Must the printing be done with 'cat' instead of print?>>
#{\dots} <<possible arguments for the print function.>>
#VALUE
# a print (or cat) is done and x is returned
#EXAMPLE
# uu <- "azerty";
# form3affiche(uu);
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
if (pau) { pause("affichage");}
# returning
x;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
geom3lmargin <- function(x,ma=0.15)
#TITLE (00) adds some padding to a range
#DESCRIPTION
#   Used for the preparation of graphic outputs:
#  the range of 'x' is extended of a proportion 
#  'ma' at both ends
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{x}<<vector of values to consider>>
#[INPUTS]
#{ma} <<proportion margin to add>> 
#VALUE
# A vector of two numerical giving the extended range
#EXAMPLE
# geom3lmargin(1:5);
# geom3lmargin(1:5,0.1);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_07_16
#REVISED 07_07_16
#--------------------------------------------
{
    res <- range(x);
    res + ma*diff(res)*c(-1,1);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
geom3xyz2pol <- function(coo)
#TITLE (00) transforms cartesian coordinates in polar coordinates
#DESCRIPTION Transforms, into R to three, cartesian coordinates
# in polar coordinates
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{coo}<<the x,y,z coordinates.>>
#[INPUTS]
#VALUE
# A numerical vector with (rho, theta, phi)
#EXAMPLE
# geom3xyz2pol(c(1/sqrt(2),1/sqrt(2),0));
#REFERENCE
#SEE ALSO geom3pol2xyz
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_07_16
#REVISED 07_10_10
#--------------------------------------------
{
    if (any(is.na(coo))) { return(rep(NA,3));}
    rho <- sqrt(sum(coo^2));
    if (abs(rho ) < 10^-10) { return(rep(0,3));}
    phi <- asin(coo[3]/rho);
    x <- coo[1]/rho/cos(phi);
    y <- coo[2]/rho/cos(phi);
    theta <- acos(x);
    if (y < 0) { theta <- 2*pi - theta;}
    c(rho,theta,phi);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
geom3pol2xyz <- function(poo)
#TITLE (00) transforms polar coordinates in cartesian coordinates
#DESCRIPTION (in R to the three)
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{poo}<<the polar coordinates (rho, theta, phi)>>
#[INPUTS]
#VALUE
# A numerical vector with (x, y, z)
#EXAMPLE
# uu <- geom3xyz2pol(c(1/sqrt(2),1/sqrt(2),0));
# geom3pol2xyz(uu);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_07_16
#REVISED 07_09_07
#--------------------------------------------
{
    if (any(is.na(poo))) { return(rep(NA,3));}
    rho <- poo[1];theta <- poo[2]; phi <- poo[3];
    rho*c(cos(theta)*cos(phi),sin(theta)*cos(phi),sin(phi));
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
geom3pointi <- function(x0,y0,x1,y1,d)
#TITLE (00) interpolation in R to the 2
#DESCRIPTION
# Interpolation in the plane: given two points p0=(x0,y0) and p1=(x1,y1)
# returns the point p=(x,y) such that
#  vector(p0,p) = z*vector(p0,p1)
#  where z is such that the distance between
#  p0 and p is abs(d). Also sign(d) == sign(z)
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS 
#{x0} <<see description>>
#{y0} <<see description>>
#{x1} <<see description>>
#{y1} <<see description>>
#[INPUTS]
#{d} <<see description>>
#VALUE
# Returns c(x,y), the interpolated points.
#EXAMPLE
# geom3pointi(1,1,10,10,sqrt(2));
#REFERENCE
#SEE ALSO
#CALLING {MaTrBeFo}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_05_23
#REVISED 07_06_14
#--------------------------------------------
{
    z <- d / sqrt((x1-x0)^2+(y1-y0)^2);
    z*c(x1-x0,y1-y0) +c(x0,y0);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
num2disc <- function(val,lim=0,pod=c("-","+"))
#TITLE (00) transforms a numeric value into a 
# categoric value.
#DESCRIPTION A first step towards the discretization
# of numeric nodes into n classes
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{val}<<vector of value to be discretize.>>
#{lim}<< Vector of the n-1 compulsory limits 
#            to define the n classes. When x == lim[i]
#            it belongs to the ith class.>>
#{pod}<< Vector of the labels associated 
#                    to the n classes.>>
#[INPUTS]
#VALUE
# the factor of the resulting categoric values
#EXAMPLE
# set.seed(1234);
# rs003k("RESET"); # needed only for R checking, to be forgotten
# num2disc(runif(10)-0.5)
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_10_06
#REVISED 09_03_20
#--------------------------------------------
{
if (isempty(val)) { return(character(0));}
if ((1+length(lim)) != length(pod)) {
    form3affiche(lim);
    erreur(pod,"Non compatible lengths of lim and pod.");
}
if (length(lim)==0) {
    return(rep(pod,length(val)));
}
res <- rep(1,length(val));
for (jbd in 1:length(lim)) {
    res <- res + (lim[jbd] <= val);
}
pod[res];
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
open8graph <- function(prefix=NULL,...) 
#TITLE (00) opens the graph device for rebastaba
#DESCRIPTION
# According to the global constant rbsb.mfi a
# graphical device is open or not.
# Must be called before plotting something ones want
# to keep under rbsb.mgr type. This global constant 
# can take only value in c("pdf","png").\cr
#DETAILS
# The file opened for storing the graph is named with
# three components separated with dots: rbsb.fpx, prefix 
# and rbsb.mgr. rbsb.fpx is a prefix to be modified by 
# the user as convenient. rbsb.ffg is the number of the
# current figure incremented by this function. rbsb.mgr is
# the suffix associated to the type of graph (either 'pdf'
# or 'png').
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#[INPUTS]
#{prefix} << When NULL the \code{1+rbsb.ffg} numeric is taken
#                to give the number of the file with three
#                digits. If not, it is a character giving
#                the complete prefix to use before the suffix.>>
#{\dots} << argument(s) to be transmitted to the openning device
#         of graphics. Quite useful for specific character and
#         picture sizes, or to get more than one graph into
#         the file.>>
#VALUE
# Nothing but when rbsb.mfi is \code{TRUE} the
# and \code{is.null(prefix)}, the graphical device is open and the global 
# constant "rbsb.ffg" is increased *before* with one.
#EXAMPLE
# rs003k("RESET"); # needed only for R checking, to be forgotten
# print(rbsb.ffg);
# open8graph();
# close8graph();
# print(rbsb.ffg);
#REFERENCE
#SEE ALSO close8graph
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_10
#REVISED 09_11_17
#--------------------------------------------
{
if (rbsb.mfi) {
    if (is.null(prefix)) {
        assign("rbsb.ffg",rbsb.ffg + 1,pos=".GlobalEnv");
        prefix <- paste(rbsb.fpx,form3justifie(rbsb.ffg,nbc=4,
                                               format=3,tronc=FALSE,
                                               carac="0"),
                        sep=".");
    }
    fifi <- paste(prefix,rbsb.mgr,sep=".");
    gopen <- FALSE;
    if (rbsb.mgr == "pdf") { pdf(fifi,...); gopen <- TRUE;}
    if (rbsb.mgr == "png") { png(fifi,...); gopen <- TRUE;}
    if (!gopen) {
        erreur(rbsb.mgr,
               "This type of graph is not yet implemented in /rbsb/");
    }
}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
close8graph <- function(message=rbsb.cha0) 
#TITLE (00) closes the file open by open8graph
#DESCRIPTION
# According
# to the global constant rbsb.mfi close the file
# open by open8graph. Also if \code{!rbsb.mba}
#  a pause is issued.
#DETAILS
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#[INPUTS]
#{message} << Message to display on the terminal.
# When empty, no message will be displayed.>>
#VALUE
# nothing but the actions indicated in the description field are performed
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_29
#REVISED 10_03_16
#--------------------------------------------
{
# closing the device
if (rbsb.mfi) { dev.off();}
# displaying the message
if (!isempty(message)) {
    if (!rbsb.mba) { pause(message,"pause from close8graph");
    } else { cat("<<<",message,">>>\n");}
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
open8text <- function(append=TRUE) 
#TITLE (00) opens the standard output text for rebastaba
#DESCRIPTION
# According to the global constant rbsb.mfi the
# standard output text of rebastaba is open (in
# append mode) or not. The name of this file is provided
# by the constant rbsb.fou.
# Must be called before printing something ones want
# to keep on file. 
#DETAILS 
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#[INPUTS]
#{append} << Must the current file rbsb.fou be continued ?>>
#VALUE
# nothing but the actions indicated in the description field are performed
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_10
#REVISED 08_08_28
#--------------------------------------------
{
if (rbsb.mfi) { sink(rbsb.fou,append);}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
close8text <- function(message=rbsb.cha0) 
#TITLE (00) pauses (and more) the program until an answer is given
#DESCRIPTION
# Closing the output file rbsb.fou according to rbsb.mfi. A pause allowing 
# to stop the process is issued if rbsb.mba is false.
#DETAILS
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#[INPUTS]
#{message} << Message to display on the terminal. 
# When empty, no message will be displayed.>>
#VALUE
# nothing but the actions indicated in the description field are performed
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_29
#REVISED 10_03_16
#--------------------------------------------
{
# closing the file
if (rbsb.mfi) { sink();}
# displaying the message
if (!isempty(message)) {
    if (!rbsb.mba) { pause(message,"pause from close8text");
    } else { cat("<<<",message,">>>\n");}
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
depeche <- function(what="???",answer=TRUE) 
#TITLE (00) issues a message on the screen [and returns the answer]
#DESCRIPTION
# This function issues a message and pause if an answer is required.
#DETAILS
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#[INPUTS]
#{what} << Message to be issued>>
#{answer} << When an answer is awaited.>>
#VALUE
# the answer as character if answer is TRUE (but no answer is transformed 
# into "") if not ""
#EXAMPLE
# uu <- depeche("What time is it?");
# print(uu);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_02_04
#REVISED 09_02_04
#--------------------------------------------
{
cat(">>> --------------> ",what,"\n");
if (answer) {
    cat(">>> Give the answer and 'Enter' to continue \n");
    res <- scan(what="character",nmax=1);
    if (length(res) == 0) { res <- "";}
} else { res <- "";} 
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
pause <- function(what="",where=NULL) 
#TITLE (00) pauses the program until an answer is given
#DESCRIPTION
# This function issues a pause with a message allowing 
# to stop the process or to continue it.
#DETAILS
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#[INPUTS]
#{what} << Message commenting the pause>>
#{where} << To indicate where the pause was made. When
#          NULL a shortened message is issued.>>
#VALUE
# nothing but the actions indicated in the description field are performed
#EXAMPLE
# rs003k("RESET"); # needed only for R checking, to be forgotten
# pause("Time for lunch!");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_25
#REVISED 09_01_27
#--------------------------------------------
{
if (!isempty(where)) {cat(">>> (",where,")\n");}
if (!isempty(what))  {cat(">>> --------------> ",what,"\n");}
cat(">>> 'Enter' to continue | any key(s) +'Enter' to stop \n");
quoi <- scan(what="character",nmax=1);
if (length(quoi) != 0) {
    stop("(...YOU decided to stop rebastaba...)",call.=FALSE);
}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
erreur <- function(x,...,w=FALSE)
#TITLE (00) issues an error message and concludes accordingly
#DESCRIPTION
# when called this function prints x, then displays a message before stopping 
# the process except if it is a warning or if the general constant
# of rebastaba rbsb.mfa is true.
#DETAILS
#PKEYWORDS
#KEYWORDS error
#INPUTS
#{x} <<object to be printed before the message. When NULL
#      or "", nothing is printed. When it is a list, all components
#      of the list are successively printed.>>
#{\dots}<<pieces of message to display after pasting>>
#[INPUTS]
#{w} << Indicates if it is a simple warning>> 
#VALUE
# nothing
#EXAMPLE
# rs003k("RESET"); # needed only for R checking, to be forgotten
# erreur(matrix(1:4,2),"This matrix is not symmetric",w=TRUE)
# erreur("Are you sure of it?",w=TRUE);
#
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_03
#REVISED 10_04_21
#--------------------------------------------
{
form3repete("~",60,TRUE);
if (!isempty(x)) {
    if (is.list(x)) {
        for (i in sjl(x)) {
            form3repete("~",40,TRUE);
            cat("<< Displaying ",deparse(substitute(x[[i]]))," >>\n")
            print(x[[i]]);
        }
    } else {
        cat("<< Displaying ",deparse(substitute(x))," >>\n")
        print(x);
    }
}
message <- paste(...);
print(message);
if (w) {
    cat(rbsb.msi,"SIMPLE WARNING:\n");
} else {
    on.exit(traceback());
    cat(rbsb.msi,"ERREUR FATALE\n");
    form3repete("~",60,TRUE);
    if (rbsb.mfa) { stop("stopped by rebastaba");}
}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rapport <- function(origine)
#TITLE (00) issues an error message when rebastaba fails
#DESCRIPTION
# Ask the user to send the author a detailed report.
#DETAILS
#PKEYWORDS
#KEYWORDS error
#INPUTS
#{origine}<<A character indicating where the difficulty
#           occured.>>
#[INPUTS]
#VALUE
# nothing
#EXAMPLE
# # rapport("For the moment, we are doing the tests...");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_10_23
#REVISED 09_01_12
#--------------------------------------------
{
    message <- new("des",
                   name="rebastaba error",
                   orig=paste(origine,collapse=" "),
                   time=today(),
                   defi="",
                   role="Improve rebastaba code",
                   comm=c(paste("Congratulations! You were clever enough",
                              "to stick rebastaba."),
                          paste("Sorry because you didn't get your",
                                "result"),
                          paste("Perhaps you can have a look at what you",
                                "are wanting, to modify your own code?"),
                          paste("In any case, I would be pleased if you",
                                "could report me the error",
                                "with a reproductible example"),
                          "Jean-Baptiste.Denis@Jouy.Inra.Fr"));
    form3repete("+",60,TRUE);
    form3repete("+",60,TRUE);
    print(message,"a");
    form3repete("+",60,TRUE);
    form3repete("+",60,TRUE);
    cat("<<D'esol'e de ce contretemps pour vous !>>\n\n");
    stop();
    invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
expr3cobu <- function(x)
#TITLE (00) provides a suitable Bugs transcription for translation into 
# Bugs language
#DESCRIPTION
# In fact, just removing the quotation for node names
#DETAILS
# The difference with expr3coge is that the expression is adapted
# for the Jags coding model without care about the columns of X matrix.
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{x} <<either a numeric or a rebastaba expression (character)>>
#[INPUTS]
#VALUE
# An interpretable character string to be included 
# instead of parameter for Jags coding
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING {easyp3cut}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_02_11
#REVISED 08_09_10
#--------------------------------------------
{
erreur(NULL,"Further on, expr3cobu will be remade. More precisely,",
            "must be replaced by the extension of easyp2code... WILL BE DONE!");
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
expr3func <- function(gc,tc="")
#TITLE (00) provides the generation function from gc and ft
#DESCRIPTION
# From pieces of code usually generated by easyp2code, respectively
# for generation and transformation, the generation function is 
# produced.
#DETAILS
# No check is made about the code validity.
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{gc} <<code for generation, character of length 1.>>
#[INPUTS]
#{tc} <<Code for transformation, character of length 1.
#       When empty transformation part is introduced.>>
#VALUE
# resulting object function (to be introduced in the @rlks 
# slot of the bn's
#EXAMPLE
# rs003k("RESET"); # needed only for R checking, to be forgotten
# expr3func("{rnorm(100)}","{abs(Y)}");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_05
#REVISED 08_09_18
#--------------------------------------------
{
nive <- 0;
# construction of the function
fff <- paste(form3ind(0+nive,FALSE),"function(X) {",
             form3ind(1+nive),"Y <-\n",
             gc,
             form3ind(1+nive),"Y <- matrix(Y,nrow(X));",
             sep="");
if (!isempty(tc)) {
    fff <- paste(fff,
                 form3ind(1+nive),"Y <-\n",
                 tc,
                 sep="");
}
fff <- paste(fff,
             form3ind(1+nive),"Y;",
             form3ind(0+nive),"}",
             sep="");
# evaluation of the function
res <- character(0);
eval(parse(text=paste("res <-",fff)));
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
isempty <- function(x) 
#TITLE (00) to avoid difficulty with is.null
#DESCRIPTION
# returns TRUE is the structure is empty
#DETAILS
#PKEYWORDS
#KEYWORDS programming
#INPUTS
#{x}    <<object to be scrutinazed>>
#[INPUTS]
#VALUE
# TRUE when the object is considered as empty
# FALSE if not
#EXAMPLE
# rs003k("RESET"); # needed only for R checking, to be forgotten
# isempty(numeric(0));
# isempty(NULL);
# isempty(rbsb.fau0);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_15
#REVISED 10_02_11
#--------------------------------------------
{
  if (is.null(x))                           { return(TRUE);}
  if (length(x)==0)                         { return(TRUE);}
  if (identical(x,rbsb.log0))           { return(TRUE);}
  if (identical(x,rbsb.num0))           { return(TRUE);}
  if (identical(x,rbsb.cha0))           { return(TRUE);}
  if (identical(x,rbsb.lis0))           { return(TRUE);}
  if (identical(x,rbsb.fun0))           { return(TRUE);}
  if (identical(x,rbsb.dfr0))           { return(TRUE);}
  if (identical(x,rbsb.fau0))          { return(TRUE);}
  if (identical(x,rbsb.des0))          { return(TRUE);}
  if (identical(x,rbsb.daf0))          { return(TRUE);}
  FALSE;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
today <- function(format="red")
#TITLE (00) returns a character giving the present day
#DESCRIPTION  returns a character giving the present day
# The day is given as aa_mm_dd numbers
#DETAILS
# based on Sys.Date R function
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#[INPUTS]
#{format} <<"red" for reduced.>>
#VALUE
# a character
#EXAMPLE
# cat("Today is",today(),"\n");
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 08_07_01
#REVISED 10_04_19
#--------------------------------------------
{
res <- as.character(Sys.Date());
if (format=="red") {
    res <- paste(strsplit(res,"-")[[1]],collapse="_");
    res <- substr(res,3,10);
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
now <- function(format="red")
#TITLE (00) returns a character giving the present moment
#DESCRIPTION  returns a character giving the present moment
# including the day given as aa_mm_dd numbers
#DETAILS
# based on Sys.time R function
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#[INPUTS]
#{format} <<"red" for reduced.>>
#VALUE
# a character
#EXAMPLE
# cat("Now is",now(),"\n");
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_04_19
#REVISED 10_04_19
#--------------------------------------------
{
res <- as.character(Sys.time());
if (format[1]=="red") {
    res <- paste(strsplit(res,"-")[[1]],collapse="_");
    res <- paste(strsplit(res,":")[[1]],collapse="_");
    res <- paste(strsplit(res," ")[[1]],collapse="_");
    res <- substr(res,3,100);
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
char2chars <- function(cara,sep=rbsb.sep0,rsep="no_action")
#TITLE (00) transforms a single character into a character vector 
#DESCRIPTION
# Just doing a character vector from a single
# character using \code{sep} to separate the different
# components.
#DETAILS
# Argument \code{sep} is a possible separator. If empty
# no splitting is performed.
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{cara} << The single character to transform.>>
#[INPUTS]
#{sep} <<  Separator for the splitting
# when '', no splitting is done.>>
#{rsep} << Indicates if repetitions of \code{sep0} must be considered as signicant or not.
#          If \code{no_action} then their repetitions will be ignored
#          if not \code{character(0)} components will be introduced.>>
#VALUE
# a character vector if everything was good.
# a 'faux' object otherwise.
#EXAMPLE
# rs003k("reset"); # for R checking convenience
# char2chars("A B C");
# char2chars("A B C","  ");
# char2chars("A B  C  ");
# char2chars("A B  C  ",rsep="");
# char2chars("0.1 0.9 // 0.5 0.5 // 0.9 0.1","//");
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
# This function is one of the evolution of the former char2vect function.
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 08_12_22
#REVISED 10_04_09
#--------------------------------------------
{
# checking
if (length(cara)>1) {
    return(new("faux",orig="char2chars",
           comm=as.character(cara),
           defi="A single character was expected as input"));
}
if (length(cara) == 0) {
    return(character(0));
}
if (!is.character(cara)) {
    return(new("faux",orig="char2chars",
           orig=class(cara),
           comm=as.character(cara),
           defi="A character even null was expected!"));
}
# extracting the vector
if (sep!="") {
    res <- strsplit(cara,sep)[[1]];
    # removing parasite values
    if (rsep == "no_action") {res <- res[res!=""];}
} else { res <- cara;}
#
# returning without error
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
list2char <- function(liste,tw=" ",nam=NULL)
#TITLE (00) transforms a list into a character vector
#DESCRIPTION
# The resulting vector will have as many components as 
# list. Each one being the concatenation of its own
# components interpreted \code{as.character} with \code{tw}
# as seperator.
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{liste} << The list to transform.
#{nam} << When \code{is.null(nam)} and they exist, the names 
# of the list are reported to the vector.>>
#[INPUTS]
#{tw} <<  Separator for each component.>>
#{nam} << Names to be apply to the created list.
# The exact number is expected, as well as non
# repeated names.>>
#VALUE
# a character vector with possibly
# named components
#EXAMPLE
# list2char(char2list(c("A B C","D E")));
# list2char(char2list(c("A B C","D E"),""),"");
# char2list(list2char(char2list(c("A B C","D E"))));
# char2list(list2char(char2list(c("A B C","D E"),""),""),"");
#REFERENCE
#SEE ALSO char2list
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_03_08
#REVISED 10_03_08
#--------------------------------------------
{
# checking
check4tyle(liste,"list",-1);
if (!is.null(nam)) {
    check4tyle(nam,"character",length(liste));
    if (length(unique(nam)) != length(nam)) {
        erreur(sort(nam),"Not all names are different!");
    }
}
# creating the list
 res <- character(length(liste));
# naming the list
if (!is.null(nam)) {
    names(res) <- nam;
} else {
    names(res) <- names(liste);
}
# filling the vector
for ( nn in sjl(res)) {
    res[nn] <- paste(as.character(liste[[nn]]),collapse=tw);
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
char2list <- function(cara,tw=" ",nam=NULL)
#TITLE (00) transforms a character into a list
#DESCRIPTION
# Just doing a list of single (or not) characters from a
# a character vector, taking care of the names
# in a due way.
#DETAILS
# Argument 'tw' is a possible separator. If empty
# only one character each component. If not
# each caracter is splitted with it...
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{cara} << The vector of characters to transform.
# When is.null(nam) and they exist, the names 
# of the vector are reported to the list.>>
#[INPUTS]
#{tw} <<  Separators for the second step splitting.>>
#{nam} << Names to be apply to the created list.
# The exact number is expected, as well as non
# repeated names.>>
#VALUE
# a list of single character with possibly
# named components
#EXAMPLE
# char2list(c("A B C","D E"));
# char2list(c("A B C","D E")," ");
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 08_12_18
#REVISED 10_04_07
#--------------------------------------------
{
# checking
check4tyle(cara,"character",-1);
if (!is.null(nam)) {
    check4tyle(nam,"character",length(cara));
    if (length(unique(nam)) != length(nam)) {
        erreur(sort(nam),"Not all names are different!");
    }
}
# creating the list
 res <- vector("list",length(cara));
# naming the list
if (!is.null(nam)) {
    names(res) <- nam;
} else {
    names(res) <- names(cara);
}
# filling the list
for ( nn in sjl(cara)) {
    recu <- char2chars(cara[nn],tw);
    if (is(recu,"faux")) {
        print(recu);
        erreur(cara[nn],"The transformation into vector was refused!");
    } else {
        res[[nn]] <- recu;
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
pprint <- function(x,fifi="",...)
#TITLE (00) double print at the screen and into a text file
#DESCRIPTION
# Just doing a double printing (i) on the screen and
# (ii) appending to the file fifi
#DETAILS
# The file must reopen to empty it if necessary
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{x} << The object to print.>>
#[INPUTS]
#{fifi} <<  Name of the file, if '' the print
#         is only done onto the screen.>>
#{\dots} <<additional arguments for the print call.>>
#VALUE
# nothing is returned
#EXAMPLE
# pprint("to see what happen!","toto.txt");
# read.table("toto.txt");
# unlink("toto.txt");
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 09_01_05
#REVISED 09_01_05
#--------------------------------------------
{
# printing onto the screen
print(x,...);
# appending to the file
if (fifi != "") {
    sink(fifi,append=TRUE);
    print(x,...);
    sink();
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
var3standard <- function(nvar,bef="-",aft="-")
#TITLE (00) provides nvar standard names of variables
#DESCRIPTION
#   numbering surrounded with 'bef' and 'aft'
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{nvar} <<Number of variables names to provide.
#         When less than 2, '' is returned.>>
#[INPUTS]
#{bef} << To put before the number.>>
#{aft} << To put after  the number.>>
#VALUE
# A character of length nvar with standard
# names for repetitions. When 'nvar' is one, an empty
# character is returned.
#EXAMPLE
# rs003k("RESET"); # needed only for R checking, to be forgotten
# var3standard(5);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
# add left '0' to get names with the same 
# number of characters whatever is the 
# number of variable names to generate.
#AUTHOR J.-B. Denis
#CREATED 09_04_04
#REVISED 09_05_29
#--------------------------------------------
{
# checking
if (isempty(nvar)) { return("");}
check4tyle(nvar,"numeric",c(0,Inf));
check4tyle(bef,"character",1);
check4tyle(aft,"character",1);
# returning
if (nvar<2) { return("");}
paste(bef,sj(nvar),aft,sep="");
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
check4tyle <- function(x,typ,len,
                       message=NULL,fatal=TRUE,na.rm=FALSE)
#TITLE (00) checks the type and the length
# of some standard object
#DESCRIPTION
# If not correct, a fatal message is issued.
# NA are detected and considered as wrong.
#DETAILS
# 'integer' has not got the meaning in is.integer R
# standard function. 'null' must be understood as
# resulting TRUE with 'isempty'.
#PKEYWORDS
#KEYWORDS error
#INPUTS
#{x} <<object to be checked.>>
#{typ} <<The list of correct types, among
# 'null",'integer','numeric','character',
# 'logical','list','any','function'.
# As understood, 'any' implies that no
# check of the type is performed.>>
#{len} <<If length(len)==1, the exact length
# to be checked, if not must be of length for
# the possible range of length(x). When -1,
# no check on the length is performed.\cr
# For data.frame, it is the number of columns.
# When \code{na.rm} the check is made after
# removing the NA values.>>
#[INPUTS]
#{message} << Some additional message to be
#            issued before stopping.>>
#{fatal} << what to do when discovering
#           an inconsistency ? TRUE: this function prints the message
#           and stops; FALSE: this function returns
#           the message as a character.
#{na.rm} << Must the \code{NA} values be removed before checking?
#           This is implemented only for the types integer, numeric,
#           character and logical.>>
#VALUE
# When the check is validated returns TRUE.
# If not, according to \code{fatal} prints the
# message and stops or returns the message.
#EXAMPLE
# check4tyle(letters,c("numeric","character"),c(20,30),"!OK");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE improves the treatment of 'NA's
#AUTHOR J.-B. Denis
#CREATED 09_05_02
#REVISED 10_02_18
#--------------------------------------------
{
# checking the arguments and preparation
# accepted types by check4tyle
typena <- c("integer","numeric","character",
           "logical");
types <- c(typena,"list","function","any",
           "data.frame","null");
type <- match(typ,types);
if (all(is.na(type))) {
    erreur(list(types,typ),
           "CHECK4TYLE: None of the proposed type was in the list!"
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
if (all(!is.na(match(typ,typena)))) { if (any(is.na(x))) { 
stop();
    erreur(type,"CHECK4TYLE: 'x' is or comprises 'NA' which were not removed!",w=TRUE);
}}
# possible type for the check
type <- types[type[!is.na(type)]];
if (!is.numeric(len)) {
    erreur(len,"CHECK4TYLE: 'len' must be NUMERIC of size one or two");
}
if ((length(len)==0)|(length(len)>2)) {
    erreur(len,"CHECK4TYLE: 'len' must be numeric of size ONE or TWO");
}
# processing for the length
if (length(len)==1) {
    if (len>=0) { if (length(x)!=len) {
        if (narm>0) { res <- c(res,paste(narm,"components were removed"));}
        res <- c(res,"CHECK4TYLE: 'x' is not of length 'len'");
    }}
} else {
    if ((length(x)<len[1])|(length(x)>len[2])) {
        if (narm>0) { res <- c(res,paste(narm,"components were removed"));}
        res <- c(res,"CHECK4TYLE: 'x' has got a length outside 'len'");
    }
}
# processing for the type
if (!("any" %in% type)) {
    ty <- FALSE;
    for (tt in type) {
        if (tt=="integer")   { if (is.numeric(x)) {
            if (all(x==round(x))) { ty <- TRUE;}
        }}
        if (tt=="numeric")   { if (is.numeric(x))   { ty <- TRUE;}}
        if (tt=="function")  { if (is.function(x))  { ty <- TRUE;}}
        if (tt=="character") { if (is.character(x)) { ty <- TRUE;}}
        if (tt=="logical")   { if (is.logical(x))   { ty <- TRUE;}}
        if (tt=="list")      { if (is.list(x))      { ty <- TRUE;}}
        if (tt=="data.frame"){ if (is.data.frame(x)){ ty <- TRUE;}}
        if (tt=="null")      { if (isempty(x))      { ty <- TRUE;}}
    }
    if (!ty) {
        res <- c(res,paste("Among type = ",paste(type,collapse="/")));
        res <- c(res,paste("The class of 'x' is '",class(x),"'!",sep=""));
        res <- c(res,"CHECK4TYLE: 'x' does not belong to any of these!");
    }
}
# returning
if (identical(res,character(0))) {
    res <- TRUE;
} else {
    if (!is.null(message)) { res <- c(res,message);}
    print.default(x);
    if (fatal) {erreur("From check4tyle:",paste(res));}
}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
listev <- function(liste,ith)
#TITLE (00) returns the ith element of a list supposed to be numeric
#DESCRIPTION
# as liste[[ith]] but returns numeric(0) if the length of 
# the list is shorter than ith
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{liste}<<the list>>
#{ith}<<number of the element to return>>
#[INPUTS]
#VALUE
# The ith element of liste if it exists
# numeric(0) otherwise
#EXAMPLE
# listev(list(A=123,B=234,C=345),2);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
# I was expecting this a property of R!
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_07_09
#REVISED 07_09_18
#--------------------------------------------
{
if (length(liste) < ith) { return(numeric(0));}
else {
    if (is.null(liste[[ith]])) { return(numeric(0));}
    else { return(liste[[ith]]);}
}
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
easyp3cut <- function(x,pth=matrix(c("(","{","[",")","}","]"),3))
#TITLE (00) splits an expression into non nested blocks 
#DESCRIPTION
# x must be a character string which is parsed to 
#   extracts \code{n} types of blocks where n is \code{nrow(pth)+1}.
#   Something in between \code{pth[i,1]} and \code{pth[i,2]} is coded \code{i},
#   everything else is coded \code{0}. See the examples.
#DETAILS
# The braces can contain more than one character, they must be
# distinct. Be aware that no check is done about the possible nesting
# of the parentheses.
#PKEYWORDS syntax
#KEYWORDS utilities
#INPUTS
#{x} <<character string to split; must be of length one.>>
#[INPUTS]
#{pth} <<character matrix with two columns defining
# the opening parentheses in the first column and the
# closing parentheses in the second column.>>
#VALUE
# A list of two equal length vectors:
#{$blo} <<The vector of character strings (braces are taken off)>>
#{$typ} <<Corresponding codes of the blocks and 0 when outside of
# any defined block.>>
#EXAMPLE
# rs003k("RESET"); # for R checking compliance (useless)
# easyp3cut("(a+b)^[2]");
# easyp3cut("abs({{*Y*}})*{{A}}^{{B}}",matrix(c("{{","}}"),1));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_07_11
#REVISED 09_11_05
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(   x,"character",1);
    check4tyle(pth,"character",-1);
    if (!is.matrix(pth)) {
        erreur(pth,"pth must a character MATRIX with two columns");
    }
    if (ncol(pth)!=2) {
        erreur(pth,"pth must a character matrix with TWO columns");
    }
    ncas <- nrow(pth);
    if (sum(outer(pth[],pth[],"=="))!=2*ncas) { erreur(pth,"ALL tags must be distinct");}
}
# going back to the previous arguments
opar <- pth[,1]; cpar <- pth[,2];
# ncas is the number of different possible types
# of parenthezided blocks (can be 0)
ncas <- nrow(pth);
ics <- rep(NA,ncas);
blo <- character(0);
if (!is.null(x)) { if (length(x)==0) {x <- character(0);}}
chaine <- x;
fini <- FALSE; # indicates the end of the algorithm
nepa <- TRUE;  # indicates if no parenthesis is currently open
if (length(chaine) == 0) { fini <- TRUE;}
# a loop to discover the blocks
while(!fini) {
    if (nepa) {
        # No parenthesis is currently openned, looking for one to be
        # and putting the associated closing parenthesis into quoi
        # ic gives the position of the first one, -1 if no more.
        for (jd in 1:ncas) {
            # looking for the positions of all opening parenthesis
            ics[jd] <- regexpr(opar[jd],chaine,fixed=TRUE)[1];
        }
        # determining the first one
        if (sum(ics > 0) > 0) {
            ic <- min(ics[ics > 0]);
            qui <- (1:ncas)[ics == ic][1];
            # quoi is the closing parenthesis to find next
            quoi <- cpar[qui];
            # lop/lcp is the length of the opening and closing parentheses
            lop <- nchar(opar[qui]);
            lcp <- nchar(cpar[qui]);
        } else {ic <- -1;}
    } else {
        # Looking for the closing parenthesis of the presently openned
        # ic will give its position
        ic <- regexpr(quoi,chaine,fixed=TRUE)[1];
    }
    #
    if ( ic < 0) {
        # No new block was identified
        blo <- c(blo,chaine);
        fini <- TRUE;
    } else {
        # a block is under discovery
        if (nepa) {
            # the block is just openned
            blo <- c(blo,substr(chaine,1,ic-1));
            chaine <- substr(chaine,ic,nchar(chaine));
            nepa <- FALSE;
        } else {
            # the block must be closed
            blo <- c(blo,substr(chaine,1,ic+lcp-1));
            chaine <- substr(chaine,ic+lcp,nchar(chaine));
            nepa <- TRUE;
        }
    }
} # while(!fini);
# at this level the blocks were cutted with their braces...
# and some additional empty blocks were introduced
# (1) suppressing the empty blocks
bnul <- (blo == "");
blo <- blo[!bnul];
# (2) now coding and preparing the codes and removing the braces
cod <- numeric(0);
if (length(blo) > 0) {
    # removing empty blocks
    for (jbd in length(blo):1) {
        if (blo[jbd] == "") {blo <- blo[-jbd];}
    }
    # identifying each block
    cod <- rep(NA,length(blo));
    for (co in sjl(blo)) {
        qqq <- blo[co];
        for (jd in 1:ncas) {
            lop <- nchar(opar[jd]); lcp <- nchar(cpar[jd]);
            if (nchar(qqq)>=(lop+lcp)) {
            if ((substr(qqq,1,lop) == opar[jd]) & (substr(qqq,nchar(qqq)-lcp+1,nchar(qqq))==cpar[jd])) {
                blo[co] <- substr(qqq,lop+1,nchar(qqq)-lcp);
                cod[co] <- jd;
            }}
        }
        if (is.na(cod[co])) {cod[co] <- 0;}
    }
}
#returning
list(blo=blo,typ=cod);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
easyp3stickback <- function(d,pth=matrix(c("(","{","[",")","}","]"),3))
#TITLE (00) converse operation of easyp3cut
#DESCRIPTION
# d must be a list with $typ and $blo components
# (see easyp3cut for the details)
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{d} <<d$typ for the type of components, 
#      d$blo for the content of each component.>>
#[INPUTS]
#{pth} <<character matrix with two columns defining
# the opening parentheses in the first column and the
# closing parentheses in the second column.>>
#VALUE
# A character(1) of the reconstituted easyprograming
#EXAMPLE
# rs003k("RESET"); # for R checking compliance (useless)
# uu <- easyp3cut("abs({Y})*[A]^(B)");
# easyp3stickback(uu);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_03_31
#REVISED 09_11_05
#--------------------------------------------
{
# checking
if (!("blo" %in% names(d))) {erreur(d,"$blo is missing");}
if (!("typ" %in% names(d))) {erreur(d,"$typ is missing");}
if (length(d$typ)!=length(d$blo)) {erreur(d,"$typ & $blo has got != lengths");}
check4tyle(pth,"character",-1);
if (!is.matrix(pth)) {
    erreur(pth,"pth must a character MATRIX with two columns");
}
if (ncol(pth)!=2) {
    erreur(pth,"pth must a character matrix with TWO columns");
}
ncas <- nrow(pth);
if (sum(outer(pth[],pth[],"=="))!=2*ncas) { erreur(pth,"ALL tags must be distinct");}
# going back to the previous arguments
opar <- pth[,1]; cpar <- pth[,2];
# ncas is the number of different possible types of parenthezided blocks (can be 0)
mncas <- max(d$typ);
if (mncas > ncas) {
    erreur(list(opar,d$typ),"Some parentheses are not described into 'opar/cpar' arguments");
}
# reconstituting
nb <- length(d$typ);
res <- "";
for (ii in sjl(d$typ)) {
    ty <- d$typ[ii]; bl <- d$blo[ii];
    if (ty==0) {
        res <- paste(res,bl,sep="");
    } else {
        res <- paste(res,opar[ty],bl,cpar[ty],sep="");
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
unex <- function(xv,di,check=TRUE)
#TITLE (00) collapsing indices
#DESCRIPTION
#   Transforms multiple indices into a unique index
#   following the storage order of array elements
#   by R.
#DETAILS In /rbsb/ this is necessary for dealing with arrays
#   containing conditional probabilities for categoric
#   variables if one wants to get paralel drawing of all
#   simulated values with "rmultinom"
#KEYWORDS iteration
#INPUTS
#{xv}<<a N by K matrix, each row corresponding to uple of indices.
#      each column must be a factor.>>
#{di} <<dimensions of the K indices>>
#[INPUTS]
#{check}<<(=TRUE) must be checked of the consistency between xv and di
#                 be performed?>>
#VALUE
# A vector of size N with the unique index values
#EXAMPLE
# rs003k("RESET"); # needed only for R checking, to be forgotten
# uu <- rbsb.dfr1;
# print(cbind(uu,unex(uu,3:4)));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_07_06
#REVISED 08_07_01
#--------------------------------------------
{
K <- ncol(xv);
res <- 0;
if (K > 0) {
    # some checks
    if (check) {
        if (K != length(di)) {erreur(di,"number of indices different!");}
        for (k in 1:K) {
            if (!is.factor(xv[,k])) { erreur(NULL,"index",k,"is not a factor!");}
            if (min(as.numeric(xv[,k])) < 1) {erreur(NULL,"index",k,"is not positive!");}
            if (max(as.numeric(xv[,k])) > di[k]) {erreur(NULL,"index",k,"is greater than the dimension!");}
        }
    }
    # computing the collapsing
    res <- as.numeric(xv[,1]); KK <- di[1];
    if (K > 1) {for (k in 2:K) {
        res <- res + KK*(as.numeric(xv[,k])-1);
        KK <- KK*di[k];
    }}
}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
list2des <- function(li,name=rbsb.cha0)
#TITLE (00) transforms a consistent list into a new des object
#DESCRIPTION
# Just analyzing the components of the list
# (consistent names have to be used) which are supposed
# to be character and tackle them to produce consistent
# slots of a /des/
#DETAILS
# The main use of this function is to tackle alk read from text files
# with the function \code{file2list} avoiding the NULL values non acceptable
# for slots
#PKEYWORDS des
#KEYWORDS classes
#INPUTS
#{li} <<The list to be transformed into a des object.>>
#[INPUTS]
#{name} <<(=rbsb.cha0) gives the name of the object
#         when li$name does not exist. If both are absent
#         then an error is issued.>>
#VALUE
# The generated 'des' object
#EXAMPLE
# rs003k("RESET"); # needed only for R checking, to be forgotten
# print(list2des(list(name="A",role="Just to see",comm="Not very interesting")));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_01_06
#REVISED 10_03_10
#--------------------------------------------
{
# checking
check4tyle(li,"list",-1);
if (is.null(li$name)) {
    if (isempty(name)) {
        erreur(li,"the component 'name' is compulsory");
    } else {
        li$name <- name;
    }
}
if (is.null(li$orig)) { li$orig <- rbsb.cha0;}
if (is.null(li$time)) { li$time <- rbsb.cha0;}
if (is.null(li$defi)) { li$defi <- rbsb.cha0;}
if (is.null(li$role)) { li$role <- rbsb.cha0;}
if (is.null(li$comm)) { li$comm <- rbsb.cha0;}
# returning
new("des",name=li$name,orig=li$orig,time=li$time,
          defi=li$defi,role=li$role,comm=li$comm);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
des2list <- function(des)
#TITLE (00) transforms a /des/ object into a list.
#DESCRIPTION
# More or less the reverse of \code{list2des}.
#DETAILS
# The main use of this function is to tackle alk read from text files
# with the function \code{file2list} avoiding the NULL values non acceptable
# for slots
#PKEYWORDS des
#KEYWORDS classes
#INPUTS
#{des} <<The /des/ to be transformed into a list.>>
#[INPUTS]
#VALUE
# The generated list
#EXAMPLE
# rs003k("RESET"); # needed only for R checking, to be forgotten
# des2list(rbsb.des0);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_03_08
#REVISED 10_03_08
#--------------------------------------------
{
# checking
if (!valid8des(des)) {
    str(des);
    erreur(NULL,"This /des/ is not valid!");
}
# filling
res <- vector("list",0);
if (!isempty(des@name)) { res$name <- des@name;}
if (!isempty(des@orig)) { res$orig <- des@orig;}
if (!isempty(des@time)) { res$time <- des@time;}
if (!isempty(des@defi)) { res$defi <- des@defi;}
if (!isempty(des@role)) { res$role <- des@role;}
if (!isempty(des@comm)) { res$comm <- des@comm;}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
char2des <- function(x)
#TITLE (00) transforms,if necessary, a single character to a 'des' object
#DESCRIPTION
# when x is not a character(1) or a 'des' an error is issued
#DETAILS
#PKEYWORDS des
#KEYWORDS utilities
#INPUTS
#{x} <<name for the created 'des' or an already existing 'des' ojbect.>>
#[INPUTS]
#VALUE
# a 'des' object (not modfyied when x was already some one.
#EXAMPLE
# rs003k("RESET"); # needed only for R checking, to be forgotten
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
               time=today());
}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
comp3interpolation <- function(values,x,xx=1:x[max(x)])
#TITLE (00) extrapolation of known values
#DESCRIPTION From a set values given at known dates (x),
# interpolation is done for other dates (xx). Extrapolation 
# is rejected.
#DETAILS
#KEYWORDS utilities
#INPUTS
#{values}<<values to be interpolated>>
#{x}<<dates associated to the known values>>
#[INPUTS]
#{xx} <<dates for which values must be found by 
#       linear interpolation.>>
#VALUE
# The resulting values (in the same order that xx)
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_10_08
#REVISED 09_10_08
#--------------------------------------------
{
# checking
check4tyle(values,"numeric",c(2,Inf),"argument 'values' not accepted");
check4tyle(     x,"numeric",c(2,Inf),"argument 'x' not accepted");
nbv <- length(values);
if (length(x)!=nbv) {
    erreur(list(length(values),length(x)),"values and x must have the same lengths");
}
check4tyle(xx,"numeric",-1,"argument 'xx' must be numeric");
if (length(xx)==0) { return(numeric(0));}
if (!identical(as.numeric(range(x)),as.numeric(range(c(x,xx))))) {
    erreur(list(range(x),range(xx)),"Only interpolation is possible!");
}
# sorting the reference values
oo <- order(x);
values <- values[oo];
x <- x[oo];
# interpoling
res <- rep(NA,length(xx));
nbc <- length(x)-1;
for (clc in sj(nbc)) {
    bi <- x[clc]; bs <- x[clc+1];
    qui <- (bi <= xx) & (xx <= bs);
    if (length(qui) > 0) {
        rr <- values[clc] + (values[clc+1]-values[clc]) * (xx[qui] - bi) / (bs - bi);
        res[qui] <- rr;
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
get8daf <- function(daf,wh=0,strict=FALSE,check=TRUE)
#TITLE (00) returns the data frame associated to a /daf/
#DESCRIPTION
# Returns the data frame associated to a /daf/. 
# The row numbers can be choosen with \code{wh}.
#DETAILS
# According to daf@what a different action is performed to 
# get the data frame:
#{f} << The named daf@valu function is used without argument.>>
#{d} << The named daf@valu data frame is used.>>
#{t} << The named daf@valu file is read with read.table.>>
#{c} << The named daf@valu file is read with read.csv.>>
#{c2} << The named daf@valu file is read with read.csv2.>>
# When t, c or c2, options are forced to \code{header=TRUE}
# and \code{comment.char='#'}.
#KEYWORDS datasets
#INPUTS
#{daf} <<The /daf/ to be read.>>
#[INPUTS]
#{wh} <<(=0) numeric giving the numbers of the observations
#           to be read. Zero means all observations.>>
#{strict} <<(=FALSE) When TRUE a fatal error is issued
#           if some asked observations does not exist. If
#           not the present ones are returned.>>
#{check} <<(=TRUE) Must the 'daf' object be checked?>>
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
check4tyle(wh,"integer",-1,"These are supposed to be the asked row numbers");
if (min(wh) < 0) { erreur(wh,"Negative row number were asked.");}
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
opo <- sj(nrow(res));
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
get8object <- function(whi)
#TITLE (00) returns an object from a character(2) coding
#DESCRIPTION
# Returns the object associated to two character strings. 
# \code{whi[1]} designates a global variate and \code{whi[2]}
# indicates the way to interpret it.
#DETAILS
# This is similar in spirit to the function \code{get8daf} but
# much less specialized.
#KEYWORDS misc
#INPUTS
#{whi} <<The designation of the object. The different
# possibilities are given by the value of \code{whi[2]}.
# When "o", the object is returned as it is (when it does
# not exist, NULL is returned).
# When "c", must be a character.
# When "n", must be a numeric.
# When "d", must be a data.frame.
# When "m", must be a matrix.
# When "a", must be an array.
# When "f", must be a function.
# When "t1", must be a text file to be read with \code{read.table}.
# When "t2", must be a text file to be read with \code{read.csv}.
# When "t3", must be a text file to be read with \code{read.csv2}.
# (when reading a file, the corresponding function is called with
# \code{,header=TRUE,comment.char="#"}.)
# >>
#[INPUTS]
#VALUE
# The object. Except when \code{whi[2]=="o"} an error is
# issued if the object does not exist or is not from the 
# expected class.
#EXAMPLE
# rs003k("RESET"); # needed only for R checking, to be forgotten
# get8object(c("rbsb.cha0","o"));
# #will rise an error
# #get8object(c("rbsb.cha0","c"));
# uu <- "Bonjour";
# get8object(c("uu","c"));
# get8object(c("rbsb.dfr1","d"));
# get8object(c("rbsb.fun0","f"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_04_20
#REVISED 10_04_20
#--------------------------------------------
{
# constant
possibles <- c("any object","a character","a numeric",
               "a data.frame","a matrix","an array",
               "a function",
               "a text file to be read with read.table",
               "a text file to be read with read.csv",
               "a text file to be read with read.csv2"
              );
names(possibles) <- c("o","c","n","d","m","a","f","t1","t2","t3");
# checking
if (rbsb.mck) {
    check4tyle(whi,"character",2,"A character(2) is expected");
    #
    if (!expr3present(whi[2],names(possibles),TRUE)) {
        erreur(list(possibles,whi),"'whi[2]' must belong to the 'names(possibles)'...");
    }
}
# getting the values
if (!exists(whi[1])) {
    if (whi[2]=="o") {
        return(NULL);
    } else {
        erreur(whi,paste("The object 'whi[1]' supposed to be",
                         possibles[whi[2]],"does not exist.",
                         "Remind that NULL objects does not exist."));
    }
} else {
    coda <- paste("res <-",whi[1]);
    eval(parse(text=coda));
    if (whi[2] == "o") {
        # nothing to do
    }
    #
    if (whi[2]=="c") {
        if (!is.character(res)) {
            erreur(whi,"The object 'whi[1]' is not a character.");
        }
    }
    #
    if (whi[2]=="n") {
        if (!is.numeric(res)) {
            erreur(whi,"The object 'whi[1]' is not a numeric.");
        }
    }
    #
    if (whi[2]=="d") {
        if (!is.data.frame(res)) {
            erreur(whi,"The object 'whi[1]' is not a data.frame.");
        }
    }
    #
    if (whi[2]=="m") {
        if (!is.matrix(res)) {
            erreur(whi,"The object 'whi[1]' is not a matrix.");
        }
    }
    #
    if (whi[2]=="a") {
        if (!is.array(res)) {
            erreur(whi,"The object 'whi[1]' is not a array.");
        }
    }
    #
    if (whi[2]=="f") {
        if (!is.function(res)) {
            erreur(whi,"The object 'whi[1]' is not a function.");
        }
    }
    #
    if (whi[2] %in% c("t1","t2","t3")) {
        # the associated file must exist
        if (file.access(whi[1]) < 0) {
            erreur(whi,"File 'whi[1]' does not exist");
        }
    }
    if (whi[2] == "t1") {
	res <- read.table(whi[1],header=TRUE,comment.char="#");
    }
    #
    if (whi[2] == "t2") {
	res <- read.csv(whi[1],header=TRUE,comment.char="#");
    }
    #
    if (whi[2] == "t3") {
	res <- read.csv2(whi[1],header=TRUE,comment.char="#");
    }
    #
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
df2ustat <- function(data,
                     quant=c(0.01,0.05,0.10,0.25,0.50,
                             0.75,0.90,0.95,0.99),
                     nbmin=30)
#TITLE (00) computes univariate statistics from a data frame
#DESCRIPTION
# Data being supposed to be a data frame: observation by rows,
# variables by columns being numeric or factor; standard univariate
# statistics are computed for each variable.
#DETAILS
#PKEYWORDS statistics
#KEYWORDS 
#INPUTS
#{data} <<Date frame containing the data set. NA values are accepted.>>
#[INPUTS]
#{quant} <<(\code{numeric}) The desired quantiles to be computed
#          for the continuous variables.>>
#{nbmin} <<(\code{numeric(1)}) Minimum number of observations required to 
#                compute the statistics for each variable.>>
#VALUE
# a list with as many components as variables (getting the variable names).
# For numeric variables it is a named vector with standard statistics.
# For categoric variables it is a matrix having a column for each category
# sorted according to the frequencies and four rows: frequencies, rounded 
# percentages, rounded cumulated percentages in both directions.
#EXAMPLE
# rs003k("RESET"); # needed only for R checking, to be forgotten
# set.seed(1234);
# don <- matrix(round(runif(100)*100),20);
# dimnames(don)[[2]] <- LETTERS[1:5];
# print(df2ustat(as.data.frame(don),quant=c(0,0.5,1),nbmin=5));
# set.seed(1234);
# don <- matrix(letters[1+round(runif(100)*10)],20); 
# dimnames(don)[[2]] <- LETTERS[1:5];
# print(df2ustat(as.data.frame(don),nbmin=5));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_10_05
#REVISED 10_03_10
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(quant,"numeric",-1,"'quant' must be numeric");
    if ((min(quant) < 0) |
        (max(quant) > 1)) {
        erreur(quant,"Some values of 'quant' are not probabilities");
    }
    check4tyle(nbmin,"integer",1,"'nbmin' must be integer");
    nbmin <- abs(nbmin);
    if (!is.data.frame(data)) {
        erreur(data,"Must be a data frame!");
    }
}
# preparing the returned list
nbv <- length(data);
res <- vector("list",nbv);
names(res) <- names(data);
# quantile names
qna <- as.character(round(quant*100,1));
qna <- paste("Q",qna,"%",sep="");
qna[quant==0.5] <- "Median";
# standard names
cona <- c("number","missing",
          "mean","std-dev.","skewness","kurtosis",
          "min","MAX",
          qna);
cana <- c("Counts","Percentage","Cum.Per","cuM.per");
# computing for each variable
for (vv in sj(nbv)) {
    if (is.numeric(data[[vv]])) {
        # continuous case
        res[[vv]] <- rep(NA,length(cona));
        names(res[[vv]]) <- cona;
        res[[vv]]["number"] <- sum(!is.na(data[,vv]));
        res[[vv]]["missing"] <- sum(is.na(data[,vv]));
        res[[vv]]["min"] <- min(data[,vv],na.rm=TRUE);
        res[[vv]]["MAX"] <- max(data[,vv],na.rm=TRUE);
        if (res[[vv]]["number"] >= nbmin) {
            res[[vv]]["mean"] <- mean(data[,vv],na.rm=TRUE);
	    res[[vv]]["std-dev."] <- sqrt(var(data[,vv],na.rm=TRUE));
	    res[[vv]]["skewness"] <- skewness(data[,vv],na.rm=TRUE);
	    res[[vv]]["kurtosis"] <- kurtosis(data[,vv],na.rm=TRUE);
            pq <- 8 + sjl(quant);
	    res[[vv]][pq] <- quantile(data[,vv],quant,na.rm=TRUE);
	}
    }
    if (is.factor(data[[vv]])) {
        # categoric case
	nbni <- length(levels(data[[vv]]));
	res[[vv]] <- matrix(0,nbni,4);
	cou <- sort(table(data[[vv]]));
	dimnames(res[[vv]]) <- list(names(cou),cana);
	res[[vv]][,1] <- cou;
	res[[vv]][,2] <- round(cou*100/sum(cou));
	res[[vv]][,3] <- round(cumsum(cou*100/sum(cou)));
	res[[vv]][,4] <- rev(round(cumsum(rev(cou*100/sum(cou)))));
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
df2bstat <- function(data,nbmin=80)
#TITLE (00) computes bivariate statistics from a data frame
#DESCRIPTION
# Data being supposed to be a data frame: observation by rows,
# variables by columns being numeric or factor; standard bivariate
# statistics are computed for each couple of variables.
#DETAILS
#PKEYWORDS statistics
#KEYWORDS 
#INPUTS
#{data} <<Date frame containing the data set. NA values are accepted.>>
#[INPUTS]
#{nbmin} <<(\code{numeric(1)}) Minimum number of observations required to 
#                compute the statistics for each variable.>>
#VALUE
# a symmetric matrix having rows and columns associated to the variables.
# When the couple is numeric/numeric: the Pearson correlation; 
# categoric/categoric: the first eigen values of {n_ij/(n_i+.n_+j)};
# numeric/categoric: the Pearson correlation giving to the categoric the
# values of mean of the numeric.
#EXAMPLE
# rs003k("RESET"); # needed only for R checking, to be forgotten
# set.seed(1234);
# don <- as.data.frame(matrix(round(runif(100)*10),20));
# names(don) <- LETTERS[1:5];
# don[[4]] <- as.factor(don[[4]]);
# don[[5]] <- as.factor(don[[5]]);
# print(df2bstat(don,5));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
# The underlying theory for the non standard correlations is
# to be established
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_12_29
#REVISED 09_12_29
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(nbmin,"integer",1,"'nbmin' must be integer");
    nbmin <- abs(nbmin);
    if (!is.data.frame(data)) {
        erreur(data,"Must be a data frame!");
    }
}
# preparing the returned matrix
nbv <- length(data);
res <- matrix(NA,nbv,nbv);
dimnames(res) <- list(names(data),names(data));
# computing the 'correlations'
for (v1 in sj(nbv)) { for (v2 in sj(nbv)) {
    if (v1==v2) { res[v1,v2] <- 1;}
    if (v1 <v2) {
        rho <- NA;
        V1 <- data[[v1]]; V2 <- data[[v2]];
        ou1 <- !is.na(V1); ou2 <- !is.na(V2);
        ou <- which(ou1 & ou2);
        if (length(ou)>=nbmin) {
            if (is.numeric(V1) & is.numeric(V2)) {
                rho <- cor(V1[ou],V2[ou]);
            }
            if (is.factor(V1) & is.factor(V2)) {
                NN <- table(V1[ou],V2[ou]);
                nl <- nrow(NN); nc <- ncol(NN);
                nn <- sum(NN)*(solve(diag(apply(NN,1,sum),nl,nl)) %*%
                               NN %*%
                               solve(diag(apply(NN,2,sum),nc,nc))
                              );
                dd <- svd(nn)$d;
                rho <- dd[1]/sum(dd);
            }
            VN <- "non";
            if (is.numeric(V1) & is.factor(V2)) { VN <- V1[ou]; VF <- V2[ou];}
            if (is.numeric(V2) & is.factor(V1)) { VN <- V2[ou]; VF <- V1[ou];}
            if (is.numeric(VN)) {
                # computing the mean by level
                scsc <- rep(NA,length(levels(VF)));
                for (ll in sjl(levels(VF))) {
                    scsc[ll] <- mean(VN[VF==levels(VF)[ll]],na.rm=TRUE);
                }
                # creating the corresponding numerical vector
                VFN <- scsc[as.numeric(VF)];
                rho <- cor(VN,VFN);
            }
        }
        res[v1,v2] <- rho;
        res[v2,v1] <- res[v1,v2];
    }
}}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
check8list <- function(lili,nana)
#TITLE (00) checks the components of a list
#DESCRIPTION
# checks that all \code{nana} are components of
# the list \code{lili}.
#DETAILS
#KEYWORDS misc
#PKEYWORDS 
#INPUTS
#{lili}<<(\code{list}). The list to be checked.>>
#{nana}<<(\code{character}). The components to be found.>>
#[INPUTS]
#VALUE
# Nothing but an error is issued if the check
# is not positive.
#EXAMPLE
#REFERENCE 
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_11_09
#REVISED 09_11_09
#--------------------------------------------
{
# checking
nini <- names(lili);
for (ii in nana) {
    if (!expr3present(ii,nini)) {
        erreur(list(ii,nini),"'ii' was not found in 'names(lili)'");
    }
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
comp3dtriang <- function(ddel,ste=1,pos=TRUE)
#TITLE (00) develops a delay
#DESCRIPTION
# returns a list with the days and proportions
# to apply to each of them. When \code{ddel} and 
# \code{ste} are interger, the \code{$xval} are
# also integer.
#DETAILS
# The function \code{seq} is used to compute \code{$xval}.
#KEYWORDS misc
#PKEYWORDS 
#INPUTS
#{ddel}<<(\code{numeric(3)}) Definition of the delay 
# (first day, maximum day, last day).>>
#[INPUTS]
#{ste} <<(\code{numeric(1)}) Definition of the
# progression.>>
#{pos} <<(\code{logical(1)}) When TRUE the window
# is first expanded of \code{ste} each side to give
# non null \code{$xval}.>>
#VALUE
# A list with two components. \code{$xval} gives
# the values; \code{prop} gives the proportion
# to apply to each of them (\code{sum($prop)=1}.
#EXAMPLE
#REFERENCE 
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_11_13
#REVISED 09_11_13
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(ddel,"numeric",3,"Bad delay");
    if (!all(diff(ddel)>=0)) {erreur(ddel,"must be non decreasing");}
    if (all(diff(ddel)==0)) {erreur(ddel,"cannot be constant");}
    check4tyle(ste, "numeric",1,"Bad ste");
    if (ste<0) {erreur(ste,"must not be negative");}
    check4tyle(pos, "logical",1,"Bad pos");
}
# dealing with pos
if (pos) {
    ddel[1] <- ddel[1] - ste;
    ddel[3] <- ddel[3] + ste;
}
# computing the xval
xval <- seq(ddel[1],ddel[3],ste);
# computing the prop
if (ddel[1]!=ddel[2]) {
    asc <- (xval-ddel[1]) / (ddel[2]-ddel[1]);
} else {
    asc <- rep(Inf,length(xval));
}
if (ddel[3]!=ddel[2]) {
    des <- (xval-ddel[3]) / (ddel[2]-ddel[3]);
} else {
    des <- rep(Inf,length(xval));
}
prop <- pmin(asc,des);
prop <- prop / sum(prop);
# dealing with pos
if (pos) {
    use <- 2:(length(prop)-1);
    prop <- prop[use];
    xval <- xval[use];
}
# returning
list(xval=xval,prop=prop);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
comp3convolu <- function(pro1,pro2=pro1)
#TITLE (00) computes a convoluted distribution
#DESCRIPTION
# From two probability vector associated to
# sequential integers starting from ZERO,
# returns the probability of the sum.
#DETAILS
#PKEYWORDS helpful
#KEYWORDS iteration
#INPUTS
#{pro1}    <<Probability vector of the first variable.
# needs not to be normalized.>>
#[INPUTS]
#{pro2}    <<Probability vector of the second variable>>
#VALUE
# A probability vector of length \code{length(pro1)+length(pro2)-1}.
#EXAMPLE
# comp3convolu(1:10);
# comp3convolu(comp3convolu(0:5),0:5);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_01_21
#REVISED 10_01_21
#--------------------------------------------
{
# checking
if (any(pro1 < 0)) {
    erreur(pro1,"Probability cannot be negative");
}
if (any(pro2 < 0)) {
    erreur(pro2,"Probability cannot be negative");
}
res <- rep(NA,length(pro1)+length(pro2)-1);
ppro <- outer(pro1,pro2);
iind <- outer(1:length(pro1),1:length(pro2),"+") - 1;
for (irr in sjl(res)) {
    res[irr] <- sum(ppro[iind==irr]);
}
# returning
res/sum(res);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
numero <- function(qui,dans,strict=TRUE)
#TITLE (00) returns the positions of identifiers
#DESCRIPTION
# From the complete and ordered list of
# identifiers returns the positions of some of them.
# If they don't belong to the list, an error is issued
# according to \code{strict}.
# However they can be repeated.
#DETAILS
#PKEYWORDS helpful
#KEYWORDS
#INPUTS
#{qui}    <<Identifiers the position of them is looked for.
# Must be a character or a numeric.>>
#{dans}    <<The ordered list of all identifiers.
# Must be a character or a numeric.>>
#[INPUTS]
#{strict} <<Must \code{qui} be a subset of \code{dans}.>>
#VALUE
# A numeric vector of the positions. It is named with the identifiers.
# When the identifier does not belong to the list and \code{strict} is
# FALSE, NA values are returned.
#EXAMPLE
# rs003k("RESET"); # needed only for R checking, to be forgotten
# numero(10:1,1:10);
# numero(c("Z","E","P","L","I","N"),LETTERS);
# numero(c("B","o","F","!"),LETTERS,strict=FALSE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_03_09
#REVISED 10_03_09
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(qui,   c("character","numeric"),-1,"   'qui' is not valid");
    check4tyle(dans,  c("character","numeric"),-1,"  'dans' is not valid");
    check4tyle(strict,c("logical"),             1,"'strict' is not valid");
}
qui  <- as.character(qui);
dans <- as.character(dans);
if (strict) {
    uu <- union(qui,dans);
    if (length(uu)!=length(dans)) {
        erreur(list(setdiff(uu,dans),dans),"Some identifiers does not belong to the list");
    }
}
# computing
res <- as.numeric(outer(qui,dans,"==")%*%sjl(dans));
names(res) <- qui;
res[res==0] <- NA;
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
ss <- function(file,
              message=rbsb.cha0,directory=rbsb.cha0,
              ...)
#TITLE (00) sources an R file
#DESCRIPTION
# Just sourcing \code{file} after adding a \code{directory}
# path and emmetting a \code{message}.
#DETAILS
#PKEYWORDS
#KEYWORDS
#INPUTS
#{file} << The R file to be sourced.>>
#[INPUTS]
#{message} <<An indicating message to display, possibly with a pause
# according to \code{rbsb.mba}.>>
#{directory} << The directory path to add before the file.>>
#{\dots} <<Further arguments to be passed to the source function.>>
#{message} << Message to display on the terminal.>>
#VALUE
# nothing but the actions indicated in the description field are performed
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_03_16
#REVISED 10_03_16
#--------------------------------------------
{
# the file to be sourced
if (isempty(directory)) { fifi <- file;
} else { fifi <- paste(directory,file,sep="/");}
# sourcing
source(fifi,...);
# the message
if (!isempty(message)) {
    if (!rbsb.mba) { pause(message,"pause from s(ource)");
    } else { cat("<<<",message,">>>\n");}
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
file2list <- function(file,path="./",
                      tags=matrix(c("<<",">>",
                                    "[[","]]",
                                    "((","))"),
                                  ncol=2,byrow=TRUE),
                      sep=rbsb.sep0,
                      rsep="no_action",
                      stag=c("/","/"),
                      comment="#",
                      end="(STOP)",
                      beg.skip="(START_SKIPPING)",
                      end.skip="(END_SKIPPING)"
                     )
#TITLE (00) reads a file and transforms it in a list (of lists) of characters
#DESCRIPTION
# reads a conveniently tagged file into nested lists.
# All lists are named lists and the tags give the names of their
# component. The maximum number of nested levels of lists is given by the
# number of rows of the matrix \code{tags}. The corresponding two columns
# give the opening and closing sequence of the tags. Final lists 
# contain \code{character} vectors, each component of them being
# on the same line and/or on the following line (before a new tag).\cr
# All tags must start at the very beginning of a line. All separator
# tags must be used sticked to the list tag.
#DETAILS
# It is compulsory to tag each level of the lists, this implies that 
# when starting a new list, a character vma (see \code{char2vma}
# for the details: vma means vector or matrix or array) is provided meaning 
# that this is the final level for this branch, or a new sublist 
# is started with the corresponding tag, or a new component of the
# list is given at a level less or equal to the present.\cr
# Separator between different character component is given by the
# \code{sep} argument or indicated by the separator tag (\code{stag}), 
# in the last case, it can be different from a leaf list to another.\cr
# Be aware that before reading a line for a character translation,
# all starting and ending spaces are eliminated.\cr
# Lines starting with \code{comment} are escaped to give the possibility 
# to the user to introduce comments for humans within the file.\cr
# A line starting with \code{end} indicates that the file must not
# be read further. A line starting with \code{beg.skip} indicates that
# from there the lines must not be considered before a line starting
# with \code{end.skip}.\cr
#  The tagging of a new [sub]list component can comprise one, two or
# three indications. These must be given without space between them
# (but this is possible within some) and they must be separated from possible
# character information by a least one space (which will be eliminated).\cr
# Separators of the structure vma (\code{xsep} of function \code{char2vma}
# is imposed to be \code{rbsb.sep1}.
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{file} << file which have to be transformed into a list.>>
#[INPUTS]
#{path} << Directory containing the file.>>
#{tags} << Character matrix with two columns indicating the opening and closing
#          tags of the lists at different levels. Its row numbers gives the maximum
#          number of levels. Opening tags must be different.>>
#{sep} << Character sequence used to split the character vectors on every
# line. That is \code{LF} is always considered as a separator.>>
#{rsep} << Indicates if repetitions of \code{sep} must be considered as signicant or not.
#          If \code{no_action} then their repetitions will be ignored
#          if not \code{character(0)} components will be introduced.>>
#{stag} << Two character strings indicating the tag to define different \code{sep} for a given
#           [sub]list.>>
#{comment} <<At the beginning of a line, it indicates that this line must not be
#          considered.>>
#{end} << To indicate the line to stop the reading (must be in first
#                        position).>>
#{beg.skip} << To indicate the line from which skipping
#                        the reading (must be in first position).>>
#{end.skip} << To indicate the line after which skipping
#                        the reading should be stopped (must be in first position).>>
#VALUE
# a list [of lists [of lists [...] ] ] of character (possibly named) vectors
#EXAMPLE
# rs003k("reset"); # for R checking convenience
# sink("list.txt")
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
# file2list("list.txt");
# unlink("list.txt");
#REFERENCE
#SEE ALSO \code{list2file}
#CALLING
#COMMENT
# file2list is the evolution of the former read8list
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 08_12_10
#REVISED 10_04_20
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(file,"character",1,"file must indicate the name of one file");
    #
    check4tyle(path,"character",1,"path must indicate the name of one directory");
    #
    if (path!="") { fifi <- paste(path,file,sep="/");} else { fifi <- file;}
    if (is.na(file.info(fifi)$size)) { erreur(fifi,"This file seems not to exist.");}
    #
    check4tyle(tags,"character",c(2,Inf),"tags must be a CHARACTER matrix with two columns");
    if (!is.matrix(tags)) {erreur(tags,"tags must be a character MATRIX of two columns");}
    if (ncol(tags)!=2) {erreur(tags,"tags must be a character matrix OF TWO COLUMNS");}
    if (length(unique(tags[,1]))!= nrow(tags)) { erreur(tags,"Opening tags are not unique!");}
    #
    check4tyle(sep,"character",1,"sep must indicate one string character");
    if (nchar(sep) == 0) { erreur(sep,"sep must not be an empty string");}
    #
    check4tyle(rsep,"character",1,"rsep must be a character(1)");
    #
    check4tyle(stag,"character",2,"stag must be a character of length two");
    if (any(nchar(stag) == 0)) { erreur(stag,"stag must not have zero length components");}
    if (length(grep(" ",stag)) > 0) { erreur(stag,"stag must not comprise spaces");}
    #
    check4tyle(comment,"character",1,"comment must be a character of length one");
    #
    check4tyle(end,"character",1,"end must be a character of length one");
    #
    check4tyle(beg.skip,"character",1,"beg.skip must be a character of length one");
    #
    check4tyle(end.skip,"character",1,"end.skip must be a character of length one");   
}
#
# reading the proposed file
if (path!="") {file <- paste(path,file,sep="/");}
lu <- readLines(file);
#
# removing the framing spaces and getting the tags
com <- vector("logical",length(lu));
sto <- bsk <- ask <- com;
for (i in sjl(lu)) {
    lu[i] <- form3decadre(lu[i]," "," ");
    if (nchar(lu[i])==0) {
        com[i] <- TRUE;
    } else {
        if (substr(lu[i],1,1)=="#")                    { com[i] <- TRUE;}
        if (substr(lu[i],1,nchar(beg.skip))==beg.skip) { bsk[i] <- TRUE;}
        if (substr(lu[i],1,nchar(end.skip))==end.skip) { ask[i] <- TRUE;}
        if (substr(lu[i],1,nchar(end))==end)           { sto[i] <- TRUE;}
    }
}
# suming up empty, commented, skipped and left lines
sk1 <- apply(outer(which(bsk)  ,sjl(lu),"<="),2,sum);
sk2 <- apply(outer(which(ask)+1,sjl(lu),"<="),2,sum);
nsk <- ((sk1-sk2) < 1);
nco <- !com;
sst <- which(sto);
if (isempty(sst)) { nst <- rep(TRUE,length(lu));
} else { 
    pst <- min(sst);
    if (isempty(pst)) { nst <- rep(TRUE,length(lu));
    } else {
        nst <- rep(FALSE,length(lu));
        if (pst>1) { nst[1:(pst-1)] <- TRUE;}
    }
}
# removing the useless lines
lu <- lu[nco & nsk & nst];
#
# getting the tagged lines 
tagged <- matrix(NA,0,6);
dimnames(tagged) <- list(NULL,c("num","lev","name","leaf","sep","type"));
#
# tagged will contain all useful informations about the tags.
#    num: line number into lu
#    lev: level number of the tag
#    name: name of the tag
#    leaf: "yes" when final branch, "no" if not. The last
#          two columns will be significant only for leaf sub-list.
#    sep: separator string to be used to get the vma character
#    type: indicates the type of the associated sub-sub-...-sub list
#          we are dealing with : "lists" indicates that more
#          levels are present, if not it is a leaf of the structure
#          and \code{rbsb.vma["c","v","V","w","m",...]} one of the type of vma character
#          is expected. 
nbtag <- 0;
for (i in sjl(lu)) {
    lev <- 0;
    # looking for a tag
    for (j in sj(nrow(tags))) { if (lev == 0) {
        pot <- form3decadre(lu[i],tags[j,1],"");
        if (nchar(pot) < nchar(lu[i])) { lev <- j;}
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
                typing <- rbsb.vma["v"];
                lu[i] <- put[2];
            } else {
                # getting the possible explicit separator
                if (substr(put[2],1,nchar(stag[1]))==stag[1]) {
                    # looking for the separator
                    pit <- form3decadre(put[2],stag[1]);
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
                    lu[i] <- paste(ouou[-1],collapse=" ");
                } else {
                    lu[i] <- "";
                }
                pyt <- ouou[1];
                # getting the type of the tag
                tta <- which(pyt==rbsb.vma);
                if (length(tta)==0) {
                    # default type
                    typing <- rbsb.vma["v"];
                } else {
                    # explicite type
                    typing <- names(rbsb.vma)[tta];
                }
            }
        tagged <- rbind(tagged,c(i,lev,put[1],"???",ssep,typing));
        } # end of nchar(put[1]) < nchar(pot)
    } # end of lev > 0
} # end of i in sjl(lu)
#
# in case of no list
if (nrow(tagged) == 0) { return(vector("list",0));}
# detecting the final lists
tagged[,"leaf"] <- "yes";
for (ii in sj(nrow(tagged)-1)) {
    lev1 <- as.numeric(tagged[  ii,"lev"]);
    lev2 <- as.numeric(tagged[ii+1,"lev"]);
    if (lev2-lev1==1) { tagged[ii,"leaf"] <- "no";}
}
# checking the sequential progress of the levels
for (ii in sj(nrow(tagged)-1)) {
    lev1 <- as.numeric(tagged[  ii,"lev"]);
    lev2 <- as.numeric(tagged[ii+1,"lev"]);
    if ((lev2-lev1) > 1) {
        erreur(tagged,"The progression of levels is not accepted: all levels must be introduced");
    }
}
# checking that no leaf tags are followed by another tag
for (ii in sj(nrow(tagged)-1)) { if (tagged[ii,"leaf"]=="no") {
    actuel <- as.numeric(tagged[  ii,"num"]);
    suivan <- as.numeric(tagged[ii+1,"num"]);
    if ((suivan-actuel) != 1) {
        erreur(list(tagged,ii),"In line 'ii', a 'no-leaf' tags is not immediately followed by another tag");
    }
}}
#
if (tagged[1,"lev"] != "1") {
    erreur(tagged,"The first level must be 1");
}
# replacing the introduced NA
for (ii in sjl(lu)) { if (is.na(lu[ii])) { lu[ii] <- "";}}
# constructing the character strings associated to each leaf list
oul1 <- which(tagged[,"leaf"]=="yes");
oul2 <- as.numeric(tagged[oul1,"num"]);
lulu <- character(length(oul1));
oul2 <- c(oul2,length(lu)+1);
for (ss in sjl(lulu)) {
    leaf <- oul1[ss];
    ssep <- tagged[leaf,"sep"];
    for (sss in oul2[ss]:(oul2[ss+1]-1)) {
        nou <- form3decadre(lu[sss]," ");
        if (nchar(nou)>0) {
            if (nchar(lulu[ss])>0) {  
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
for (ll in sj(nrow(tagged))) {
    # getting the name
    leve <- as.numeric(tagged[ll,"lev"]);
    if (niv<leve) {
        if (niv+1!=leve) { rapport("Error(1) found into 'file2test'");}
        vari <- paste(vari,"[['",tagged[ll,"name"],"']]",sep="");
    }
    if (niv>=leve) {
        va <- strsplit(vari,"[['",fixed=TRUE)[[1]];
        vari <- paste(va[1:leve],collapse="[['");
        vari <- paste(vari,"[['",tagged[ll,"name"],"']]",sep="");
    }
    niv <- leve;
    # filling the value
    if (tagged[ll,"leaf"]=="yes") {
        ulul <- ulul+1;
        # getting the character vector
        choc <- strsplit(lulu[ulul],sep,fixed=TRUE)[[1]];
	if (rsep=="no_action") {
	    choc <- choc[choc!=""];
	}
        # building the code to interpret the character vector
        chacha <- char2vma(choc,tagged[ll,"type"],rbsb.sep1);
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
list2file <- function(lili,file,path="./",
                      tags=matrix(c("<<",">>",
                                    "[[","]]",
                                    "((","))"),
                                  ncol=2,byrow=TRUE),
                      stag=c("/","/"),
                      comment="#",
                      comments=character(0)
                     )
#TITLE (00) transforms a list and write it onto a character file
#DESCRIPTION
# The reverse operation of \code{list2file}. The list must be a
# rsbs-list, that is to comply
# some properties : all components of the [sub-]lists must be either
# a list or a [named] character vector/matrix/array. The number of nested list
# must not be greater than the number of rows in matrix \code{tags}.\cr
# Every list component must be named.\cr
# The idea is to get a file readable by \code{file2list} to produce back
# the object \code{lili}.
#DETAILS
# Also, the character strings of the structure must not comprise
# the \code{rbsb.sep0} constant in case, this global constant must be
# conveniently modified. The same for \code{rbsb.sep1}.\cr
# Use is made of the general constant
# \code{rbsb.mck}.
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{lili} << The list structure to be transformed and written in
#          \code{file}.>>
#{file} << file to be written. If it already exist, it will be destroyed and recreated.>>
#[INPUTS]
#{path} << Directory containing the file.>>
#{tags} << Character matrix with two columns indicating the opening and closing
#          tags of the [sub]lists at different levels. Its row numbers gives the maximum
#          accepted number of levels. Opening tags must be different.>>
#{stag} << Two character strings indicating the tagging to define the separator for each
#          character vector.>>
#{comment} <<At the beginning of a line, it indicates that this line must not be
#          considered. This function will introduce its signature at the beginning
#          of the file.>>
#{comments} <<Comments that the user want to be added at the beginning of the file.>>
#VALUE
# Nothing but a file is created when everything is right.
#EXAMPLE
# rs003k("reset");
# uu <- list(A=1:3,
#            B=matrix(letters[1:20],nrow=5,dimnames=list(1:5,c("on","tw","th","fo"))),
#            C=list(a=1:3,b=letters,c=list(final=array(1:8,c(2,2,2)))));
# list2file(uu,"toto.txt");
# unlink("toto.txt");
#REFERENCE
#SEE ALSO  \code{file2list}
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_03_25
#REVISED 10_04_02
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(lili,"list",-1,"lili must be a list");
    #
    check4tyle(file,"character",1,"file must indicate the name of one file");
    #
    check4tyle(path,"character",1,"path must indicate the name of one directory");
    #
    if (is.na(file.info(path)$size)) { erreur(path,"This directory seems not to exist.");}
    #
    check4tyle(tags,"character",c(2,Inf),"tags must be a CHARACTER matrix with two columns");
    if (!is.matrix(tags)) {erreur(tags,"tags must be a character MATRIX of two columns");}
    if (ncol(tags)!=2) {erreur(tags,"tags must be a character matrix OF TWO COLUMNS");}
    if (length(unique(tags[,1]))!= nrow(tags)) { erreur(tags,"Opening tags are not unique!");}
    #
    check4tyle(stag,"character",2,"stag must be a character of length two");
    if (any(nchar(stag) == 0)) { erreur(stag,"stag must not have zero length components");}
    if (length(grep(" ",stag)) > 0) { erreur(stag,"stag must not comprise spaces");}
    #
    check4tyle(comment,"character",1,"'comment' must be a character of length one");
    #
    check4tyle(comments,"character",-1,"'comment' must be a character of any length");
}
#
# opening the proposed file
file <- paste(path,file,sep="/");
sink(file);
# issuing the starting comments
cat(comment,"\n");
cat(comment,"This file was created on",today(),"by list2file of rs00 package");
cat(comment,"\n");
for (ii in sjl(comments)) {
    cat(comment,comment,comments[ii],"\n");
}
cat(comment,"\n");
# processing the list for writing it
if (length(lili) == 0) {
    cat(comment,"The proposed list was of length zero!\n");
    cat(comment,"\n");
} else {
    # exploring the list
    liliex <- explore8list(lili);
    # checking the list-vma nature 
    if(!all(liliex[,"classes"] %in% c("list",
                                      "integer","numeric","character",
                                      "matrix","array"))) {
        erreur(liliex,"The 'lili' list is not composed of list/vector/matrix/array");
    }
    # checking the existence of names for all levels
    if (any(is.na(liliex[,"name"]))) {
        erreur(liliex,"Not all 'lili' components have got a name");
    }
    # checking the levels
    xlev <- max(as.numeric(liliex[,"level"]));
    if (xlev > nrow(tags)) {
        erreur(list(liliex,tags),"'lili' has got too many levels for the proposed 'tags'");
    }
    # ordering the table
    uv <- order(liliex[,"numbers"]);
    # writing down the file
    for (ii in uv) {
        niv <- as.numeric(liliex[ii,"level"]);
        if (niv <=2) { cat(comment);}
        if (niv == 1) { form3line(20,"=",wid=1,imp=TRUE);}
        if (niv ==2) { cat("\n");}
        cat(tags[niv,1],liliex[ii,"name"],tags[niv,2],sep="");
        if (liliex[ii,"classes"] != "list") {
            coco <- get8listcomp(lili,liliex[ii,])[[1]];
            caca <- vma2char(coco,rbsb.sep1);
            cat(stag[1],rbsb.sep0,stag[2],sep="");
            cat(caca$type,"\n");
            cat(paste(caca$character,collapse=rbsb.sep0),"\n");
        } else {
            cat("\n");
        }
    }
}
# closing the file
sink();
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
explore8list <- function(lili)
#TITLE (00) returns the structure of a list in matrix
#DESCRIPTION
# Explores the branches of a list, returning them into a simple
# character matrix: a row for each branch and some columns to describe
# some characteristics.\cr
# The columns are 'number' (the number of this component from its
# branch), "numbers" (the succesions of all numbers leading to 
# this component separated with spaces), "name" (<NA> if does not exist),
# "names" (succession of names separated with spaces),
# "level" (the branch level),
# "class" (the classes of 
# the component separated with spaces).
#DETAILS
# \code{rbsb.sep1} is used to join the names, as a consequence it must
# not be present into the names. In case an error is issued.
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{lili} << The list structure to be explored.>>
#[INPUTS]
#VALUE
# The resulting character matrix (see the description section).
#EXAMPLE
# rs003k("reset");
# uu <- list(A=1:3,
#            B=matrix(letters[1:20],nrow=5,dimnames=list(1:5,c("on","tw","th","fo"))),
#            C=list(a=1:3,b=letters,c=list(final=array(1:8,c(2,2,2)))));
# explore8list(uu);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_04_01
#REVISED 10_04_19
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(lili,"list",-1,"lili must be a list");
}
#
# starting the table
nta <- c("numbers","number","names","name","depth","level","classes");
tab <- matrix(NA,0,length(nta));
dimnames(tab) <- list(NULL,nta);
if (length(lili) == 0) { return(tab);
} else {
    for (ii in sjl(lili)) {
        nom <- names(lili)[ii];
        if (is.null(nom)) { nom <- "<NA>";}
        if (nom=="") { nom <- "<NA>";}
        if (expr3present(rbsb.sep1,nom)) {
            erreur(list(rbsb.sep1,nom),
                   "This name for the list comprises 'rbsb.sep1' which is not accepted by 'explore8list'");
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
	for (ii in sjl(coco)) {
	    nom <- names(coco)[ii];
	    if (is.null(nom)) { nom <- "<NA>";}
	    if (nom=="") { nom <- "<NA>";}
            noms <- paste(tab[qq,"names"],nom,sep=rbsb.sep1);
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
name8list <- function(lili)
#TITLE (00) returns the same list but with names
#DESCRIPTION
# Explores the branches of a list, returning the same valued list
# but adding names to the components if not existing. This is a 
# way to comply one of the two requirements to be a rbsb-list (see 
# \code{is8rbsblist}.
#DETAILS
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{lili} << The list structure to be possibly named.>>
#[INPUTS]
#VALUE
# The resulting named list.
#EXAMPLE
# rs003k("reset");
# uu <- list(list(1:3,4:6),
#            B=matrix(letters[1:20],nrow=5,dimnames=list(1:5,c("on","tw","th","fo"))),
#            list(a=1:3,b=letters,list(array(1:8,c(2,2,2)))));
# name8list(uu);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_04_19
#REVISED 10_04_19
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(lili,"list",-1,"lili must be a list");
}
#
# exploring the list
tata <- explore8list(lili);
if (nrow(tata) == 0) {return(lili);}
#
# looking for missing names and 
# if so, providing them
qq <- which(tata[,"name"]=="<NA>");
while (length(qq)>0) {
    qqq <- qq[1];
    # selecting all concerned names
    qqqq <- tata[tata[,"level"]==tata[qqq,"level"],"numbers"];
    # proposing replacement names
    nana <- form3names(length(qqqq));
    ouou <- strsplit(qqqq[1]," ")[[1]];
    ouou <- ouou[-length(ouou)];
    if (length(ouou) > 0) {
        coco <- paste("lili[[",
                      paste(ouou,collapse="]][["),
                      "]]",sep="");
    } else {
        coco <- "lili";
    }
    coco <- paste("names(",coco,") <- nana;",sep="");
    eval(parse(text=coco));
    #
    tata <- explore8list(lili);
    qq <- which(tata[,"name"]=="<NA>");
}
#
# returning
lili;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
get8listcomp <- function(lili,tata)
#TITLE (00) returns components from a list structure
#DESCRIPTION
# Returns components from a list structure as a one level list.
# The list \code{lili} must have been explored with \code{explore8list}
# and the branch(es) to return are indicated through their lines in the 
# generated table (\code{tata}).
#DETAILS
# Names of the produced list are the stacked names of the initial list 
# \code{lili}.
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{lili} << The list structure components of which have to be extracted.>>
#{tata} << The lines of the table provided by \code{explore8list}.>>
#[INPUTS]
#VALUE
# The resulting list with as many component as indicated rows.
#EXAMPLE
# rs003k("reset");
# uu <- list(A=1:3,
#            B=matrix(letters[1:20],nrow=5,dimnames=list(1:5,c("on","tw","th","fo"))),
#            C=list(a=1:3,b=letters,c=list(final=array(1:8,c(2,2,2)))));
# vv <- explore8list(uu);
# get8listcomp(uu,vv[7,])[[1]];
##
# uu <- list(1:3,
#            matrix(letters[1:20],nrow=5,dimnames=list(1:5,c("on","tw","th","fo"))),
#            list(a=1:3,b=letters,c=list(final=array(1:8,c(2,2,2)))));
# vv <- explore8list(uu);
# get8listcomp(uu,vv[7,])[[1]];
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
if (rbsb.mck) {
    check4tyle(lili,"list",-1,"lili must be a list");
    #
    check4tyle(tata,"character",-1,"'tata' must be a 'character'");
    if (!is.matrix(tata)) {
        check4tyle(tata,"character",length(nta),"When not a matrix, 'tata' must have the same length that 'nta'");
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
for (ii in sj(nrow(tata))) {
    coco <- paste("lili[[",
                  paste(strsplit(nunu[ii],rbsb.sep0)[[1]],
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
set8listcomp <- function(x,lili,tata)
#TITLE (00) modifies one component from a list structure
#DESCRIPTION
# Replaces one component from a list structure which must not be a list
# (neither before or after the replacement).
# The list \code{lili} must have been explored with \code{explore8list}
# and the branch to modify is indicated through its line in the 
# generated table by \code{tata}.
#DETAILS
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{x} << The object to be inserted in place of the former one.>>
#{lili} << The list structure a component of which is to be modified.>>
#{tata} << The line of the table provided by \code{explore8list}
#          indicating the component to replace.>>
#[INPUTS]
#VALUE
# The resulting list
#EXAMPLE
# rs003k("reset");
# uu <- list(A=1:3,
#            B=matrix(letters[1:20],nrow=5,dimnames=list(1:5,c("on","tw","th","fo"))),
#            C=list(a=1:3,b=letters,c=list(final=array(1:8,c(2,2,2)))));
# vv <- explore8list(uu);
# set8listcomp(LETTERS,uu,vv[7,]);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_04_08
#REVISED 10_04_08
#--------------------------------------------
{
# in case of
if (length(lili)==0) {
    return(vector("list",0));
}
nta <- c("numbers","number","names","name","depth","level","classes");
# checking
if (rbsb.mck) {
    check4tyle(lili,"list",-1,"lili must be a list");
    #
    check4tyle(tata,"character",length(nta),"'tata' must be a 'character' of length length(nta)");
    #
    if (tata[length(nta)] == "list") {erreur(tata,"the component to replace must not be a list");}
}
# preparation
tata <- matrix(tata,1,length(nta));
dimnames(tata) <- list(NULL,nta);
#
# replacing the desired component
coco <- paste("lili[[",
                  paste(strsplit(tata[1,"numbers"]," ")[[1]],
                        collapse="]][["),
                  "]]",sep="");
coco <- paste(coco,"<- x;");
eval(parse(text=coco));
# returning
lili;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
char2vma <- function(cha,quoi=rbsb.vma["v"],
                     xsep=rbsb.sep1,nat="C")
#TITLE (00) transforms a character into a vector (or matrix, or array)
#DESCRIPTION
# from a \code{character} vector, returns a vector, or a matrix, or
# an array of characters with possibly names, or dimames. The information
# can be supplied in different ways for each of the three possibilities.
#DETAILS
# The processing is done in character mode but the result can be
# transformed into numerical values with the help of argument \code{nat}.
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{cha} << The character to transform.>>
#[INPUTS]
#{quoi} << Indicates which structure to return.
#  For vectors, the possibilities are :
#    \code{rbsb.vma["c"]} for a no named character(1); collapsing
#             is done with \code{rbsb.sep0}.
#    \code{rbsb.vma["C"]} for a no named character() of any length
#             (components are separated with \code{xsep} whiwh are
#              removed from the result); collapsing
#             is done with \code{rbsb.sep0}.
#    \code{rbsb.vma["v"]} for a no named vector;
#    \code{rbsb.vma["V"]} for a named vector with
#          all names before the values; then an even number
#          of components must be provided.
#    \code{rbsb.vma["u"]} for a named vector with
#          names interlaced with the value (name_i, value_i); then an even number
#          of components must be provided.
#  For matrix, the possibilities are
#    \code{rbsb.vma["m"]} for a no named matrix given by row, two adjacent rows
#          being separated with \code{xsep} sequence, introduced as one of the
#          component of \code{cha}, then for a 2x3 matrix, the length of \code{cha}
#          will be 6+2 = 8.
#    \code{rbsb.vma["n"]} for a matrix with only the columns names. The expected sequence is
#          the names of columns, then the values as for \code{rbsb.vma["m"]}; then for a 2x3
#          matrix, the length of \code{cha} will be 3+1+8=12.
#    \code{rbsb.vma["o"]} for a matrix with only rows named. The expected sequence is
#          name of row, values of rows... Then 2x3 => a length of 8+2=10.
#    \code{rbsb.vma["p"]} when names for columns and rows, in a mixed way... Then 2x3 =>
#          a length of 14.
#    When \code{rbsb.vma["M"],rbsb.vma["N"],rbsb.vma["o"],rbsb.vma["P"]}, the same but the matrix will be transposed after
#          being read; said in another way, the values are given by columns.
#  For an array, the possibilities are
#    \code{rbsb.vma["a"]} for a no named array, the dimensions, \code{xsep}, the values in
#    the classical order (varying the first index the fastest). 2x3 will give
#    a length of 2+1+6=9.
#    \code{rbsb.vma["A"]} for a named array, the dimension, \code{xsep}, the names of each
#    dimension in the right order also separated and finished with \code{xsep}. 
#    2x3 will gives a length of 2+1+2+1+3+1+6=16.
#{xsep} << Character sequence used to separate the character vector into blocks
#    giving information about the structure (see the examples).
#{nat} << Nature of the returned vector. Can be \code{C} for character, \code{N}
#         for numeric or \code{L} for logical.
#VALUE a vector or a matrix or an array according to the inputs
#EXAMPLE
# rs003k("reset"); # For R checking convenience
## vectors
# char2vma(letters,"c");
# char2vma(letters,"C",xsep="e");
# char2vma(letters);
# char2vma(letters,"V");
# char2vma(letters,"u");
# char2vma(c(LETTERS,letters),rbsb.vma["V"]);
# char2vma(c("A","a","B","b","C","c"),rbsb.vma["u"]);
## matrices
# char2vma(c(1:3,"//",4:6),rbsb.vma["m"]);
# char2vma(c(1:3,"//",4:6),rbsb.vma["M"]);
# char2vma(c(LETTERS[1:3],"//",1:3,"//",4:6),rbsb.vma["n"]);
# char2vma(c(LETTERS[1:3],"//",1:3,"//",4:6),"N");
# char2vma(c("a",1:3,"//","b",4:6),"o");
# char2vma(c(c(LETTERS[1:3],"//","a",1:3,"//","b",4:6)),rbsb.vma["p"]);
## arrays
# char2vma(c(2:4,"//",1:24),"a");
# char2vma(c(2:4,"//","one","two","//",LETTERS[1:3],"//",letters[1:4],"//",1:24),"A");
# char2vma(c(2:4,"//","one","two","//",LETTERS[1:3],"//",letters[1:4],"//",1:24),"A",nat="N");
#REFERENCE
#SEE ALSO  \code{vma2char}
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_03_28
#REVISED 10_04_07
#--------------------------------------------
{
# of use
ssep <- which(cha==xsep);
# checking
if (rbsb.mck) {
    check4tyle(cha,"character",-1,"cha must be a character");
    #
    check4tyle(quoi,"character",1,"quoi (character(1)) must indicate the type of desired output");
    if (!(quoi %in% rbsb.vma)) {
        erreur(quoi,"'quoi' not in the list of possible values...");
    }
    #
    check4tyle(xsep,"character",1,"must indicate the character string of separation");
    #
    if (quoi %in% c(rbsb.vma["V"],rbsb.vma["u"])) {
        if ((length(cha) %% 2) != 0) {
            erreur(list(cha,quoi),"Here the length of 'cha' must be even");
        }
    }
    #
    if (quoi %in% c(rbsb.vma["n"],rbsb.vma["N"],rbsb.vma["m"],rbsb.vma["M"])) {
        if (length(ssep) > 1) {
            nbc <- ssep[1] - 1;
            nbr <- (length(cha)+1) / (nbc+1);
            if (length(cha) != (nbc*nbr+nbr-1)) {
                erreur(list(quoi,length(cha),nbc,nbr),"Dimensions not consistent");
            }
        }
    }
    #
    if (quoi %in% c(rbsb.vma["o"],rbsb.vma["O"],rbsb.vma["p"],rbsb.vma["P"])) {
        if (length(ssep) > 1) {
            nbc <- ssep[1] - 1;
            X <- length(cha);
            if (quoi %in% c(rbsb.vma["p"],rbsb.vma["P"])) {
                X <- 2+X;
                nbc <- nbc+2;
            }
            if ((X %% nbc) != 0) {
                erreur(list(quoi,length(cha),nbc),"Dimensions not consistent");
            }
        }
    }
    #
    if (quoi %in% c(rbsb.vma["a"],rbsb.vma["A"])) { 
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
        X <- length(didi) + 1 + prod(didi);
        if (quoi == rbsb.vma["A"]) { X <- X + sum(didi) + length(didi); }
	if (length(cha) != X) {
            erreur(list(cha,quoi,xsep),"Inconsistency for an array");
	}
    }
    #
    check4tyle(nat,"character",1,"'nat' must be a character(1)");
}
#
res <- rbsb.cha0;
#
if (quoi %in% c(rbsb.vma["c"])) {
    # a single character must be returned
    # rbsb.sep0 is used as collapsor
    res <- paste(cha,collapse=rbsb.sep0);
}
#
if (quoi %in% c(rbsb.vma["C"])) {
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
if (quoi %in% c(rbsb.vma["v"],rbsb.vma["V"],rbsb.vma["u"])) {
    # a vector must be returned
    if (quoi == rbsb.vma["v"]) {
        res <- cha;
    } else {
        nb <- floor(length(cha)/2);
        if (quoi == rbsb.vma["V"]) { nam <- rep(c(TRUE,FALSE),each=nb);
        } else { nam <- rep(c(TRUE,FALSE),nb); }
    res <- cha[!nam];
    names(res) <- cha[nam];
    }
}
#
if (quoi %in% c(rbsb.vma["m"],rbsb.vma["n"],rbsb.vma["o"],rbsb.vma["p"],
                rbsb.vma["M"],rbsb.vma["N"],rbsb.vma["O"],rbsb.vma["P"])) {
    # a matrix must be returned
    nbc <- ssep[1] - 1;
    cha <- c(cha,xsep);
    if (quoi %in% c(rbsb.vma["p"],rbsb.vma["P"])) {
        cha <- c(" ",cha);
    }
    if (quoi %in% c(rbsb.vma["p"],rbsb.vma["P"])) { add <- 1;
    } else { add <- 0; }
    cha <- matrix(cha,ncol=nbc+1+add,byrow=TRUE);
    cha <- cha[,-ncol(cha),drop=FALSE];
    if (quoi %in% c(rbsb.vma["m"],rbsb.vma["M"])) {
        res <- cha;
    }
    if (quoi %in% c(rbsb.vma["n"],rbsb.vma["N"])) {
        res <- cha[-1,,drop=FALSE];
        dimnames(res) <- list(NULL,cha[1,,drop=FALSE]);
    }
    if (quoi %in% c(rbsb.vma["o"],rbsb.vma["O"])) {
        res <- cha[,-1,drop=FALSE];
        dimnames(res) <- list(cha[,1,drop=FALSE],NULL);
    }
    if (quoi %in% c(rbsb.vma["p"],rbsb.vma["P"])) {
        res <- cha[-1,-1,drop=FALSE];
        dimnames(res) <- list(cha[-1,1,drop=FALSE],cha[1,-1,drop=FALSE]);
    }
    if (quoi %in% c(rbsb.vma["M"],rbsb.vma["N"],rbsb.vma["O"],rbsb.vma["P"])) {
        res <- t(res);
    }
}
#
if (quoi %in% c(rbsb.vma["a"],rbsb.vma["A"])) {
    # an array must be returned
        didi <- cha[1:(ssep[1]-1)];
        didi <- as.numeric(didi);
    if (quoi == rbsb.vma["a"]) {
        res <- array(cha[-(1:ssep[1])],dim=didi);
    }
    if (quoi == rbsb.vma["A"]) {
        nbdi <- length(didi);
        res <- array(cha[-(1:ssep[1+nbdi])],dim=didi);
        ndi <- vector("list",0);
        for (jj in sjl(didi)) { if (ssep[jj+1]-ssep[jj]>1) {
            ndi[[jj]] <- cha[(ssep[jj]+1):(ssep[jj+1]-1)];
        }}
        dimnames(res) <- ndi;
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
#TITLE (00) transforms a vector (or matrix, or array) into a character
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
# rs003k("reset"); # For R checking convenience
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
#REVISED 10_03_29
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    #
    check4tyle(xsep,"character",1,"must indicate the character string of separation");
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
        for (hh in sjl(nna)) {
            if (is.null(nna[[hh]])) {
                nna[[hh]] <- sj(dim(x)[hh]);
            }
        }
        for (ii in sjl(nna)) {
            res[[1]] <- c(res[[1]],nna[[ii]],xsep);
        }
        res[[2]] <- rbsb.vma["A"];
    }
    res[[1]] <- c(res[[1]],as.character(x));
} else {
    if (is.matrix(x)) {
        # dealing with a matrix
        if (is.null(dimnames(x))) {
            res[[1]] <- character(0);
            for (ii in sj(nrow(x))) {
                res[[1]] <- c(res[[1]],x[ii,],xsep);
            }
            res[[1]] <- res[[1]][-length(res[[1]])];
            res[[2]] <- rbsb.vma["m"];
        } else {
            nna <- dimnames(x);
            for (hh in sjl(nna)) {
                if (is.null(nna[[hh]])) {
                    nna[[hh]] <- sj(dim(x)[hh]);
                }
            }
            res[[1]] <- c(as.character(nna[[2]]),xsep);
            for (ii in sj(nrow(x))) {
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
#TITLE (00) checks whether a list is rbsb-compatible list or not
#DESCRIPTION
# A rbsb-list is a list with
#{i} <<All components and sub-components being named.>>
#{ii} <<All components and sub-components being either a list
# or a character vector/matrix/array. Vma components are called
# leafs of the rbsb-list.>>\cr
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
# Also the global variable \code{rbsb.is8rbsblist} is modified
# to give the result by a \code{des} object.
#EXAMPLE
# rs003k("reset");
# uu <- list(A=1:3,
#            B=matrix(letters[1:20],nrow=5,dimnames=list(1:5,c("on","tw","th","fo"))),
#            C=list(a=1:3,b=letters,c=list(final=array(1:8,c(2,2,2)))));
# is8rbsblist(uu);
# is8rbsblist(list(uu));
# print(rbsb.is8rbsblist,quoi="c");
# is8rbsblist(list(uu,nu=NULL));
# print(rbsb.is8rbsblist,quoi="c");
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
    check4tyle(lili,"list",-1,"lili must be a list");
}
# exploring the list
eee <- explore8list(lili);
# checking the presence on unamed components
#          and the types of the list
comment <- rbsb.cha0;
for (ii in sj(nrow(eee))) {
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
