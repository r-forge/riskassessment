
###########################################
######## NEW OBJECT des
###########################################


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8des <- function(object)
#TITLE (f3) checks a /des/
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
    } else { erreur(res,w=TRUE);}
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
    orig=paste("Created by rs0"),
    time="unknown",
    defi="undefined",
    role=character(0),
    comm=character(0)),
                validity=valid8des
        );
#

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8des <- function(x,...,quoi="ndr",empha=1)
#TITLE (f3) prints a des object
#DESCRIPTION
#   This function prints in a interpreted way a /des/ object.
#DETAILS
# Global constant rbsb3k("mwi") is used to justify the paragraphes.
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
#check3rbsb(ds,"ds");
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
                    wid=rbsb3k("mwi"),sep=sep,ed=ed);
}}}
if (expr3present("o",quoi)) {
if (!isempty(x@orig)) {
if (x@orig != "") {
    form3paragraphe(c(tt[3],x@orig),empha,
                    wid=rbsb3k("mwi"),sep=sep,ed=ed);
}}}
if (expr3present("t",quoi)) {
if (!isempty(x@time)) {
if (x@time != "") {
    form3paragraphe(c(tt[4],x@time),empha,
                    wid=rbsb3k("mwi"),sep=sep,ed=ed);
}}}
if (expr3present("r",quoi)) {
if (!isempty(x@role)) {
if (x@role != "") {
    form3paragraphe(c(tt[5],x@role),empha,
                    wid=rbsb3k("mwi"),sep=sep,ed=ed);
}}}
if (expr3present("c",quoi)) {
if (!isempty(x@comm)) {
if ((x@comm[1] != "")|(length(x@comm)>1)) {
    form3paragraphe(c(tt[6],x@comm),max(0,empha),
                    wid=rbsb3k("mwi"),sep=sep,ed=ed);
}}}
form3repete(sou,nbs,TRUE);
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#
#


###########################################
######## NEW OBJECT faux
###########################################
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8faux <- function(object)
#TITLE (f3) checks a /faux/
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
#REVISED 09_09_29
#--------------------------------------------
{
    res <- character(0);
    if ((object@level != 0) & (object@level != 1) & (object@level != 2)) {
        res <- "faux@level must be 0, 1 or 2";
    }
    res <- c(res,valid8des(as(object,"des")));
    #
    if (length(res)==0) { res <- TRUE;
    } else { erreur(res,w=TRUE);}
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
print8faux <- function(x,...,print=1)
#TITLE (f0) prints a faux object
#DESCRIPTION
#   This function prints in a interpreted way a /faux/ object.
#DETAILS
# Global constant rbsb3k("mwi") is used to justify the paragraphes.
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
# uu <- new("faux",orig="ici",defi="Fatal Error",comm=c("In fact it was a joke","No error occurred!","You must be relieved"));
# print(uu,print=2);
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

setMethod("print",signature(x = "faux"),print8faux)
setMethod("print",signature(x = "des"), print8des)
