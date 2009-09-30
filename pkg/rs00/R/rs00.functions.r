
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rbsb3k <- function(whi="msi",wha='cval',val=NULL)
#TITLE (f0) standard constants
#DESCRIPTION
# defines/returns/prints the constants used within /rbsb/. 
# The performed action depends on the first argument.
# The set involved depends on the second argument.\cr
# Be aware that there is no checks about the modification
# of values... Not recommended for a standard use.
#DETAILS
# This solution was adopted to replace
# a set of global constants that I was not
# able to make acceptable with R packages standards.
# It is recommended to use rbsb3k to change the values of
# the constants, but this can be directly done with \code{options}.
#PKEYWORDS helpful
#KEYWORDS misc
#INPUTS
#[INPUTS]
#{whi}    <<(='msi') a character(1) indicating which subset of /rbsb/ 
#            constants must be updated/returned/printed.
# \code{'*'} for all;
# \code{'m'} for the miscellaneous subset;
# \code{'f'} for the file subset;
# \code{'c'} for the coding subset;
# \code{'n'} for the null object subset.
# \code{'s'} for the statistics sunset.
# \cr And constant name
# for individual subsets (with three characters).>>
#{wha} <<(='cval') What to do with the designated subset.
# can be \code{'std'} then the standard values are
# imposed to the designated subset of constants.
# Or \code{'print'} then the designated subset of constants is 
# printed.  Or \code{'list'}, then the designated subset of constants
# is returned as a named list. Or \code{'cval'} then the current
# value is returned (only one constant). Or \code{'nval'} then
# the new value is imposed to the one constant precised with \code{whi'}.
# >>
#{val} <<(=NULL) any object to be imposed to the designated constant
# by \code{whi} when \code{wha} is \code{'nval'}.>>
#VALUE
# According to \code{wa} nothing (with possibly a printing), a list, an objet.
#EXAMPLE
## to get the short labels
# names(rbsb3k("*","list"));
## to obtain the standard values
# rbsb3k('*','std');
## to print the current values
# rbsb3k('*','print');
## to impose a new /rbsb/ signature
# rbsb3k('msi','nval','my rebastaba');
# rbsb3k('msi','print');
## to modify the current indentation
# rbsb3k("min","nval",123);
# rbsb3k("min","print");
## any object can be involved
# rbsb3k('*','std');
# is.matrix(rbsb3k("cpt"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_09_16
#REVISED 09_09_29
#--------------------------------------------
{
#
# definition of the different constants
sc <- character(0);
sc["msi"] <- "The signature of rebastaba";
sc["min"] <- "Number of spaces for indentation";
sc["mba"] <- "Must batch be activated (= no pause after displaying a result)?";
sc["mfa"] <- "Must fatal error be fatal?";
sc["mwi"] <- "The width (nbr of characters) for printing paragraphs";
sc["mfi"] <- "Must the results be directed to files (if not to the screen)?";
sc["mgr"] <- "Type of graphics files";
sc["ffg"] <- "Last number of the graphics files";
sc["fpx"] <- "Prefix for the resulting files";
sc["fou"] <- "Standard file for text outputs";
sc["cpt"] <- "Different closing parentheses as a dimnamed matrix";
sc["cni"] <- "character(1) to designate the node under consideration";
sc["nlo"] <- "Null value for logical objects";
sc["nnu"] <- "Null value for numeric objects";
sc["nch"] <- "Null value for character objects";
sc["nli"] <- "Null value for list objects";
sc["nfu"] <- "Null value for function objects";
sc["ndf"] <- "Null value for data.frame objects";
sc["nde"] <- "Null value for des objects";
sc["nfa"] <- "Null value for faux objects";
sc["smn"] <- "Minimum number of observations to compute statistics";
sc["sna"] <- "The different natures for random variates";
sc["spr"] <- "The different natures for random variates";
sc["snp"] <- "Properties of the different natures of random variates as a dimnamed matrix";
#
# checking
check4tyle(whi,"character",1);
check4tyle(wha,"character",1);
p_wha <- c("std","nval","print","list","cval");
if (!expr3present(wha,p_wha)) {
    erreur(list(wha,p_wha),"The first argument is not accepted...");
}
#
# getting the designated subset of constants
whis <- character(0);
if ("*"==whi) { whis <- sc;}
if ("m"==whi) { whis <- sc[ 1: 7];}
if ("f"==whi) { whis <- sc[ 8:10];}
if ("c"==whi) { whis <- sc[11:12];}
if ("n"==whi) { whis <- sc[13:20];}
if ("s"==whi) { whis <- sc[21:24];}
if (length(whis)==0) {
    if (sum(names(sc)==whi)==0) {
        erreur(whi,"First argument 'whi' not accepted!");
    }
    whis <- sc[whi]; 
}
nwhi <- names(whis);
if (expr3present(wha,c("cval","nval")) & (length(nwhi)!=1)) {
    erreur(list(wha,whis),"When getting or imposing constants, one and only one is required!");
}
#
# changing the subset of constants
if (expr3present(wha,c("std","nval"))) {
    # imposing the standard values
    if (expr3present("msi",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.msi="/rbsb0.1-0/");
        } else { options(rbsb.msi=val); }
    }
    if (expr3present("min",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.min=3);
        } else { options(rbsb.min=val); }
    }
    if (expr3present("mba",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.mba= TRUE);
        } else { options(rbsb.mba=val); }
    }
    if (expr3present("mfa",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.mfa= TRUE);
        } else { options(rbsb.mfa=val); }
    }
    if (expr3present("mwi",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.mwi=  70);
        } else { options(rbsb.mwi=val); }
    }
    if (expr3present("mfi",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.mfi=TRUE);
        } else { options(rbsb.mfi=val); }
    }
    if (expr3present("mgr",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.mgr="pdf");
        } else { options(rbsb.mgr=val); }
    }
    if (expr3present("ffg",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.ffg=0);
        } else { options(rbsb.ffg=val); }
    }
    if (expr3present("fpx",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.fpx=paste("rbsb",format(Sys.time(), "%y_%m_%d"),sep="."));
        } else { options(rbsb.fpx=val); }
    }
    if (expr3present("fou",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.fou=paste(options()$rbsb.fpx,"txt",sep="."));
        } else { options(rbsb.fou=val); }
    }
    if (expr3present("cpt",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.cpt= matrix(c("{{","}}",
                                                     "[[","]]",
                                                     "[" ,"]" ,
                                                     "<<",">>"),ncol=2,
                                  byrow=TRUE,dimnames=list(c("nodes","rounding","variables","vectors"),c("opening","closing"))));
        } else { options(rbsb.cpt=val); }
    }
    if (expr3present("cni",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.cni= "*Y*");
        } else { options(rbsb.cni=val); }
    }
    if (expr3present("nlo",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.nlo= logical(0));
        } else { options(rbsb.nlo=val); }
    }
    if (expr3present("nnu",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.nnu= numeric(0));
        } else { options(rbsb.nnu=val); }
    }
    if (expr3present("nch",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.nch= character(0));
        } else { options(rbsb.nch=val); }
    }
    if (expr3present("nli",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.nli=    vector("list",0));
        } else { options(rbsb.nli=val); }
    }
    if (expr3present("nfu",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.nfu= function(...){invisible()});
        } else { options(rbsb.nfu=val); }
    }
    if (expr3present("ndf",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.ndf= as.data.frame(matrix(0,0,0)));
        } else { options(rbsb.ndf=val); }
    }
    if (expr3present("nde",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.nde= new("des"));
        } else { options(rbsb.nde=val); }
    }
    if (expr3present("nfa",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.nfa= new("faux"));
        } else { options(rbsb.nfa=val); }
    if (expr3present("smn",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.smn= 30);}
        } else { options(rbsb.smn=val); }
    if (expr3present("sna",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.sna=c("conti","integ","cateo","categ","unkno"));}
        } else { options(rbsb.sna=val); }
    if (expr3present("spr",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.spr=c("categoric","ordered","numeric"));}
        } else { options(rbsb.spr=val); }
    if (expr3present("snp",nwhi,TRUE)) {
        if (wha=="std") { options(rbsb.snp= 
          matrix(c(FALSE,FALSE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE),5,3,dimnames=list(c("conti","integ","cateo","categ","unkno"),c("categoric","ordered","numeric"))
             ));}
        } else { options(rbsb.snp=val); }
    }
    return(invisible());
} else {
    # loading the current values in a list
    lili <- vector("list",0);
    fax <- function(lili,qui,nwhi) {
        if (expr3present(qui,nwhi,TRUE)) { lili[[qui]] <- options()[[paste("rbsb.",qui,sep="")]];}
        lili
    }
    lili <- fax(lili,"msi",nwhi);
    lili <- fax(lili,"min",nwhi);
    lili <- fax(lili,"mba",nwhi);
    lili <- fax(lili,"mfa",nwhi);
    lili <- fax(lili,"mwi",nwhi);
    lili <- fax(lili,"mfi",nwhi);
    lili <- fax(lili,"mgr",nwhi);
    lili <- fax(lili,"ffg",nwhi);
    lili <- fax(lili,"fpx",nwhi);
    lili <- fax(lili,"fou",nwhi);
    lili <- fax(lili,"cpt",nwhi);
    lili <- fax(lili,"cni",nwhi);
    lili <- fax(lili,"nlo",nwhi);
    lili <- fax(lili,"nnu",nwhi);
    lili <- fax(lili,"nch",nwhi);
    lili <- fax(lili,"nli",nwhi);
    lili <- fax(lili,"nfu",nwhi);
    lili <- fax(lili,"ndf",nwhi);
    lili <- fax(lili,"nde",nwhi);
    lili <- fax(lili,"nfa",nwhi);
    lili <- fax(lili,"smn",nwhi);
    lili <- fax(lili,"sna",nwhi);
    lili <- fax(lili,"spr",nwhi);
    lili <- fax(lili,"snp",nwhi);
    # exploiting the list
    if (wha=="cval") { return(lili[[nwhi]]);}
    if (wha=="list") { return(lili);}
    if (wha=="print") {
        for (ii in sjl(lili)) {
            cat("(((*)))  [",nwhi[ii],"]  --> ",whis[ii],"\n",sep="");
            print(lili[[nwhi[ii]]]);
        }
        return(invisible());
    }
}
#
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sj <- function(nb)
#TITLE (f0) trick for loops
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
#TITLE (f0) trick for loops
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
#TITLE (f0) returns the formatted list of a series of names
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
#TITLE (f0) inverse function of form3liste
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
#TITLE (f0) extracts the contents of parentheses from a character
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
#TITLE (f0) (sch belongs to ch)
#DESCRIPTION
# When \code{vect} is FALSE, returns TRUE if the 
# character string sch is included at least one
# time into the character string ch.\cr
# When \code{vect} is TRUE, returns TRUE when one
# of the component of the vector \code{ch} is 
# identical to \code{sch};
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{sch} <<the character string to be found.>>
#{ch}  <<the character string where to look for>>
#[INPUTS]
#{exact} <<(=FALSE) When exact, one component must
# be strictly identical, if not a subtring is sufficient.
#VALUE TRUE or FALSE
#EXAMPLE
# expr3present('a','non');
# expr3present('n','non');
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
# some checking
check4tyle(sch,"character", 1);
check4tyle( ch,"character",-1);
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
#TITLE (f0) formats a character string
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
#TITLE (f0) prints a repeated given string
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
#TITLE (f0) surrounds a character string
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
form3decadre <- function(chaine,bef=" ",aft=bef)
#TITLE (f0) remove character before and after a character string
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
#VALUE
# character string after removing
#EXAMPLE
#  form3decadre('IMPORTANT','IM',' ANT');
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_12_10
#REVISED 08_12_10
#--------------------------------------------
{
# checking
check4tyle(chaine,"character",c(0,1));
check4tyle(bef,"character",1);
check4tyle(aft,"character",1);
# null case
if (length(chaine) == 0) { return(chaine);}
# removing at the beginning of the string
lb <- nchar(bef);
if (lb>0) {
    repeat {
        deb <- substr(chaine,1,lb);
        if (deb == bef) { chaine <- substring(chaine,lb+1);
        } else { break;}
    }
}
# removing at the end of the string
la <- nchar(aft);
if (la>0) {
    repeat {
        lc <- nchar(chaine);
        fin <- substr(chaine,lc-la+1,lc);
        if (fin == aft) { chaine <- substring(chaine,1,lc-la);
        } else { break;}
    }
}
# returning
chaine;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3ind <- function(niv=1,cr=TRUE,ele=" ")
#TITLE (f0) provides indentation of different levels
#DESCRIPTION
# The level of indentation is given by \code{niv*rbsb3k("min")} times
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
res <- paste(res,form3repete(ele,niv*rbsb3k("min")),sep="");
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3names <- function(nbn,nom=character(0),prefix="")
#TITLE (f0) provides systematic names for nodes
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
#{nom} <<}) Already present names (to avoid identical names).>>
#{prefix} << Systematic prefix of all names to generate. Must
#                 comprise the dot, if one wants suh a separator
#                 between it and the specific part of the name. 
#                 Of course can be underscore or whatever else.>>
#VALUE
# vector with nbn different strings associated to new names
#EXAMPLE
# form3names(2);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_19
#REVISED 08_10_20
#--------------------------------------------
{
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
    } else { mama <- max(1*outer(nom,LETTERS,"==") %*% matrix(1:26,ncol=1));}
    if (nbn < (27-mama)) {
        # adding letters 
        res <- LETTERS[mama+(1:nbn)];
    } else {
        # adding numbered nodes
        ajou <- 0; nu <- 1; res <- character(0);
        while ( ajou < nbn ) {
          nono <- paste("N",nu,sep="");
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
#TITLE (f0) print a separator line from a given pattern
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
#TITLE (f0) adds a character vector into a file
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
#TITLE (f0) prints or prepares a title
#DESCRIPTION
# prints a character string "tit"
# with more or less emphasis
#DETAILS
#PKEYWORDS
#KEYWORDS print
#INPUTS
#{tit}<<the title to print>>
#[INPUTS]
#{empha} << Level of emphasize.</>
#         -1: single line without carriage return</>
#          0: single line</>
#          1: underlined</>
#          2: underlined and overlined</>
#          3: 2 + 1 line before</>
#          4: 3 + 1 line after</>
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
#REVISED 08_08_28
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
#TITLE (f0) prints or prepares paragraphes
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
#TITLE (f0) displays with its name the object x
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
# nothing but a print (or cat) is done
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
#REVISED 09_03_25
#--------------------------------------------
{
cat("<< Displaying ",deparse(substitute(x))," >>\n");
if (cat) { cat(x,"\n");
} else { print(x,...);}
if (pau) { pause("affichage");}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
geom3lmargin <- function(x,ma=0.15)
#TITLE (f0) adds some padding to a range
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
#TITLE (f0) transforms cartesian coordinates in polar coordinates
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
#TITLE (f0) transforms polar coordinates in cartesian coordinates
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
#TITLE (f0) interpolation in R to the 2
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
#TITLE (f0) transforms a numeric value into a 
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
#TITLE (f0) opens the graph device for rebastaba
#DESCRIPTION
# According to the global constant rbsb3k("mfi") a
# graphical device is open or not.
# Must be called before plotting something ones want
# to keep under rbsb3k("mgr") type. This global constant 
# can take only value in c("pdf","png").\cr
#DETAILS
# The file opened for storing the graph is named with
# three components separated with dots: rbsb3k("fpx"), prefix 
# and rbsb3k("mgr"). rbsb3k("fpx") is a prefix to be modified by 
# the user as convenient. rbsb3k("ffg") is the number of the
# current figure incremented by this function. rbsb3k("mgr") is
# the suffix associated to the type of graph (either 'pdf'
# or 'png').
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#[INPUTS]
#{prefix} << When NULL the \code{1+rbsb3k("ffg")} numeric is taken
#                to give the number of the file with three
#                digits. If not, it is a character giving
#                the complete prefix to use before the suffix.>>
#{\dots} << argument(s) to be transmitted to the openning device
#         of graphics. Quite useful for specific character and
#         picture sizes, or to get more than one graph into
#         the file.>>
#VALUE
# Nothing but when rbsb3k("mfi") is \code{TRUE} the
# and \code{is.null(prefix)}, the graphical device is open and the global 
# constant rbsb3k("ffg") is increased *before* with one.
#EXAMPLE
# rbsb3k('*','std');
# rbsb3k('ffg','print');
# open8graph();
# close8graph();
# rbsb3k('ffg','print');
#REFERENCE
#SEE ALSO close8graph
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_10
#REVISED 09_09_18
#--------------------------------------------
{
if (rbsb3k("mfi")) {
    if (is.null(prefix)) {
        rbsb3k("ffg","nval",rbsb3k("ffg") + 1);
        prefix <- paste(rbsb3k("fpx"),form3justifie(rbsb3k("ffg"),nbc=4,
                                               format=3,tronc=FALSE,
                                               carac="0"),
                        sep=".");
    }
    fifi <- paste(prefix,rbsb3k("mgr"),sep=".");
    gopen <- FALSE;
    if (rbsb3k("mgr") == "pdf") { pdf(fifi,...); gopen <- TRUE;}
    if (rbsb3k("mgr") == "png") { png(fifi,...); gopen <- TRUE;}
    if (!gopen) {
        erreur(rbsb3k("mgr"),
               "This type of graph is not yet implemented in /rbsb/");
    }
}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
close8graph <- function(message=rbsb3k("msi")) 
#TITLE (f0) close the file open by open8graph
#DESCRIPTION
# According
# to the global constant rbsb3k("mfi") close the file
# open by open8graph. Also if \code{!rbsb3k("mba")}
#  a pause is issued.
#DETAILS
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#[INPUTS]
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
#CREATED 07_09_29
#REVISED 07_10_25
#--------------------------------------------
{
if (rbsb3k("mfi")) { dev.off();}
if (!rbsb3k("mba")) { pause(message,"pause from close8graph");
} else { cat("<<<",message,">>>\n");}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
open8text <- function(append=TRUE) 
#TITLE (f0) opens the standard output text for rebastaba
#DESCRIPTION
# According to the global constant rbsb3k("mfi") the
# standard output text of rebastaba is open (in
# append mode) or not. The name of this file is provided
# by the constant rbsb3k("fou").
# Must be called before printing something ones want
# to keep on file. 
#DETAILS 
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#[INPUTS]
#{append} << Must the current file rbsb3k("fou") be continued ?>>
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
if (rbsb3k("mfi")) { sink(rbsb3k("fou"),append);}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
close8text <- function(message=rbsb3k("msi")) 
#TITLE (f0) pauses (and more) the program until an answer is given
#DESCRIPTION
# Closing the output file rbsb3k("fou") according to rbsb3k("mfi"). A pause allowing 
# to stop the process is issued if rbsb3k("mba") is false.
#DETAILS
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#[INPUTS]
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
#CREATED 07_09_29
#REVISED 07_10_25
#--------------------------------------------
{
if (rbsb3k("mfi")) { sink();}
if (!rbsb3k("mba")) { pause(message,"pause from close8text");
} else { cat("<<<",message,">>>\n");}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
depeche <- function(what="???",answer=TRUE) 
#TITLE (f0) issues a message on the screen [and returns the answer]
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
#TITLE (f0) pauses the program until an answer is given
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
#TITLE (f0) issues an error message and concludes accordingly
#DESCRIPTION
# when called this function prints x, then displays a message before stopping 
# the process except if it is a warning or if the general constant
# of rebastaba rbsb3k("mfa") is true.
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
# ## Not run:
# erreur("Are you sure of it?",w=TRUE);
# ## End(Not run)
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_03
#REVISED 08_12_05
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
if (w) {
    cat(rbsb3k("msi"),"SIMPLE WARNING:",message,"\n");
} else {
    cat(rbsb3k("msi"),"ERREUR FATALE\n");
    cat("      ",message,"\n");
    form3repete("~",60,TRUE);
    if (rbsb3k("mfa")) { stop("stopped by rebastaba");}
}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rapport <- function(origine)
#TITLE (f0) issues an error message when rebastaba fails
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
    message <- new("ds",
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
#TITLE (f0) provides a suitable Bugs transcription for translation into 
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
#TITLE (f0) provides the generation function from gc and ft
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
#TITLE (f0) to avoid difficulty with is.null
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
# isempty(numeric(0));
# isempty(NULL);
# isempty(rbsb3k("nfa"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_15
#REVISED 09_09_22
#--------------------------------------------
{
  if (is.null(x))                           { return(TRUE);}
  if (length(x)==0)                         { return(TRUE);}
  if (identical(x,rbsb3k("nfa")))           { return(TRUE);}
  if (identical(x,rbsb3k("nde")))           { return(TRUE);}
  FALSE;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
today <- function(format=c("red","nor"))
#TITLE (f0) returns a character giving the present day
#DESCRIPTION  returns a character giving the present day
# The day is given as aa_mm_dd numbers
#DETAILS
# based on Sys.Date R function
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#[INPUTS]
#{format} <<"red" for reduced and "nor" for normal.>>
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
#REVISED 09_09_07
#--------------------------------------------
{
res <- as.character(Sys.Date());
if (format[1]=="red") {
    res <- paste(strsplit(res,"-")[[1]],collapse="_");
    res <- substr(res,3,10);
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
read8list <- function(file,path="./",
                      unique="=",several=":",
                      lpa=c("<<",">>"),
                      end="(STOP)",
                      beg.skip="(START_SKIPPING)",
                      end.skip="(END_SKIPPING)"
                     )
#TITLE (f0) read a list of lists of characters from a file
#DESCRIPTION
# read from 'file' a list of sublists, each being a character vector.
# The first level components of the list are detected from the
# opening and closing brackets provided by 'lpa', each of these 
# component is a also a list (that is one of the sublists).
# The names between the brackets 'lpa' provides names for the 
# list components.
# The components of the sublist are named or unamed character
# vectors. They also have names which are given by the character 
# before 'unique' or 'several' indicators.\cr
# What is after these indicators is got has a unique or several
# pieces of characters. The way it is done is indicating in the
# DETAILS section.\cr
# Empty list are not accepted, a fatal error is issued.
#DETAILS
# Lines starting with the name of a sublist can be more than one 
# in that case a concatenation is performed. The difference between
# 'unique' and 'several' lines is that you can afford no more than 
# one new element with 'unique' and as many you want with 'several'.
# The counter part is that the element can be named with unique 
# and not with 'several'.\cr
# The names of the elements are given between parenthesis between
# the name of the sublist and the 'unique' indicator. See the EXAMPLE 
# section for more insights.\cr
# Starting and ending spaces are always eliminated with 'unique' and 'several'
# lines. Then the character string is returned as one element of the character
# vector in case of 'unique'. In case of 'several', it is split according
# to the character string which is between parenthesis (with ' ' as
# default) before 'several'. See the example section. 
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{file} << file which have to be interpreted.>>
#[INPUTS]
#{path} << Directory containing the file.>>
#{unique} << 'Chararcter indication' to distinguish the component names
#               (on the right) and the contents (on the left) for the characters.>>
#{several} << 'List indication' to distinguish the component names
#                (on the right) and the series of values (on the left) for the 
#                list of characters.>>
#{lpa} << Parentheses to use to detect the component of the list.
#        They cannot be ''.>>
#{end} <<") The line to stop the reading (must be in first
#                        position).>>
#{beg.skip} <<") The line from which skipping
#                        the reading (must be in first position).>>
#{end.skip} <<") The line after which strop skipping
#                        the reading (must be in first position).>>
#VALUE
# a list of lists of character (possibly named) vectors
#EXAMPLE
# sink("list.txt")
# cat("# comments can be included as well\n")
# cat("<<A>>\n");
# cat("a(1)= un deux trois\n");
# cat("b(1)= un uno one\n");
# cat("b(2)= deux dos two\n");
# cat("b(3)= trois tres three\n");
# cat("<<B>>\n");
# cat("a: un deux trois\n");
# cat("# the following three are interesting\n");
# cat("b(/): un / uno / one\n");
# cat("b: deux dos two\n");
# cat("b: trois tres three\n");
# cat("<<C>>\n");
# cat("a: un deux trois\n");
# cat("a= un uno one\n");
# cat("a= deux dos two\n");
# cat("a= trois tres three\n");
# sink();
# read8list("list.txt");
# unlink("list.txt");
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 08_12_10
#REVISED 09_09_26
#--------------------------------------------
{
# checking
check4tyle(lpa,"character",2,"Parentheses must be given as character(2)");
#
tags <- c(unique,several,lpa[1:2],end,beg.skip,end.skip);
if (!all(nchar(tags)>0)) {
   erreur(form3liste(tags),"these tags must comprise at least one character");
}
if (several==unique) {
   erreur(c(several,unique),"unique and several indicators must be different");
}
# reading the proposed file
if (substr(path,nchar(path),nchar(path))!="/") {
    path <- paste(path,"/",sep="");
}
file <- paste(path,file,sep="");
lu <- readLines(file);
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
# taking into account the deleted lines
lu <- lu[nco & nsk & nst];
#
com <- vector("character",length(lu));
for (i in sjl(lu)) {
    pot <- form3decadre(lu[i],lpa[1],"");
    if (nchar(pot) < nchar(lu[i])) {
        put <- strsplit(pot,lpa[2])[[1]];
        if (nchar(put) < nchar(pot)) {
            com[i] <- put;
        }
    }
}
cli <- which(com!="");
lli <- length(cli);
if (lli == 0) {
    erreur(list(file,lpa),"No one list component was found");
}
names(cli) <- com[cli];
if (cli[1] != 1) {
    erreur(cli,"The first list component is not in the first non eliminated line.");
}
# creating the first level list
res <- vector("list",lli);
names(res) <- names(cli);
# filling each component of the list
for (ll in sj(lli)) {
    res[[ll]] <- vector("list");
    # determining the lines to investigate
    ldeb <- cli[ll] + 1;
    if (ll < lli) { lfin <- cli[ll+1] - 1;
    } else { lfin <- length(lu);}
    # dealing with each line
    if (ldeb<=lfin) { for (li in ldeb:lfin) {
        lulu <- lu[li];
        # is it a character or a list?
        supl <- strsplit(lulu,several)[[1]][1];
        lsupl <- nchar(supl);
        supc <- strsplit(lulu,unique)[[1]][1];
        lsupc <- nchar(supc);
        cara <- (nchar(supl) > nchar(supc));
        # doing accordingly
        if (cara) {
            # looking for a possible name
            nca <- expr3extrait(supc,"(",")");
            if (length(nca) > 1) {
                erreur(supc,"Two or more names detected for a character!");
            }
            if (length(nca) == 1) {
                supc <- strsplit(supc,"(",fixed=TRUE)[[1]][1];
                if (nchar(supc)==0) {
                    erreur(nca,"doesn't have a proper name for the sub_list");
                }
            }
            contenu <- substring(lulu,lsupc+nchar(unique)+1);
            contenu <- form3decadre(contenu," "," ");
            res[[ll]][[supc]] <- c(res[[ll]][[supc]],contenu);
            if (length(nca)==1) {
                # giving the detected name
                names(res[[ll]][[supc]])[length(res[[ll]][[supc]])] <- nca;
            }
        } else {
            # looking for a possible separator
            nse <- expr3extrait(supl,"(",")");
            if (length(nse) > 1) {
                erreur(supc,"Two or more separators detected for a list!");
            }
            if (length(nse) == 1) {
                supl <- strsplit(supl,"(",fixed=TRUE)[[1]][1];
                if (nchar(supl)==0) {
                    erreur(nse,"doesn't correspond to a proper name for the sub_list");
                }
                dupli <- TRUE;
            } else {
                nse <- " "; dupli <- FALSE;
            }
            contenu <- substring(lulu,lsupl+nchar(several)+1);
            contenu <- form3decadre(contenu," "," ");
            element <- strsplit(contenu,nse,fixed=TRUE)[[1]];
            # eliminating annoying duplicated spaces
            if (!dupli) {
                res[[ll]][[supl]] <- c(res[[ll]][[supl]],element[element!=""]);
            } else {
                res[[ll]][[supl]] <- c(res[[ll]][[supl]],element);
            }
        }
    }}
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
char2vect <- function(cara,sep=" ",wh="C",mat_sep="//")
#TITLE (f0) transform a single character into a vector 
# (or matrix)
#DESCRIPTION
# Just doing a character or numeric vector from a single
# characters. sep is used to separate the different
# items. wh indicates if the result must be transformed
# with as numeric but before, an attempt is done to returned
# a matrix (instead of a vector) separation subsets of items
# (interpreted as rows). The separation for subset are items
# identical to mat_sep.\cr
# It is important to test the result to know if the transformation
# was indeed performed.
#DETAILS
# Argument 'sep' is a possible separator. If empty
# no splitting is performed. When one of the item is mat_sep,
# then an attempt is done to get a matrix but if the sizes
# of subsets are not identical, an erreur is returned. It then
# must be tested.
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{cara} << The single character to transform.>>
#[INPUTS]
#{sep} <<  Separators for the splitting.
# when '', no splitting is done.>>
#{wh} << Type of conversion: 'C' means character,
#              'N' means numeric.>>
#{mat_sep} << Separator to be used to indicate
#            subsets associated to rows of a matrix. When
#            it is '', no subsetting is performed.>>
#VALUE
# a character (or numeric) vector if everything was good.
# a 'faux' object otherwise.
#EXAMPLE
# char2vect("A B C");
# char2vect("1 2 3"," ","N");
# char2vect("0.1 0.9 // 0.5 0.5 // 0.9 0.1"," ","C");
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 08_12_22
#REVISED 09_01_27
#--------------------------------------------
{
# checking
if (length(cara)>1) {
    return(new("faux",where="char2vect",
           summa="A single character was expected as input"));
}
if (length(cara) == 0) {
    if (wh == "N") { return(numeric(0));
    } else { return(character(0));}
}
if (!is.character(cara)) {
    return(new("faux",where="char2vect",
           summa="A character even null was expected!"));
}
# extracting the vector
if (sep!="") {
    res <- strsplit(cara,sep)[[1]];
    # removing parasite values
    res <- res[res!=""];
} else { res <- cara;}
# putting a matrix structure
if (mat_sep != '') {
    ou <- which(mat_sep==res);
    if (length(ou) > 0) {
        nc <- ou[1]-1;
        if (nc<1) { return(new("faux",where="char2vect",
                               summa="Matrix separator in first position"));}
        if (!all((c(ou,1+length(res)) %% (nc+1))==0)) {
            return(new("faux",where="char2vect",
                       summa="subset of differents lengths",
                       detai=res));
        }
        res <- t(matrix(res[-ou],nrow=nc));
    }
}
#
# further conversion as numeric
if (wh=="N") { 
    if (is.matrix(res)) { didi <- dim(res);
    } else { didi <- c(-10,-10);}
    res <- as.numeric(res);
    if (didi[1] >= 0) { res <- matrix(res,nrow=didi[1],ncol=didi[2]);}
    if (any(is.na(res))) {
        return(new("faux",where="char2vect",
               summa="This vector was supposed to be numeric-like!",
               detai=cara));
    }
}
# returning without error
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
char2list <- function(cara,tw="",wh="C")
#TITLE (f0) transform a character into a list
#DESCRIPTION
# Just doing a list of single (or not) characters from a
# a character vector, taking care of the names
# in a due way.
#DETAILS
# Argument 'tw' is a possible separator. If empty
# (default) only one character each component. If not
# each caracter is splitted with it...
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{cara} << The vector of characters to transform.>>
#[INPUTS]
#{tw} <<  Separators for the second step splitting.>>
#{wh} << Type of conversion: 'C' means character,
#              'N' means numeric.>>
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
#REVISED 08_12_22
#--------------------------------------------
{
# checking
check4tyle(cara,"character",-1);
# extracting the list
res <- vector("list",length(cara));
names(res) <- names(cara);
for ( nn in sjl(cara)) {
    recu <- char2vect(cara[nn],tw,wh);
    if (class(recu)=="faux") {
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
#TITLE (f0) double print at the screen and into a text file
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
easyp2code2 <- function(eas,transfo=FALSE,bugs=FALSE)
#TITLE (f0) transforms an easyp expression into an R/bugs block
#DESCRIPTION
# Numeric values are accepted as easyp expressions. Length(eas)
# can be greater than one, in that case a matrix comprising
# as many columns will be returned. Repetition cases is supposed
# to be already introduced with more than one component.
# Rounding and transformation are options.\cr
# The code where Bugs code is constructed must be updated
#DETAILS
# In rebastaba perspective, this function must be called to 
# define the parameters of the standard nodes like "normal",
# "Bernoulli", "multinomial" and so on... But the analysis
# of the different types of parents is supposed to be 
# already done, and eas accordingly prepared. For instance
# repeated parents are already extended. This gives the opportunity
# for easyp2code2 to belong to the zero (independent from /rbsb/ 
# objects) level.\cr
# Checking of parentship and so on are supposed to be already 
# done before the calling to easyp2code2. Also is supposed already
# detected and expanded the necessary repetitions.\cr
# Notice that the code includes some constant for other uses.
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{eas} <<either a numeric or a rebastaba expression (character).
#      Its length can be greater than one for repeated standard
#      scalar distributions or vector parameters of other
#      distributions.>>
#[INPUTS]
#{transfo} << Are rounding and transformation accepted?
#            In that case, correspond elements are replaced with
#            Y or Y[,i] according to the length of eas.>>
#{bugs} << Must the code be bugs or R (default). To
#         be done.>>
#VALUE
# An interpretable character string to be included
# when generating code (see the following examples).
#EXAMPLE
#REFERENCE
#SEE ALSO easyp2code1
#CALLING
#COMMENT
# the bugs case is to be made.
#FUTURE
# later on for the example section
# easyp2code2(1234)                   # "{cbind(rep(1234,nrow(X)));}" 
# easyp2code2("1234")                 # "{cbind(rep(1234,nrow(X)));}" 
# easyp2code2("2*pi")                 # "{cbind(rep(2*pi,nrow(X)));}" 
# easyp2code2("1+sqrt({{A}})")        # "{cbind(1 + sqrt(X[,'A']))};" 
# easyp2code2(11:12)                  # "{cbind(rep(11,nrow(X)),rep(12,nrow(X)))}"
#AUTHOR J.-B. Denis
#CREATED 08_09_09
#REVISED 09_03_31
#--------------------------------------------
{
# constants for construction
opaco <- "cbind(rep("; cpaco <- ",nrow(X)))";
opapa <- "X[,'"; cpapa <- "']";
if (bugs) {
    erreur('easyp2code2',"sorry, this functionality not yet implemented");
}
# checking and going to the character style
check4tyle(eas,c("numeric","character"),-1);
if (is.numeric(eas)) { eas <- as.character(eas); }
#
# dealing with empty easyp code: character(0) or only spaces.
# Is this possibility relevant?
if (length(eas) == 0) { return("");}
RET <- 0;
for (i in sjl(eas)) {
    if (eas[i] == paste(rep(" ",nchar(eas[i])),collapse="")) { RET <- RET+1;}
}
if (RET == length(eas)) { return("");}
#
##########################################
##########################################
##########################################
ax <- function(dd) {
    # some checkings
    if (transfo) { admi <- 1:5; # every type
    } else { admi <- c(2:3,5);} # neither itself nor rounding
    if (length(dd$blo) == 0) {
        form3affiche(eas);
	rapport("An empty easyp expression was not detected by easyp3trouve!");
    }
    if ((1 %in% dd$typ) & !transfo) {
	erreur(eas,"At this level, the node itself is not expected!");
    }
    if ((4 %in% dd$typ) & !transfo){
	erreur(eas,"At this level, no rounding is expected!");
    }
    if (length(unique(c(dd$typ,admi))) != length(admi)) {
	form3affiche(dd);
        form3affiche(eas);
	rapport("Check this piece of easyprogramming!");
    } 
    # translating
    nbb <- length(dd$typ);
    res <- rep("",nbb);
    vecteur <- FALSE;
    for ( ip in sj(nbb)) {
        quoi <- dd$typ[ip];
        if (quoi == 1) {
            res[ip] <- "Y"; 
            vecteur <- TRUE;
        }
        if (quoi == 2) {
            res[ip] <- paste(opapa,dd$blo[ip],cpapa,sep="");
            vecteur <- TRUE;
        }
        if (quoi == 3) {
            res[ip] <- dd$blo[ip];
            vecteur <- TRUE;
        }
        if (quoi == 4) {
            res[ip] <- paste("round(Y,",dd$blo[ip],")",sep="");
            vecteur <- TRUE;
        }
        if (quoi == 5) { res[ip] <- dd$blo[ip];}
    }
    res <- paste(res,collapse="");
    if (!vecteur) {
        # the vector structure must be imposed
        res <- paste(opaco,res,cpaco,sep="");
    }
res;
}
##########################################
##########################################
##########################################
# about the eas to be analyzed
neas <- length(eas);
#
#
# when there is only one component
if (neas == 1) {
    # analyzing the expression
    dd <- easyp3cut(eas);
    # checking and translating
    res <- ax(dd);
    # returning
    res <- paste(form3ind(2,FALSE),"{",
                 form3ind(3),res,";",
                 form3ind(2),"}",
                 sep="");
    return(res);
}
# preparing the coding of the more than one components
rres <- rep("",neas);
# translating each eas component
for (nn in sj(neas)) {
    # analyzing the expression under consideration
    dd <- easyp3cut(eas[nn]);
    # checking and translating
    rres[nn] <- ax(dd);
} # ending the loop over more than one component
# assembling
nive <- 2;
# i.e. surrounding the expression to produce a relevant matrix
# e.g. {matrix(rep(X[,'A'],4),ncol=4);}
rr <- paste(form3ind(0+nive,FALSE),"{",
            form3ind(1+nive),"cbind(",
            sep="");
for (nn in sj(neas)) {
    rr <- paste(rr,form3ind(2+nive),rres[nn],sep="");
    if (nn < neas) { rr <- paste(rr,",",sep=""); }
}
res <- paste(rr,
             form3ind(1+nive),");",
             form3ind(0+nive),"}",
             sep="");
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
var3standard <- function(nvar,bef="-",aft="-")
#TITLE (f0) provides nvar standard names of variables
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
check4tyle <- function(x,typ,len,message=NULL)
#TITLE (f0) checks the type and the length
# of some standard object
#DESCRIPTION
# If not correct, a fatal message is issued
#DETAILS
# 'integer' has not got the meaning in is.integer R
# standard function. 'null' must be understood as
# resulting TRUE with 'isempty'.
#PKEYWORDS
#KEYWORDS error
#INPUTS
#{x} <<object to be checked.>>
#{typ} <<The list of correct types, among
# 'null",'integer','numeric','character','list','any','function'.
# As understood, 'any' implies that no
# check of the type is performed.>>
#{len} <<If length(len)==1, the exact length
# to be checked, if not must be of length for
# the possible range of length(x). When -1,
# no check on the length is performed.\cr
# For data.frame, it is the number of columns.>>
#[INPUTS]
#{message} << Some additional message to be
#            issued before stopping.>>
#VALUE
# Nothing but a fatal error is issued when
# the check fails.
#EXAMPLE
# check4tyle(letters,c("numeric","character"),c(20,30),"!OK");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_05_02
#REVISED 09_06_26
#--------------------------------------------
{
# checking and preparation
types <- c("integer","numeric","character",
           "list","function","any",
           "data.frame","null");
type <- match(typ,types);
if (all(is.na(type))) {
    erreur(list(types,typ),
           "CHECK4TYLE: None of the proposed type was in the list!"
          );
}
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
        if (!isempty(message)) { form3titre(message);}
        erreur(list(x,len),"CHECK4TYLE: 'x' is not of length 'len'");
    }}
} else {
    if ((length(x)<len[1])|(length(x)>len[2])) {
        if (!isempty(message)) { form3titre(message);}
        erreur(list(x,len),"CHECK4TYLE: 'x' has got a length outside 'len'");
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
        if (tt=="list")      { if (is.list(x))      { ty <- TRUE;}}
        if (tt=="data.frame"){ if (is.data.frame(x)){ ty <- TRUE;}}
        if (tt=="null")      { if (isempty(x))       { ty <- TRUE;}}
    }
    if (!ty) {
        if (!isempty(message)) { form3titre(message);}
        erreur(list(type,x),
               "CHECK4TYLE: 'x' does not belong to any types of 'type'!"
              );
    }
}
# returning
return(invisible());
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
list2vect <- function(li,wh="C",name.rm=TRUE)
#TITLE (f0) transforms a list of scalars into a (named) vector
#DESCRIPTION
# All components of the list are supposed to be scalars (if not
# a fatal error is issued. When numeric type is asked and the conversion
# is not possible, NA is returned.
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{li} <<The list to deal with.>>
#[INPUTS]
#{wh} << Type of conversion: 'C' means character,
#              'N' means numeric.>>
#{name.rm} << Indicates when it exists the removing
#            of item "name" (to be consistent with read8list.>>
#VALUE
# a unique vector concatening all components of the list 
# with the desired type. 
#EXAMPLE
# list2vect(list(a="AAA",b="BBB"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_05_03
#REVISED 09_05_03
#--------------------------------------------
{
# checking
if (!is.list(li)) {
    erreur(li,"A list was expected...");
}
# preparing
if (!isempty(names(li))) {
    if (name.rm) {
        ouna <- which(names(li)=="name");
        if (length(ouna) == 1) {
            li <- li[-ouna];
        }
    }
}
if (wh=="N") {
    res <- numeric(length(li));
} else {
    res <- character(length(li));
}
if (!isempty(names(li))) {
    names(res) <- names(li);
}
# processing
for (ii in sjl(li)) {
    item <- li[[ii]];
    if (length(ii)!=1) { 
        erreur(ii,"Must be of length 1");
    }
    if (wh=="N") {
        res[ii] <- as.numeric(item);
    } else {
        res[ii] <- as.character(item);
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
listev <- function(liste,ith)
#TITLE (f0) returns the ith element of a list supposed to be numeric
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


### Les fonctions suivantes ne sont pas encore au point !!!

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
easyp3cut <- function(x,opar=c("(","{","["),cpar=c(")","}","]"))
#TITLE (f0) splits an expression into non nested blocks 
#DESCRIPTION
# x must be a character string which is parsed to 
#   extracts \code{n} type of blocks where n is \code{length(opar)+1}.
#   Something in between \code{opar[i]} and \code{cpar[i]} is coded \code{i},
#   everything else is coded \code{0}. See the examples.
#DETAILS
# The braces can contain more than one character, they must be
# distinct.
#PKEYWORDS syntax
#KEYWORDS utilities
#INPUTS
#{x} <<character string to split; must be of length one.>>
#[INPUTS]
#{opar} <<character defining the opening parentheses.>>
#{cpar} <<character defining the closing parentheses (must 
#         be of length \code{lenght(opar)}.>>
#VALUE
# A list of two equal length vectors:
#{$blo} <<The vector of character strings (braces are taken off)>>
#{$typ} <<Corresponding codes of the blocks>>
#EXAMPLE
# easyp3cut("(a+b)^[2]");
# easyp3cut("abs({{*Y*}})*{{A}}^{{B}}","{{","}}");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_07_11
#REVISED 09_09_08
#--------------------------------------------
{
cat("\n\nLes fonctions suivantes doivent etre mises au point !!!\n\n");
# checking
check4tyle(   x,"character",1);
check4tyle(opar,"character",-1);
ncas <- length(opar);
check4tyle(cpar,"character",ncas);
if (sum(outer(opar,opar,"=="))!=ncas) { erreur(opar,"Identical opening parentheses");}
if (sum(outer(cpar,cpar,"=="))!=ncas) { erreur(cpar,"Identical closing parentheses");}
# ncas is the number of different possible types of parenthezed blocks
# going back to the previous algorithm
ics <- rep(NA,ncas);
blo <- character(0);
if (!is.null(x)) { if (length(x)==0) {x <- character(0);}}
chaine <- x;
fini <- FALSE;
nepa <- TRUE;
if (length(chaine) == 0) { fini <- TRUE;}
# discovering the blocks
while(!fini) {
    # No parenthesis is currently openned, looking for one to be
    # and putting the associated closing parenthesis into quoi
    # ic gives the position of the first one, -1 if no more.
    if (nepa) {
        for (jd in 1:ncas) {
            ics[jd] <- regexpr(opar[jd],chaine,fixed=TRUE)[1];
        }
        if (sum(ics > 0) > 0) {
            ic <- min(ics[ics > 0]);
            qui <- (1:ncas)[ics == ic][1];
            # quoi is the closing parenthesis to find next
            quoi <- cpar[qui];
            # lop/lcp is the length of the opening and closing parentheses
            lop <- nchar(opar[qui]);
            lcp <- nchar(cpar[qui]);
        }
        else {ic <- -1;}
    }
    # Looking for the closing parenthesis of the presently openned
    # ic gives its position
    else {
        ic <- regexpr(quoi,chaine,fixed=TRUE)[1];
    }
    # No new block to be identified
    if ( ic < 0) {
        blo <- c(blo,chaine);
        fini <- TRUE;
    }
    # a block is under discovery
    else {
        # the block is just openned (nepa to FALSE)
        if (nepa) {
            blo <- c(blo,substr(chaine,1,ic-lop+1));
            #blo <- c(blo,substr(chaine,1,ic-lop));
            chaine <- substr(chaine,ic,nchar(chaine));
            nepa <- FALSE;
        }
        # the block must be closed (nepa to TRUE)
        else {
            blo <- c(blo,substr(chaine,1,ic+lcp-1));
            chaine <- substr(chaine,ic+lcp,nchar(chaine));
            nepa <- TRUE;
        }
    }
}
# at this level the blocks were cutted with their braces...
# now coding and preparing the codes and removing the braces
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
easyp3stickback <- function(d)
#TITLE (f0) converse operation of easyp3cut
#DESCRIPTION
# d must be a list with $typ and $blo components
# (see easyp3cut for the details)
#DETAILS
# The braces used are defined into the matrix
# of constants options()$rbsb.cpt. Those used are precised
# in the local vector quels.\cr
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{d} <<d$typ for the type of components, 
#      d$blo for the content of each component.>>
#[INPUTS]
#VALUE
# A character(1) of the reconstituted easyprograming
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
# examples to be proposed
# uu <- easyp3cut("abs({{*Y*}})*{{A}}^{{B}}");
# easyp3stickback(uu);
#AUTHOR J.-B. Denis
#CREATED 09_03_31
#REVISED 09_03_31
#--------------------------------------------
{
cat("\n\nLes fonctions suivantes doivent etre mises au point !!!\n\n");
# checking
if (!("blo" %in% names(d))) {erreur(d,"$blo is missing");}
if (!("typ" %in% names(d))) {erreur(d,"$typ is missing");}
if (length(d$typ)!=length(d$blo)) {erreur(d,"$typ & $blo has got != lengths");}
# reconstituting
nb <- length(d$typ);
res <- "";
for (ii in sjl(d$typ)) {
    tt <- d$typ[ii];
    if ((tt==1)|(tt==2)) {
        res <- paste(res,options()$rbsb.cpt["nodes","opening"],
                         d$blo[ii],
                         options()$rbsb.cpt["nodes","closing"],
                     sep="");
    }
    if (tt==3) {
        res <- paste(res,options()$rbsb.cpt["vectors","opening"],
                         d$blo[ii],
                         options()$rbsb.cpt["vectors","closing"],
                     sep="");
    }
    if (tt==4) {
        res <- paste(res,options()$rbsb.cpt["rounding","opening"],
                         d$blo[ii],
                         options()$rbsb.cpt["rounding","closing"],
                     sep="");
    }
    if (tt==5) {
        res <- paste(res,d$blo[ii],
                     sep="");
    }

}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
unex <- function(xv,di,check=TRUE)
#TITLE (f1) collapsing indices
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
# uu <- data.frame(F1=factor(rep(1:3,each=4)),F2=factor(rep(1:4,3)));
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
stat8corg <- function(xy,nat)
#TITLE (f1) computes correlations between two variables
#DESCRIPTION The two common correlation coefficients are computed
# for two variables.
# TO BE DONE: the natures of the random variables are taken into
# account to accept non valued variables.
#DETAILS
#PKEYWORDS stat
#KEYWORDS
#INPUTS
#{xy} <<matrix with two columns, containing the
#       two variables>>
#{nat} <<the two natures of the two variables>> 
#[INPUTS]
#VALUE
# A vector of two values:
#    (1) the correlation coefficient of Pearson (on the values)
#  & (2) the correlation coefficient of Spearman (on the ranks)
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_07_04
#REVISED 09_08_22
#--------------------------------------------
{
np <- rbsb3k("snp");
res <- c(NA,NA);
if ((var(xy[,1],na.rm=TRUE) > 0) & (var(xy[,2],na.rm=TRUE) > 0)) {
    if (np[nat[1],"numeric"] & np[nat[2],"numeric"]) {
        res[1] <- cor.test(xy[,1],xy[,2],method="pearson",exact=FALSE)$estimate;
    }
    if (np[nat[1],"ordered"] & np[nat[2],"ordered"]) {
        res[2] <- cor.test(xy[,1],xy[,2],method="spearman",exact=FALSE)$estimate;
    }
}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
stat3univ <- function(data,nbmin=30)
#TITLE (f1) computes univariates statistics from a data matrix
#DESCRIPTION
# data is a matrix: repetition by rows, variables by columns
#DETAILS
#PKEYWORDS statistics
#KEYWORDS 
#INPUTS
#{data} <<Matrix with the data. NA values are possible.>>
#[INPUTS]
#{nbmin} <<(=30) Minimum number of observations required to 
#                compute the statistics.>>
#VALUE
# a matrix having a column for each variables and different
# statistics in rows. Dimnames are sufficient to understand
# what it is.
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_10_05
#REVISED 09_09_22
#--------------------------------------------
{
# checking
if (!is.matrix(data) &
    !is.data.frame(data)) {
    erreur(data,"A data MATRIX or a DATA.FRAME was expected!");
}
# preparing the returned matrix
quoi <- c("number","missing",
          "mean","std-dev.","min","Q01%","Q05%","Q10%","Q25%",
          "median","Q75%","Q90%","Q95%","Q99%","MAX");
if (length(quoi) != 15) {
    form3affiche(quoi);
    rapport("MODIFIER LA D'EFINITION DE uu DANS bn2dn ET CE TEST");
}
mar <- matrix(NA,length(quoi),ncol(data));
dimnames(mar) <- list(quoi,dimnames(data)[[2]]);
#
for (jd in sj(ncol(mar))) {
    mar["number", jd] <- sum(!is.na(data[,jd]));
    mar["missing",jd] <- sum(is.na(data[,jd]));
    if (mar["number",jd] > nbmin) {
        mar["mean",jd] <- mean(data[,jd],na.rm=TRUE);
        mar["std-dev.",jd] <- sqrt(var(data[,jd],na.rm=TRUE));
        mar["min",jd] <- min(data[,jd],na.rm=TRUE);
        mar["MAX",jd] <- max(data[,jd],na.rm=TRUE);
        mar[6:14,jd] <- quantile(data[,jd],
                      c(0.01,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.99),
                      na.rm=TRUE);
    }
}
# returning
mar;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
list2des <- function(li,name=rbsb3k("nch"))
#TITLE (f2) transforms a consistent list into a new des object
#DESCRIPTION
# Just analyzing the components of the list
# (consistent names have to be used) which are supposed
# to be character and tackle them to produce consistent
# slots of a /des/
#DETAILS
# The main use of this function is to tackle alk read from text files
# with the function read8list avoiding the NULL values non acceptable
# for slots
#PKEYWORDS des
#KEYWORDS classes
#INPUTS
#{li} <<The list to be transformed into a des object.>>
#[INPUTS]
#{name} <<(=rbsb3k("nch")) gives the name of the node
#         when li$name does not exist. If both are absent
#         then an error is issued.>>
#VALUE
# The generated 'des' object
#EXAMPLE
# print(list2des(list(name="A",role="Just to see",comm="Not very interesting")));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_01_06
#REVISED 09_09_22
#--------------------------------------------
{
# checking
check4tyle(li,"list",-1);
if (is.null(li$name)) {
    if (isempty(name)) {
        erreur(li,"the component 'name' is compulsory");
    } else {
        li@name <- name;
    }
}
if (is.null(li$orig)) { li$orig <- rbsb3k("nch");}
if (is.null(li$time)) { li$time <- rbsb3k("nch");}
if (is.null(li$defi)) { li$defi <- rbsb3k("nch");}
if (is.null(li$role)) { li$role <- rbsb3k("nch");}
if (is.null(li$comm)) { li$comm <- rbsb3k("nch");}
# returning
new("des",name=li$name,orig=li$orig,time=li$time,
          defi=li$defi,role=li$role,comm=li$comm);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
char2des <- function(x)
#TITLE (f1) transforms,if necessary, a single character to a 'des' object
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
    (class(x) != "des")) {
    erreur(x,"Must be 'character(1)' or a 'des' object!");
}
# returning
if (class(x)=="des") { res <- x;
} else {
    res <- new("des",name=x,
               orig=paste("created by",rbsb3k("msi")),
               time=today());
}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
