
rs003k("reset");
rsba3k("reset");
rsgn3k("reset");
rsbn3k("reset");

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rsdn3k <- function(whi)
#TITLE (dn) assigns the constants for the rsdn layer
#DESCRIPTION
# defines or returns the constants used within /rbsdn/. 
# The performed action depends on the argument.
#DETAILS
# All constant names start with 'rbsb.'.
# This solution was adopted to replace
# a set of global constants that were difficult
# to make acceptable with R packages standards.
# It is recommended not to modify these constants
# unless you are completely aware of the consequences.\cr
# The constants can be any object type.
#PKEYWORDS helpful
#KEYWORDS misc
#INPUTS
#{whi}    <<(\code{character(1)} indicating either to reset or
#           to return the names or the current values. The possible
#           values are \code{RESET}, \code{reset}, \code{names}, \code{definitions} or \code{values}.>>
# >>
#[INPUTS]
#VALUE
# When \code{whi=="RESET"} nothing (but the assignments are
# performed for all layers \code{rs00}, \code{rsba}, \code{rsgn} and \code{rsdn}).
# When \code{whi=="reset"} nothing (but the assignments of 
# the layer \code{rsdn} are performed).
# When \code{whi=="names"} the names as a character vector.
# When \code{whi=="definitions"} the definitions as a named character vector.
# When \code{whi=="values"} the values through a named list.
#EXAMPLE
## First assign the standard values
# rsdn3k("RESET");
# print(rbsb.dn2);
## to get the short labels
# rsdn3k("names");
## to obtain the current values
# rsdn3k("values");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_11_25
#REVISED 10_02_01
#--------------------------------------------
{
# checking
if (!expr3present(whi,c("RESET","reset","names","definitions","values"))) {
    print.default(whi);
    stop("rsdn3k does not accept this argument");
}
#
if (whi=="RESET") { rsbn3k("RESET");}
#
# definition of the different constants
sc <- character(0);
sc["dn0"] <- "null /dn/"; 
sc["dn1"] <- "Example 1 of /dn/"; 
sc["dn2"] <- "Example 2 of /dn/"; 
sc["dn3"] <- "Example 3 of /dn/"; 
sc["dn4"] <- "Example 4 of /dn/"; 
sc["emdn"] <- "standard 'quoi' for 'print8dn'"; 
sc["scn"] <- "name for the scoring variable"; 
sc["scv"] <- "4 values for the scoring (cod,red,pod,NA)"; 
sc["mnu"] <- "minimum number of individuals for the computation of univariate statistics";
sc["mnb"] <- "minimum number of individuals for the computation of bibariate statistics";
sc["cdi"] <- "'sig' for categorical distance (used in draw8empidata)";
sc["efi"] <- "file name for storing characteristics of empirical drawings";
sc["eti"] <- "every this value a title is introduced in empirical storage";
sc["ede"] <- "decimal precision for empirical storage";
sc["nar"] <- "minimal ratio of missing values (warnig issued otherwise)";
sc["esu"] <- "must the selected candidates be written in rbsb.efi?";
sc["csv"] <- "coding for the complete set of variables";
sc["sii"] <- "intermediate printing within simulate8bn [0|1|2|3|4]";
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
    assign("rbsb.sii",0,pos=.GlobalEnv);
    assign("rbsb.csv",paste(rbsb.cpt["variables","opening"],
                            rbsb.all,
                            rbsb.cpt["variables","closing"],sep=""),
           pos=.GlobalEnv);
    assign("rbsb.esu",FALSE,pos=.GlobalEnv);
    assign("rbsb.nar",0.3,pos=.GlobalEnv);
    assign("rbsb.ede", 1,pos=.GlobalEnv);
    assign("rbsb.eti",50,pos=.GlobalEnv);
    assign("rbsb.efi","rebastaba.empidat.txt",pos=.GlobalEnv);
    assign("rbsb.cdi",-1,pos=.GlobalEnv);
    assign("rbsb.mnu",15,pos=.GlobalEnv);
    assign("rbsb.mnb",45,pos=.GlobalEnv);
    assign("rbsb.scn",">?<",pos=.GlobalEnv);
    assign("rbsb.scv",c(0,1,3,10),pos=.GlobalEnv);
    assign("rbsb.dn0",new("dn",
                          description=rbsb.des0,
                          nat=rbsb.cha0,    
                          pod=rbsb.lis0,         
                          red=rbsb.lis0,         
                          cod=rbsb.lis0,         
                          df=as.data.frame(matrix(NA,0,0))
                        ),pos=.GlobalEnv);
    assign("rbsb.dn1",new("dn",
                          description=char2des("1rst Example of /dn/"),
                          nat=rep("conti",4),    
                          pod=c(rep(list(c(-10,100)),3),list(c( 0,Inf))),         
                          red=c(rep(list(c(- 5, 80)),3),list(c( 0,90))),         
                          cod=c(rep(list(c(  0, 50)),3),list(c(20,60))),         
                          df=as.data.frame(matrix(round(abs(100*sin(1:40))),10,
                                           dimnames=list(NULL,c(nanv(rbsb.nom1,0),rbsb.scn))))
                        ),pos=.GlobalEnv);
    assign("rbsb.dn2",new("dn",
                          description=char2des("2d Example of /dn/"),
                          nat=rep("conti",5),    
                          pod=c(rep(list(c(-10,10)),4),list(c(0,Inf))),         
                          red=c(rep(list(c(-10,10)),4),list(c(0,50))),         
                          cod=c(rep(list(c(-10,10)),4),list(c(0,30))),         
                          df=as.data.frame(matrix(round(abs(100*sin(1:50))),10,
                                           dimnames=list(NULL,c(nanv(rbsb.nom2,0),rbsb.scn))))
                        ),pos=.GlobalEnv);
    assign("rbsb.dn3",new("dn",
                          description=char2des("3rd Example of /dn/"),
                          nat=rep("conti",7),    
                          pod=c(rep(list(c(-10,10)),6),list(c(0,Inf))),         
                          red=c(rep(list(c(-10,10)),6),list(c(0,50))),         
                          cod=c(rep(list(c(-10,10)),6),list(c(0,30))),         
                          df=as.data.frame(matrix(round(abs(100*sin(1:70))),10,
                                           dimnames=list(NULL,c(nanv(rbsb.nom3,0),rbsb.scn))))
                        ),pos=.GlobalEnv);
    assign("rbsb.dn4",new("dn",
                          description=char2des("4th Example of /dn/"),
                          nat=c(rep("conti",3),rep("categ",3)),    
                          pod=c(list(c(0,50)),
                                list(c(0,100)),
                                list(c(0,100)),
                                list(LETTERS[1:2]),
                                list(LETTERS[1:4]),
                                list(LETTERS[1:4])
                               ),         
                          red=c(list(c(0,20)),
                                list(c(0,50)),
                                list(c(0,100)),
                                list(LETTERS[1:2]),
                                list(LETTERS[1:3]),
                                list(LETTERS[1:4])
                               ),         
                          cod=c(list(c(0,10)),
                                list(c(0,30)),
                                list(c(0,100)),
                                list(LETTERS[1:2]),
                                list(LETTERS[1:2]),
                                list(LETTERS[1:4])
                               ),         
                          df=data.frame(matrix(round(abs(100*sin(1:300))),100,
                                               dimnames=list(NULL,LETTERS[1:3])),
                                        matrix(sample(rep(LETTERS[1:4],75),300),
                                               100,3,
                                               dimnames=list(NULL,LETTERS[4:6]))
                                       )
                        ),pos=.GlobalEnv);
    assign("rbsb.emdn","dr",pos=.GlobalEnv);
}
#
return(invisible());
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

###########################################
###########################################
########
#((((((( NEW S4 CLASS dn
########
###########################################
###########################################

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8dn <- function(object)
#TITLE (dn) checks a /dn/
#DESCRIPTION
#   This function checks /dn/ objects
#DETAILS
# It is the validity method for /dn/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The dn object to be validated.>>
#[INPUTS]
#VALUE
# TRUE when the object seems acceptable
# else a character describing the error(s)
#EXAMPLE
# rsdn3k("RESET"); # for R checking convenience
# valid8dn(rbsb.dn3);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_11_25
#REVISED 10_01_29
#--------------------------------------------
{
    res <- character(0);
    if (length(setdiff(slotNames("dn"),slotNames(object)))) {
        erreur(slotNames(object),paste("Not all slots (",slotNames("dn"),") are present",sep=""));
    }
    #
    rr <- valid8des(object@description);
    if (!identical(TRUE,rr)) { res <- c(res,rr);}
    # checking the consistency of slot lengths
    nbv <- length(object@df);
    varna <- dimnames(object@df)[[2]];
    #
    if (length(object@nat) != nbv) {
        res <- c(res,"length of @nat is different from the variable number");
    }
    if (length(object@pod) != nbv) {
        res <- c(res,"length of @pod is different from the variable number");
    }
    if (length(object@red) != nbv) {
        res <- c(res,"length of @red is different from the variable number");
    }
    if (length(object@cod) != nbv) {
        res <- c(res,"length of @cod is different from the variable number");
    }
    # checking some variable specification
    for (vv in sj(nbv)) {
        if (rbsb.snp[object@nat[vv],"numeric"]) {
            # numeric variable
            if (sum(abs(object@red[[vv]]))==Inf) {
                res <- c(res,paste("Variable",varna[vv],"has got infinite @red"));
            }
            if (sum(abs(object@cod[[vv]]))==Inf) {
                res <- c(res,paste("Variable",varna[vv],"has got infinite @cod"));
            }
        }
    }
    #
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=rbsb.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

###########################################
setClass("dn", representation(
    description="des",    # description of the dn
    nat="character",     # nature of the variables
    pod="list",          # possible       domain of the variables
    red="list",          # representation domain of the variables
    cod="list",          # common         domain of the variables
    df="data.frame"      # result of the simulation (here are the
                         # names of the variables)
                         ));
###########################################

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8dn <- function(x,...,quoi="tu",qui=rbsb.cha0,sorting=rbsb.num0,ndec=3,simu=1:10)
#TITLE (dn) prints a /dn/
#DESCRIPTION
# prints a dn object with more or less details. If the score is wanted,
# first transform the /dn/ with \code{score4dn}.
#DETAILS
#KEYWORDS print
#PKEYWORDS dn
#INPUTS
#{x} <<the dn to print>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.>>
#{quoi} <<(\code{character(1)}) a character string indicating what to print,\cr
#          t: just the size and name,\cr
#          d: description,\cr
#          v: variable names,\cr
#          D: the domains of every variable,\cr
#          i: some individuals as indicated by simu
#             (in the natural order if s is not present)\cr
#          s: indicates that the individuals must be ordered
#             according to the score.\cr
#          u: univariate statistics,\cr
#          b: bivariate statistics,\cr
#          a: all options = tdvDisub\cr
#{qui}<<(\code{character(0:1)}) The variables which must be considered for the printing.
#   The default implies all of them, if not either a character vector 
#   providing the nodes or a numeric vector with the iin (internal numbers 
#   of them). Using this argument is a way to modify the order of displaying
#   the variables when printing.>>
#{sorting}<<(\code{numeric} or \code{character}) indicates the variable upon which the individuals
#             have to be sorted before printing (has got priority with respect to 's' option).
#{ndec} <<(=3) number of decimals used to print the values of the numeric variables.>>
#{simu} <<(=1:10), which simulated values to print?\cr
#          when 0: no simulated values are printed,\cr
#          when a vector: indicates the numbers of simulations to print
#                         (default = the first ten),\cr
#          when a value: the percentage to be printed
#                         (50 = half of the simulations)
#       >>
#VALUE
#  returns nothing but a printing is performed
#EXAMPLE
# rsdn3k("RESET");
# print(rbsb.dn2);
# rbsb.mnu <- 8;
# print(score4dn(rbsb.dn2));
# print(rbsb.dn4,quoi="a");
# print(rbsb.dn4,sorting="A");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_08_24
#REVISED 10_03_09
#--------------------------------------------
{
if (rbsb.mck) {
    if (!identical(TRUE,valid8dn(x))) { erreur(NULL,"Not a valid /dn/"); }
    check4tyle(quoi,"character",1);
    check4tyle(qui,"character",c(0,1));
    check4tyle(sorting,c("character","numeric"),0:1);
    check4tyle(ndec,"numeric",1);
    check4tyle(simu,"numeric",-1);
}
nbva <- ncol(x@df);
nbsi <- nrow(x@df);
if (expr3present("a",quoi)) { quoi <- "tdDvisub";}
vardn <- dimnames(x@df)[[2]];
# sorting variable
if (!isempty(sorting)) {
    quoi <- paste(quoi,"i",sep="");
    vsorting <- which(sorting==vardn);
    if (length(vsorting)==0) {
        erreur(list(vardn,sorting),"The variable indicated for sorting does not exist");
    }
}
# variables to be considered (iin coding)
if (isvide(qui)) { qui <- sjl(vardn);
} else {
    if (is.numeric(qui)) { qui <- numero(qui,sjl(vardn));
    } else {qui <- numero(qui,vardn);}
}
# their translation for numeric and categoric variables
# into variable names
quinum <- vardn[intersect(which(rbsb.snp[x@nat,"numeric"]),qui)];
quicat <- vardn[intersect(which(rbsb.snp[x@nat,"categoric"]),qui)];
quitou <- vardn[qui];
nsou <- 49; # length of some tag line of the printing
form3repete("-",nsou,TRUE);
# the title
if(expr3present("t",quoi)) {
  cat("dn object of name \"",x@description@name,"\"\n",sep="");
  ti <- paste("There are",nbva,"variables and",nbsi,"simulations");
  if (length(qui) < nbva) { ti <- paste(ti," but only ",length(qui),"variables are considered here.");}
  form3titre(ti); 
  form3repete("-",nsou,TRUE);
}
# the size
if (expr3present("d",quoi)) {
    print(x@description,rbsb.emdn);
}
# the variable names
if (expr3present("v",quoi)) {
  cat("  Variable names are:\n");
  for (i in 1:nbva) {
    if (i == 1) { cat("( ");} else {cat(" ; ");}
    cat(vardn[i]);
    if (i == nbva) { cat(" )\n");}
  }
  form3repete("-",nsou,TRUE);
}
# the domains
if (expr3present("D",quoi)) {
  cat("  Domains (of asked variables) are:\n");
  for (i in qui) {
      cat("Variable (",i,"): ",vardn[i],"\n",sep="");
      if (vardn[i] %in% quicat) {
          cat("          possible:",form3liste(x@pod[[i]],opa="",cpa="",sep=";"),"\n");
          cat("    representation:",form3liste(x@red[[i]],opa="",cpa="",sep=";"),"\n");
          cat("            common:",form3liste(x@cod[[i]],opa="",cpa="",sep=";"),"\n");
      }
      if (vardn[i] %in% quinum) {
          cat("          possible:",form3liste(x@pod[[i]],OPA="[",CPA="]",opa="",cpa="",sep=","),"\n");
          cat("    representation:",form3liste(x@red[[i]],OPA="[",CPA="]",opa="",cpa="",sep=","),"\n");
          cat("            common:",form3liste(x@cod[[i]],OPA="[",CPA="]",opa="",cpa="",sep=","),"\n");
      }
  }
  form3repete("-",nsou,TRUE);
}
# simulated values
if (expr3present("i",quoi)) { if (length(quitou)>0) {
    quels <- NULL;
    if (length(simu) == 1) {
        if (simu > 0) {
            simu <- min(100,simu);
            quels <- seq(1,nrow(x@df),length=max(1,round(nrow(x@df)*simu/100)));
            quels <- round(quels);
        }
    }
    else {
        quels <- intersect(simu,1:nrow(x@df));
        if (length(quels) <= 0) { quels <- 1:min(10,nrow(x@df));}
    }
    if (!is.null(quels)) {
        # preparing the data to print
        scoo <- dn2score(x,quitou);
        if (!isempty(sorting)) {
            oo <- order(x@df[[sorting]])[quels];
            sorted <- paste("sorted with respect to",sorting," ");
        } else {
            if (expr3present("s",quoi)) {
                oo <- order(scoo)[quels];
                sorted <- "sorted with respect to the computed score";
            } else {
                oo <- quels;
                sorted <- "";
            }
        }
        cat("\n   ",round(100*length(quels)/nrow(x@df)),
            "% of the ",sorted,"simulated values\n\n",sep="");
        # preparing the table to print
        prdf <- x@df[oo,quitou,drop=FALSE];
        # identifying numerical variables and rounding them
        nunu <- rbsb.snp[x@nat,"numeric"];
        prdf[,nunu] <- round(prdf[,nunu],ndec);
        # at last printing
        print(prdf);
    }
  form3repete("-",nsou,TRUE);
}}
# univariate statistics
if (expr3present("u",quoi)) { if (length(quitou)>0) {
  cat("The univariate statistics\n\n");
  print(dn2ustat(x,qui));
  form3repete("-",nsou,TRUE);
}}
# bivariate statistics
if (expr3present("b",quoi)) { if (length(quitou)>0) {
  cat("The bivariate statistics\n\n");
  print(round(dn2bstat(x,qui),ndec));
  form3repete("-",nsou,TRUE);
}}
invisible();    
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
plot8dnco <- function(x,red,cod,title="",type="smooth",...)
#TITLE (dn) univariate plot for a continuous variable
#DESCRIPTION
# produces the histogram (raw or smoothed) of a continuous variable.
#DETAILS
#PKEYWORDS plot dn
#KEYWORDS
#INPUTS
#{x} <<the continuous variable to be plotted>>
#{red} <<the range of the representation>>
#{cod} <<the common limits to be indicated with vertical dotted lines>>
#[INPUTS]
#{title} <<(\code{character} The title to be given to the plot.>>
#{type} << type of representation. "smooth" means that \code{density}
#          is called else a simple histogram is drawn.>>
#{\dots} <<Further arguments to be passed to the \code{hist} or
#          \code{plot} function. \code{xlim} must not be provided
#          this way since it is already given by \code{red}.>>
#VALUE
#  returns nothing but one plot is drawn
#EXAMPLE
# rsdn3k("RESET"); # for R checking convenience
# plot8dnco(rbsb.dn4@df[[1]],red=rbsb.dn4@red[[1]],cod=rbsb.dn4@cod[[1]]);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_01_04
#REVISED 10_02_12
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(x,"numeric",-1,"A continuous variable is expected",na.rm=TRUE);
    check4tyle(red,"numeric",2,"A range is expected");    
    check4tyle(cod,"numeric",2,"A range is expected");    
}
# plotting
if (type=="smooth") {
    # a smoothed histogram is required
    xx <- density(x,na.rm=TRUE);
    plot(xx,xlim=red,main=title,...);
} else {
    # a simple histogram is required
    hist(x,xlim=red,main=title,...);
}
abline(v=cod,lty=2);
# returning
invisible();    
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
plot8dnca <- function(f,red,cod,title="",type="ordered",...)
#TITLE (dn) univariate plot for a categoric variable
#DESCRIPTION
# produces the histogram (raw or ordered) of a categoric variable.
#DETAILS
#PKEYWORDS plot dn
#KEYWORDS 
#INPUTS
#{f} <<the categoric variable (in fact a factor) to be plotted>>
#{red} <<the range of the representation>>
#{cod} <<the common limits to be indicated with shaded bars>>
#[INPUTS]
#{title} <<(\code{character} The title to be given to the plot.>>
#{type} << type of representation. "ordered" means that the categories
#          will be sorted by decreasing importance.>>
#{\dots} <<Further arguments to be passed to the \code{hist} or
#          \code{plot} function. \code{xlim} must not be provided
#          this way since it is already given by \code{red}.>>
#VALUE
#  returns nothing but one plot is drawn
#EXAMPLE
# rsdn3k("RESET"); # for R checking convenience
# plot8dnca(rbsb.dn4@df[[5]],red=rbsb.dn4@red[[5]],cod=rbsb.dn4@cod[[5]]);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_01_04
#REVISED 10_01_04
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    if (!is.factor(f)) {
        erreur(f,"A factor is expected for a categoric variable");
    }
    check4tyle(red,"character",-1,"A range of character is expected");    
    check4tyle(cod,"character",-1,"A range of character is expected");    
    if (sum(is.na(match(cod,red))) > 0) {
        erreur(list(cod,red),"'cod' is not included into 'red'");
    }
}
# preparing
fdd <- f;
if (type=="ordered") {
    # the order must be proposed according to frequencies
    ff <- table(f);
    oo <- rev(order(ff));
    ooo <- order(oo);
    le <- levels(f);
    ll <- as.numeric(f);
    fdd <- factor(ooo[ll]);
    levels(fdd) <- le[oo];
    red <- red[ooo];
    cod <- cod[ooo];
}
col <- rep("grey",length(red));
col[match(cod,red)] <- "white";
# plotting
plot(fdd,col=col,main=title,...);
# returning
invisible();    
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
plot8dncoco <- function(xx,rred,ccod,title="",type="points",lla=NULL,...)
#TITLE (dn) bivariate plot for two continuous variables
#DESCRIPTION
# produces a Cartesian plot (\code{points}) or an estimated
# density into R^2 (\code{contour}).
#DETAILS
#PKEYWORDS plot dn
#KEYWORDS 
#INPUTS
#{xx} << matrix or data frame with two columns of the two continuous variable to be plotted>>
#{rred} <<list of two components with the ranges of the representation>>
#{ccod} <<list of two components with the common limits to be indicated with vertical dotted lines>>
#[INPUTS]
#{title} <<(\code{character} The title to be given to the plot.>>
#{type} << type of representation. "points" means a scatter plot with symbols,
#          "labels" means scatter plot with text labels and
#          "contour" means the isocontour plot.>>
#{lla} << Defines the labels for the represented points. NULL for standard,
#         If scalar must be the number of character
#{\dots} <<Further arguments to be passed to the \code{plot} or
#          \code{contour} function. \code{xlim} must not be provided
#          this way since it is already given by \code{rred}.>>
#VALUE
#  returns nothing but one plot is drawn
#EXAMPLE
# rsdn3k("RESET"); # for R checking convenience
# plot8dncoco(rbsb.dn4@df[,1:2],rred=list(rbsb.dn4@red[[1]],rbsb.dn4@red[[2]]),ccod=list(rbsb.dn4@cod[[1]],rbsb.dn4@cod[[2]]),xlab="A",ylab="B");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_01_05
#REVISED 10_02_01
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    if (!(is.matrix(xx) | is.data.frame(xx))) {
        erreur(xx,"'xx' must be data frame or matrix");
    }
    if (ncol(xx) < 2) {
        erreur(xx,"'xx' must have at least two columns");
    }
    check4tyle(xx[,1],"numeric",-1,"A continuous first variable is expected",na.rm=TRUE);
    check4tyle(xx[,2],"numeric",-1,"A continuous second variable is expected",na.rm=TRUE);
    check4tyle(rred,"list",c(2,Inf),"A list with at least two components is expected.");
    check4tyle(ccod,"list",c(2,Inf),"A list with at least two components is expected.");
    check4tyle(rred[[1]],"numeric",2,"A continuous first range is expected");    
    check4tyle(rred[[2]],"numeric",2,"A continuous second range is expected");    
    check4tyle(ccod[[1]],"numeric",2,"A continuous first range is expected");    
    check4tyle(ccod[[2]],"numeric",2,"A continuous second range is expected");    
}
# plotting
if ((type=="points") | (type=="labels")) {
    # a simple scatter plot is required
    plot(xx[,1],xx[,2],type="n",
         xlim=rred[[1]],ylim=rred[[2]],
         main=title,...);
    abline(v=ccod[[1]],h=ccod[[2]],lty=2);
    if (type=="points") {
        if (is.null(lla)) { lla <- 1; }
        points(xx[,1],xx[,2],pch=lla);
    } else {
        text(xx[,1],xx[,2],labels=lla);
    }
} else {
    # a contour plot is required
    ff <- kde2d(xx[,1],xx[,2],lims=c(rred[[1]],rred[[2]]));
    contour(ff,...);
    abline(v=ccod[[1]],h=ccod[[2]],lty=2);
}
# returning
invisible();    
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
plot8dncoca <- function(xx,rred,ccod,title="",type="smooth",...)
#TITLE (dn) bivariate plot for a continuous and a categoric variables
#DESCRIPTION
# produces a series of univariate diagram of the continuous variables 
# for each category of the categoric variable
#DETAILS
#PKEYWORDS plot dn
#KEYWORDS 
#INPUTS
#{xx} << data frame with two columns of the continuous variable (in first position) and the categoric variable (in second position) to be plotted>>
#{rred} <<list of two components with the ranges of the representation>>
#{ccod} <<list of two components with the common ranges to be indicated in the diagram.>>
#[INPUTS]
#{title} <<(\code{character} The title to be given to the plot.>>
#{type} << type of representation. "smooth" means density plots
#          else histograms are drawn.>>
#{\dots} <<Further arguments to be passed to the \code{plot} or
#          \code{hist} function. \code{xlim} must not be provided
#          this way since it is already given by \code{rred[[1]]}.>>
#VALUE
#  returns nothing but a series of diagrams (as many as representation categories of the second variable) is drawn.
#EXAMPLE
# rsdn3k("RESET"); # for R checking convenience
# par(mfrow=c(2,1));
# plot8dncoca(rbsb.dn4@df[,c(1,4)],rred=list(rbsb.dn4@red[[1]],rbsb.dn4@red[[4]]),ccod=list(rbsb.dn4@cod[[1]],rbsb.dn4@cod[[4]]),xlab="A",ylab="density",title="Variable D");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_01_05
#REVISED 10_01_12
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    if (!is.data.frame(xx)) {
        erreur(xx,"'xx' must be a data frame");
    }
    if (ncol(xx) < 2) {
        erreur(xx,"'xx' must have at least two columns");
    }
    check4tyle(xx[,1],"numeric",-1,"A continuous first variable is expected");
    if (!is.factor(xx[,2])) {
        erreur(xx,"A factor is expected as second variable");
    }
    check4tyle(rred,"list",c(2,Inf),"A list with at least two components is expected.");
    check4tyle(ccod,"list",c(2,Inf),"A list with at least two components is expected.");
    check4tyle(rred[[1]],"numeric",2,"A continuous first range is expected");    
    check4tyle(rred[[2]],"character",-1,"A categoric second range is expected");    
    check4tyle(ccod[[1]],"numeric",2,"A continuous first range is expected");    
    check4tyle(ccod[[2]],"character",-1,"A categoric second range is expected");    
}
# plotting
for (cate in rred[[2]]) {
    x <- xx[xx[,2]==cate,1];
    if (cate %in% ccod[[2]]) {
        ope <- "((("; clo <- ")))";
    } else {
        ope <- " - "; clo <- " - ";
    }
    ti <- paste(title,ope,cate,clo);
    plot8dnco(x,rred[[1]],ccod[[1]],title=ti,type=type,...);
}
# returning
invisible();    
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
plot8dncaca <- function(xx,rred,ccod,title="",type="joint",...)
#TITLE (dn) bivariate plot for two categoric variables
#DESCRIPTION
# produces an image matrix of the frequencies of the categoric variables.
# Several options are possible to give (i) different orders for the categories,
# (ii) different viewpoint of the probability table (joint, conditional) and 
# (iii) different color scales.
#DETAILS
#PKEYWORDS plot dn
#KEYWORDS 
#INPUTS
#{xx} << data frame with two columns of the categoric variables to be plotted>>
#{rred} <<list of two components with the ranges of the representation>>
#{ccod} <<list of two components with the common ranges to be indicated in the diagram.>>
#[INPUTS]
#{title} <<(\code{character} The title to be given to the plot.>>
#{type} << type of representation. "joint" means joint probability;
#          "row" mean conditional for each row and "column" means
#          conditional for each column.>>
#{\dots} <<Further arguments to be passed to the \code{image} 
#          function.>>
#VALUE
#  returns nothing but a scaled matrix is drawn.
#EXAMPLE
# rsdn3k("RESET"); # for R checking convenience
# plot8dncaca(rbsb.dn4@df[,c(4,5)],rred=list(rbsb.dn4@red[[4]],rbsb.dn4@red[[5]]),ccod=list(rbsb.dn4@cod[[4]],rbsb.dn4@cod[[5]]),title="Variables D & E");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_01_12
#REVISED 10_02_01
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    if (!is.data.frame(xx)) {
        erreur(xx,"'xx' must be a data frame");
    }
    if (ncol(xx) < 2) {
        erreur(xx,"'xx' must have at least two columns");
    }
    if (!is.factor(xx[,1])) {
        erreur(xx,"A factor is expected as first variable");
    }
    if (!is.factor(xx[,2])) {
        erreur(xx,"A factor is expected as second variable");
    }
    check4tyle(rred,"list",c(2,Inf),"A list with at least two components is expected.");
    check4tyle(ccod,"list",c(2,Inf),"A list with at least two components is expected.");
    check4tyle(rred[[1]],"character",-1,"A categoric first range is expected");    
    check4tyle(rred[[2]],"character",-1,"A categoric second range is expected");    
    check4tyle(ccod[[1]],"character",-1,"A categoric first range is expected");    
    check4tyle(ccod[[2]],"character",-1,"A categoric second range is expected");    
}
# preparing
tt <- table(xx[,1:2]);
tt <- tt / sum(tt);
if (type=="row") {
    tt <- tt / apply(tt,1,sum);
}
if (type=="column") {
    tt <- t(t(tt) / apply(tt,2,sum));
}
tit <- paste(title,paste("(",type,")",sep=""));
if (!exists("xlab")) { xlab="";}
if (!exists("ylab")) { ylab="";}
# plotting
image(tt,xaxt="n",yaxt="n",
      xlab=xlab,ylab=ylab,
      main=tit);
axis(1,(0:(nrow(tt)-1)/(nrow(tt)-1)),levels(xx[,1]));
axis(2,(0:(ncol(tt)-1)/(ncol(tt)-1)),levels(xx[,2]));
# returning
invisible();    
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
plot8dn <- function(x,y="useless",...,
                    titles=NULL,stitles=NULL,
                    xxlab=NULL,yylab=NULL,
                    varx=1,vary=0,
                    quels=0,lab=1,shaking=0.100,
                    file="")
#TITLE (dn) Plots the numeric or categoric variables of a dn object
#DESCRIPTION
# produces the (X,Y) plots from a given dn with possible selection
# of variables and simulations. red and cod slots are used. To get a
# better picture for cartesian representations, some shaking can be
# applied to the integ variables.\cr
# When X!=Y in the couple (X,Y) a bivariate representation is issued
# according to the nature of the two variables. When X==Y, then a univariate
# representation is performed.
#DETAILS
# Some 
#PKEYWORDS plot dn
#KEYWORDS
#INPUTS
#{x} <<the rbsb object to plot>>
#[INPUTS]
#{y} << For compatibility with the generic plot function. Useless here.>>
#{\dots} <<Further arguments to be passed to the plot function.>>
#{titles} << Defines the titles to provide on each
#             Cartesian diagram(s) with a character vector
#             cycled as necessary to cover all diagrams.
#             If NULL the name of the dn with pertinent
#             information is issued.>>
#{stitles} << Defines the subtitles to provide on each
#             Cartesian diagram(s) with a character.
#             If NULL standard sub-title.>>
#{xxlab} << As \code{stitles} but for \code{xlab} arguments of the plots.>>
#{yylab} << As \code{xxlab} for \code{ylab}.>>
#{varx} << Indicates the variables to use as abscissae.
#             If 1, all numeric variables of the dn are involved.
#             If 2, all categoric variables of the dn are involved.
#             If 3, all variables of the dn are involved.
#             If not, indicates by their names which variables
#             must be used.>>
#{vary} << Indicates which variables to use as ordinates.
#             If -1, only univariate graphs of \code{varx} are done,
#             If 0, \code{vary} is set identical to \code{varx},
#             If 1, all numeric variables of the dn are involved,
#             If 2, all categoric variables of the dn are involved,
#             If 3, all variables of the dn are involved.
#             For these numerical orders, reciprocal bivariate graphs
#             are discarded.
#             If not numeric, must have the same size as varx
#              providing the names of the variables to use in 
#              ordinates.>>
#{quels} << Indicates the selection of the points to be 
#             displayed. (i) 0 then all simulations are displayed.
#             (ii) A positive integer indicating the number
#             of simulations to sample within the data frame.
#             (iii) Can be a character string and must be on the
#             format "V1==3" indicating that only the simulated 
#             observations for which the variable "V1" has got 3
#             as value will be displayed. So "V3" must exist and
#             probably is an integ or categ variable. Be careful
#             that no spaces are possible, e.g. "V1 == 3" will
#             not work.
#             (iv) Can be a boolean vector with as many components
#             as simulated observations, only those with TRUE will
#             be selected.
#             (v) When its length is zero, no plot is performed.>>
#{lab} << Indicates the labels with which the points must
#             be identified in case of binumeric plots.
#             (i) Can be a numeric scalar: the symbol for localization.
#             (ii) Can be a numerical vector with the same size
#             that the number of rows of x@df, used to indicate
#             which standard symbols of R plot to use as label
#             (in case of selection, only those selected will be
#             used).
#             (iii) Can be a single character: then must indicates a name
#             variables (if continuous the values of it will be used after
#             rounding as labels).
#             (iv) Can be a character vector with the same size
#             that the number of rows of x@df, used as there are
#             (in case of selection, only those selected will be
#             used).>>
#{shaking} << In standard deviation of each variable, the
#             coefficient for giggling the 'integ' variables in order
#             to better see identically plotted points in binumeric plots.>>
#{file} << When not empty produces a text file of this name
#             containing the numbering of all page representations.
#             Quite useful to find a specific diagram when the 
#             diagrams are numerous.>>
#VALUE
#  returns nothing but plots are drawn and possibly a text file is created.
#EXAMPLE
# rsdn3k("RESET"); # for R checking convenience
# plot(rbsb.dn1);
# plot(rbsb.dn1,vary=-1);
# plot(rbsb.dn4,varx="A",vary="B");
# plot(rbsb.dn4,varx="A",vary="D");
# plot(rbsb.dn4,varx="D",vary="A");
# plot(rbsb.dn4,varx="E",vary="F");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_29
#REVISED 10_04_30
#--------------------------------------------
{
if (!identical(TRUE,valid8dn(x))) { erreur(NULL,"Not a valid /dn/"); }
#
# the set of variables
#
varna <- dimnames(x@df)[[2]];
nuva <- rbsb.snp[x@nat,  "numeric"];
cava <- rbsb.snp[x@nat,"categoric"];
names(nuva) <- names(cava) <- varna;
#
# constituting the vector of abscissae (varx)
#
if (is.numeric(varx)) {
    # must be a scalar
    if (length(varx) != 1) { erreur(varx,"A scalar was expected!");}
    vx <- round(varx);
    if (!(vx %in% 1:3)) {erreur(varx,"Either 1, 2 or 3 was expected");}
    if (vx == 1) {
        # only numeric variables
        varx <- varna[nuva];
    }
    if (vx == 2) {
        # only categoric variables
        varx <- varna[cava];
    }
    if (vx == 3) {
        # only numeric or categoric variables
        varx <- varna[nuva | cava];
    }
} else {
    if (!is.character(varx)) { erreur(varx,"Bad varx argument: must be numeric or character");}
    if (length(intersect(varna,varx)) != length(unique(varx))) {
        erreur(list(varna,varx),"'varx' does not comprise only variable names");
    }
}
if (isvide(varx)) { return(invisible());}
#
# constituting the vector of ordinates (vary)
#
if (is.numeric(vary)) {
    # must be a scalar
    if (length(vary) != 1) { erreur(vary,"A scalar was expected!");}
    vy <- round(vary);
    if (!(vy %in% -1:3)) {erreur(vary,"Either -1, 0, 1, 2 or 3 was expected");}
    if (vy == -1) {
        # only univariate graphs
        vary <- character(0);
    }
    if (vy == 0) {
        # all graphs based on varx
        vary <- varx;
    }
    if (vy == 1) {
        # only numeric variables
        vary <- varna[nuva];
    }
    if (vy == 2) {
        # only categoric variables
        vary <- varna[cava];
    }
    if (vy == 3) {
        # only numeric or categoric variables
        vary <- varna[nuva | cava];
    }
    # preparing the couple accordingly
    if (length(vary)>0) {
        vco <- intersect(varx,vary);
        vvx <- setdiff(varx,vco);
        vvy <- setdiff(vary,vco);
        varx <- vary <- character(0);
        # common variables
        if (length(vco) > 0) {
            # univariate graphs
            varx <- c(varx,vco);
            vary <- c(vary,vco);
            # bivariate graphs
            if (length(vco) > 1) {
                for (i in 2:length(vco)) { for (j in 1:(i-1)) {
                    varx <- c(varx,vco[j]);
                    vary <- c(vary,vco[i]);
                }}
            }
        }
        # common x non common couples
        for (i in sjl(vvy)) { for (j in sjl(vco)) {
            varx <- c(varx,vco[j]);
            vary <- c(vary,vvy[i]);
        }}
        # non common x common couples
        for (i in sjl(vco)) { for (j in sjl(vvx)) {
            varx <- c(varx,vvx[j]);
            vary <- c(vary,vco[i]);
        }}
        # non common x non common couples
        for (i in sjl(vvy)) { for (j in sjl(vvx)) {
            varx <- c(varx,vvx[j]);
            vary <- c(vary,vvy[i]);
        }}
        # precaution check
        if (length(varx)!=length(vary)) { rapport("'varx' and 'vary' should be of equal size!");}
    }
} else {
    if (!is.character(vary)) { erreur(vary,"Bad vary argument: must be numeric or character");}
    if (length(intersect(varna,vary)) != length(unique(vary))) {
        erreur(list(varna,vary),"'vary' does not comprise only variable names");
    }
}
#
# number of graphs
#
nbg <- length(varx);
if (rbsb.mck) {
    if (!is.null(titles)) {
        check4tyle(titles,"character",nbg,"A title must be given for each graph, if not NULL");
    }
    if (!is.null(stitles)) {
        check4tyle(stitles,"character",nbg,"A sub-title must be given for each graph, if not NULL");
    }
    if (!is.null(xxlab)) {
        check4tyle(xxlab,"character",nbg,"An xlab must be given for each graph, if not NULL");
    }
    if (!is.null(yylab)) {
        check4tyle(yylab,"character",nbg,"An ylab must be given for each graph, if not NULL");
    }
}
#
# preparing the title
#
if (!is.null(titles)) {
    titi <- as.character(1:nbg);
    titi[] <- titles;
    titles <- titi; 
} else {
    titles <- paste(x@description@name," (",1:nbg,")",sep="");
}
#
# determining which simulations to drawn
#
if (length(quels) <= 0) { return();}
if (length(quels) == 1) {
    if (is.numeric(quels)) { 
        if (quels <= 0) { quels <- 1:nrow(x@df);
        } else {
            nn <- min(quels,nrow(x@df));
	    quels <- sample(1:nrow(x@df),nn);
        }
    } else {
        if (!is.character(quels)) { erreur(quels,"'quels' must be numeric or character");}
        ou <- regexpr("==",quels,fixed=TRUE)[1];
        va <- substr(quels,1,ou-1);
        qu <- substr(quels,ou+2,nchar(quels));
        quels <- which(x@df[,va] == as.numeric(qu));
    }
} else {
    if (length(quels) != nrow(x@df)) {
        erreur(list(length(quels),nrow(x@df)),
               "length of quels must be equal to the number of data");
    }
    quels <- which(quels>0);
    quels <- quels[quels <= nrow(x@df)];
}
#
# preparing the labels
#
if (is.numeric(lab)) {
    pts <- "points";
    if (length(lab) > 1) {
        lla <- lab[quels];
    } else { lla <- lab;}
} else {
    pts <- "labels";
    if (length(lab) > 1) {
        lla <- lab[quels];
    } else {
        if (!(lab %in% varna)) { erreur(list(varna,lab),"lab not a variable!");}
        ilab <- which(lab==varna);
        if (nuva[lab]) {
            lla <- round(x@def[quels,lab]);
        } else {
            lla <- as.character(x@df[quels,lab]);
        }
    }
}
#
# preparing the giggling coefficients for the numeric variables
#
gig <- rep(0,ncol(x@df));
for (i in 1:ncol(x@df)) {
    if (nuva[i]) {
        gig[i] <- shaking * sqrt(var(x@df[quels,i],na.rm=TRUE));
    }
}
#
# performing the plots
#
gg <- 0;
if (file!="") {sink(file);}
if (length(vary) == 0) { for (g in 1:nbg) {
    if (!is.null(xxlab)) { xlab <- xxlab[g];
    } else { xlab <- varx[g];}
    # strict univiariate cases
    nuvx <- which(varx[g]==varna);
    if (file != "") { cat(g,": [",nuvx,"]  (",varx[nuvx],")\n")};
    if (nuva[nuvx]) {
        plot8dnco(x@df[,nuvx],x@red[[nuvx]],x@cod[[nuvx]],
                  xlab=xlab,title=titles[g],sub="(univariate)",...);
    } else {
        plot8dnca(x@df[,nuvx],x@red[[nuvx]],x@cod[[nuvx]],
                  xlab=xlab,title=titles[g],sub="(univariate)",...);
    }
}} else { for (g in 1:nbg) {
    # univiariate and bivariate cases
    if (!is.null(xxlab)) { xlab <- xxlab[g];
    } else { xlab <- varx[g];}
    if (!is.null(yylab)) { ylab <- yylab[g];
    } else { ylab <- vary[g];}
    nuvx <- which(varx[g]==varna);
    nuvy <- which(vary[g]==varna);
    covx <- nuva[nuvx];
    covy <- nuva[nuvy];
    if (nuvx == nuvy) {
        # univariate case
        if (is.null(stitles)) { sti <- "(univariate)";
        } else { sti <- stitles[g];}
        gg <- gg+1;
        if (file != "") { cat(gg,": [",nuvx,"]  (",varx[nuvx],")\n")};
        if (covx) {
            plot8dnco(x@df[,nuvx],x@red[[nuvx]],x@cod[[nuvx]],
                      xlab=xlab,title=titles[g],sub="(univariate)",...);
        } else { 
            plot8dnca(x@df[,nuvx],x@red[[nuvx]],x@cod[[nuvx]],
                      xlab=xlab,title=titles[g],sub=sti,...);
        }
    } else {
        # bivariate case
        if (is.null(stitles)) { sti <- "(bivariate)";
        } else { sti <- stitles[g];}
        if ( covx &  covy) {
            gg <- gg+1;
            if (file != "") { cat(gg,": [",nuvx,"&",nuvy,"]  (",varx[nuvx],"&",vary[nuvy],")\n")};
            plot8dncoco(x@df[,c(nuvx,nuvy)],
                        rred=list(x@red[[nuvx]],x@red[[nuvy]]),
                        ccod=list(x@cod[[nuvx]],x@cod[[nuvy]]),
                        title=titles[g],
                        type=pts,lla=lla,
                        xlab=xlab,ylab=ylab,
                        sub=sti,
                        ...
                       );
        }
        if ( covx & !covy) {
            nbg <- length(x@red[[nuvy]]);
            if (file != "") { cat(paste(gg+1,"-",gg+nbg,sep=""),
                                  ": [",nuvx,"&",nuvy,"]  (",
                                  varx[nuvx],"&",vary[nuvy],")\n")};
            gg <- gg+nbg;
            plot8dncoca(x@df[,c(nuvx,nuvy)],
                        rred=list(x@red[[nuvx]],x@red[[nuvy]]),
                        ccod=list(x@cod[[nuvx]],x@cod[[nuvy]]),
                        title=paste(titles[g],paste("[",vary[g],"]",sep="")),
                        xlab=xlab,
                        sub=sti,
                        type="smooth",...
                       );
        }
        if (!covx &  covy) {
            nbg <- length(x@red[[nuvx]]);
            if (file != "") { cat(paste(gg+1,"-",gg+nbg,sep=""),
                                  ": [",nuvx,"&",nuvy,"]  (",
                                  varx[nuvx],"&",vary[nuvy],")\n")};
            gg <- gg+nbg;
            plot8dncoca(x@df[,c(nuvy,nuvx)],
                        rred=list(x@red[[nuvy]],x@red[[nuvx]]),
                        ccod=list(x@cod[[nuvy]],x@cod[[nuvx]]),
                        title=paste(titles[g],paste("[",varx[g],"]",sep="")),
                        xlab=xlab,
                        sub=sti,
                        type="smooth",...
                       );
        }
        if (!covx & !covy) {
            gg <- gg+1;
            if (file != "") { cat(gg,": [",nuvx,"&",nuvy,"]  (",varx[nuvx],"&",vary[nuvy],")\n")};
            plot8dncaca(x@df[,c(nuvx,nuvy)],
                        rred=list(x@red[[nuvx]],x@red[[nuvy]]),
                        ccod=list(x@cod[[nuvx]],x@cod[[nuvy]]),
                        title=paste(titles[g]," {",varx[g],"/",
                                    vary[g],"}",sep=""),
                        xlab=xlab,ylab=ylab,
                        sub="(bivariate)",
                        type="joint",...
                       );
        }
    }
}}
#
if (file != "") { sink();}
invisible();    
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("plot", signature(x = "dn"),  plot8dn);
setMethod("print",signature(x = "dn"), print8dn);
#

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
draw8empidata <- function(nam,ny,E,X,nat,win,bnid,
                           strict=FALSE,mnbo=10) 
#TITLE (dn) performs an empidata [multivariate] draw
#DESCRIPTION
# According to parent values provided in X, returns a draw
# for simulation from E data frame. The returned variables
# are defined by ny, the subset selection id defined by win.
# See the detail section for the management of missing values.\cr
# Each time a drawn is done, a line indicating the size in which 
# the drawn was made is appended to the file 'rebastaba.empidat.txt'.
# This file is destroyed when compiling 'rebastaba.la.r'
#DETAILS 
#  To allow the successive call of empidata with corresponding variables
#  the node name was eliminated from the correspondance. E.g. let 
#  suppose that we are asking a first correspondance with the sex to
#  get the height of some individuals. We could match the previous
#  drawn "SEX" with "SEX" in the empidata E to get "HGT" in data frame
#  but which will be "E[HGT]" for the /bn/. Now, if we want to use the
#  obtained "E[HGT]" to ge the weight from another empidata set F, we will
#  introduce "E[HGT]" to match "HGT" into data frame F getting "WGT"
#  named as "F[WGT]" at the /bn/ level. This explains why, at the code
#  level 'nam' is transformed into "namv" with "SEX" giving "SEX"
#  and "E[HGT]" giving "HGT". This is the consequence that square
#  brackets can be problematic for variable names in data.frames. Also
#  this supports some consistency.
#  \cr
#  Missing values can be implied in several ways. Here are the
#  rules which are applied (the principle is to keep the correlations
#  between the variables, if the user does not want them, it must 
#  call draw8empidata several times with the same data base.).\cr
#  Let us denote X[parents], E[parents] and E[variables], respectively,
#  the inherited parents from the previous simulated values, the
#  corresponding parents in the data base according to win selection
#  and the corresponding variables we are drawing.
#{1} <<When there are missing values in E[variables], either they
#      are left (strict=FALSE) either they are prevented, forbidding
#      these observations to be drawn.>>
#{2} <<When there are parents, any missingness in X[parents] leads to
#      missing values in all E[variables] since there is no
#      way to know the corresponding observations in E data base..>>
#{3} <<For the same reason, when there are parents, all observations
#      with missing values in E[parents] are discarded.>>
#{4} <<When there are less observations available to draw than mnbo,
#      then NA are issued. Notice that the important point of the size
#      of the available sample for a given combination is not
#      considered more than with the mnbo parameter with no
#      posterior control by the user.>>
#KEYWORDS
#PKEYWORDS 
#INPUTS
#{nam} << Vector of the parent names of the node to be simulated. When the
#         parent names comprise node name and brackets, there are eliminated
#         to look for the correspondence into E data frame. Said in a
#         different way, never brackets for data.frame names. 
#         A value of character(0) means no parents. In that case the
#         draw is random whateber be win@ty.>>
#{ny} << Name(s) of the variable(s) to simulate (WITH node name and brackets).>>
#{E} << The data frame to be used for the empidata distribution.
#       It must comprise all possible parents and variables 
#       indicated in ny (but WITHOUT the node name and brackets).>>
#{X} << The matrix of values of the already simulated values.
#       It must comprises the
#       parents. If there is no parents, the matrix must be given (even
#       with no columns) just to give the number of simulations by its
#       row number.>>
#{nat} << Vector of the parent natures, same order as nam.>>
#{win} << /win/ object to be used for the selection (see 'draw8pred').>>
#{bnid} << character identifying the bn (for indication in the
#          rbsb.efi sinking.>>
#[INPUTS]
#{strict} <<(=FALSE) Must the elimination of missing values be common
#           to all variables of the node?>>
#{mnbo} <<(=10) Minimum of number of observations for a draw to 
#         be performed. If less than mnbo, a NA is returned.>>
#VALUE
# A vector (matrix) of size nrow(X) with the simulated value in the right order.
# When the subset which is selected is less than mnbo, then NA is introduced.
#EXAMPLE
# rsdn3k("RESET"); # to comply R checking
# E <- data.frame(A=1:100,c=100:1); # the empirical distribution
# X <- as.data.frame(matrix(20:29,10,1,dimnames=list(NULL,"A"))); # the covariate values
# win <- new("win",wgt=1,k=0,di=c(0,1),nb=c(1,2));
# draw8empidata("A","B[c]",E,X,"conti",win,bnid="rbsb.bn2",mnbo=1);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
# ??? think about the necessity of ny/nyold ???
#AUTHOR J.-B. Denis
#CREATED 07_10_17
#REVISED 10_04_22
#--------------------------------------------
{
#
# some checking and preparation of constants
#
if (rbsb.mck) {valid8win(win);}
check4tyle(bnid,"character",1,"The 'bnid' argument is not accepted");
nyold <- ny;
ny <- nv2nv(ny)$var;
namv <- nam;
if (isvide(nam)) {
    nam <- character(0);
    namv <- character(0);
    nat <- character(0);
} else {
    toto <- nv2nv(nam);
    for (ii in sjl(nam)) {
        if (toto$var[ii] != "") {
            namv[ii] <- toto$var[ii];
        }
    }
}
#
if (!is.data.frame(E)) {erreur(E,"E is not a data.frame!");}
nbs <- nrow(X); nbe <- nrow(E);
if (nbs < 1) {erreur(nbs,"asking for less than ONE simulation!");}
if (nbe < 1) {erreur(nbe,"The empidata data frame is empty!");}
ava <-  dimnames(E)[[2]];
if (!all(ny %in% c(ava,names(rbsb.ena)))) {
    cat("The proposed variables to generate are:",ny,"\n");
    cat("The available variables in the data frame are:",ava,"\n");
    cat("The specific variables for data frame are:",names(rbsb.ena),"\n");
    erreur(ny,"some are missing!" );
}
if (!all(namv %in% ava)) {
    cat("The proposed parents are: <",nam,">\n");
    cat("They were interpreted as: <",namv,"> for the data frame correspondence\n");
    cat("But the available variables in the data frame are: <",ava,">\n");
    erreur(NULL,"The parents are not in the empidata data frame!" );
}
if (!all(nam %in% dimnames(X)[[2]])) {
    cat("parents are:",nam,"\n");
    cat("the variables in the simulated matrix are:",dimnames(X)[[2]],"\n");
    erreur(NULL,"The parents are not present in the already simulated variables!" );
}
if (length(nam) != length(nat)) {
    cat("parents are: <",nam,">\n");
    cat("the natures are: <",nat,">\n");
    erreur(NULL,"They have got different lengths!" );
}
if (length(nam) != length(win@wgt)) {
    erreur(list(nam,win),"The parents (nam) and the win@wgt must have the same length!" );
}
#
# establishing the weight to be used when negative
#
for (i in sjl(nam)) {
    if (win@wgt[i] < 0) {
        # standard distance
        if (rbsb.snp[nat[i],"numeric"]) {
            sig <- sqrt(var(X[,nam[i]]));
            win@wgt[i] <- -win@wgt[i] * sig;
        }
        if (rbsb.snp[nat[i],"categoric"]) {
            win@wgt[i] <- -win@wgt[i] * rbsb.cdi;
        }
    }
}
#
# Applying the strictness option
#
if (strict) {
    # restriction of the drawing to the complete variable set
    if (length(ny) > 1) {
        E <- E[apply(!is.na(E[,ny]),1,all),,drop=FALSE];
    } else {
        E <- E[!is.na(E[,ny]),,drop=FALSE];
    }
}
# In case of parents, removing the incomplete observations of the base
if (length(nam) > 0) {
    if (length(nam) > 1) {
        E <- E[apply(!is.na(E[,namv]),1,all),,drop=FALSE];
    } else {
        E <- E[!is.na(E[,namv]),,drop=FALSE];
    }
}  
nbe <- nrow(E);
#
# At last drawing
#
if (nbe >= mnbo) {
    # sufficient number of observations
    # to perform random draws
    dvar <- form3liste(nyold,OPA="[ ",CPA=" ]",opa="",cpa="",sep=" / ")
    sink(rbsb.efi,TRUE);
    form3titre(paste("/bn/ =",bnid," |  variable(s) =",dvar),10);
    form3titre(paste("empidata of size",nbs),3);
    if (length(nam)==0) {
        form3titre("There are no parents",3);
        form3titre(paste("They were drawn in a population of size",nrow(E)),3);
    } else {
        pare <- form3liste(nam,OPA="< ",CPA=" >",opa="",cpa="",sep=" / ");
        difc <- form3liste(win@wgt,OPA="{ ",CPA=" }",opa="",cpa="",sep=" / ");
        form3titre(paste("The parent(s) is(are)",pare),3);
        form3titre("The corresponding subsetting criterium is:",3);
        print(win,1);
        form3titre(paste("The total size of the population is",nrow(E)),3);
        cat("\n\n        The following table gives for each drawn value:\n",
            "                {1} the size of the selected subset\n",
            "                {2} the parent value(s)\n",
            "                {3} the drawn value(s)\n\n");
        titre <- paste(" among { #items } with {",paste(nam,collapse='; '),
                       "}  -> {",
                       paste(nyold,collapse="; "),"}");
        cat("\n",titre,"\n\n");
        if (rbsb.esu) {
            form3line(wid=1);
            cat("As rbsb.esu is TRUE selected subsets will be introduced for each draw\n");
            form3line(wid=1);
        }
    }
    sink();
    #
    if (length(nam) == 0) {
        # there is no parent, the draw can be done straightforwardly
        # writting the size
        # no parents => no more selection before drawing
        probaE <- rep(1,nrow(E));
        qes <- sample(1:nbe,nbs,replace=TRUE,prob=probaE);
        # adding the possible specific nodes
        if (names(rbsb.ena)[1] %in% ny) {
            xxx <- rep(nrow(E),nrow(E));
            E <- cbind(E,xxx);
            dimnames(E)[[2]][ncol(E)] <- names(rbsb.ena)[1];
        }
        if (names(rbsb.ena)[2] %in% ny) {
            xxx <- rep(NA,nrow(E));
            E <- cbind(E,xxx);
            dimnames(E)[[2]][ncol(E)] <- names(rbsb.ena)[2];
        }
        res <- E[qes,ny,drop=FALSE];
        names(res) <- nyold;
    } else {
        # formating the size of subpopulation
        nblen <- min(8,nchar(as.character(nrow(E))));
        #
        # there are parents, the drawing will be done row by row
        #
        # preparing the resulting data frame
        res <- data.frame(row.names=1:nrow(X));
        for (ji in sjl(nyold)) { res[[ji]] <- rep(NA,nrow(X));}
        names(res) <- nyold;
        # there are parents
        # => some subsetting must be done
        # at the level of each draw
        # it is done in a ugly and slow loop!
        # all the difficult part is done into draw8pred
        sst <- numeric(nbs);
        for (hd in sj(nbs)) {
            # we are working with the hd-th simulation
            ### ??? 
            ### For the moment, nat of E is badly
            ### reconstituted... To be improved when
            ### introducing /rbsb/ data.frame objects
            #### ???
            nata <- rep("conti",ncol(E));
            for (na in sjl(nata)) {
                if (is.factor(E[[na]])) {
                    nata[na] <- "categ";
                }
            }
            tirage <- draw8pred(X[hd,nam,drop=FALSE],
                                E,nata,win);
            xxx <- tirage$pred;
            # it is supposed that only a row is returned
            # adding the possible specific nodes
            if (names(rbsb.ena)[1] %in% ny) {
                xxx[[names(rbsb.ena)[1]]] <- tirage$nb_c;
            }
            if (names(rbsb.ena)[2] %in% ny) {
                xxx[[names(rbsb.ena)[2]]] <- tirage$di_x;
            }
            #
            res[hd,nyold] <- xxx[ny];
            sst[hd] <- tirage$nb_c;
            #
            # storing the results
            #
            sink(rbsb.efi,TRUE);
            # from time to time the identification of the columns
            if ((hd %% rbsb.eti) == 0) { cat("\n",titre,"\n\n");}
            # the size of the selected subset
            cat("[",sst[hd],"] ");     
            # the conditioning values
            cat("with {");
            for (ben in nam) {
                if (is.factor(X[,ben])) { uuu <- as.character(X[hd,ben]);
                } else { uuu <- round(X[hd,ben],rbsb.ede);}
                cat("",uuu);
            }
            cat("}");
            # the prediction obtained
            cat(" -> {");
            for (ben in nyold) {
                if (is.factor(res[,ben])) { uuu <- as.character(res[hd,ben]);
                } else { uuu <- round(res[hd,ben],rbsb.ede);}
                cat("",uuu);
            }
            cat("}\n");
            sink();
        }
        # ??? the following must be suppressed in a next future (09_06_27)
        # putting back the factors; taking care not to loose some non used levels!
        for (ji in sjl(nyold)) {
            if (is.factor(E[[ny[ji]]])) {
                lele <- levels(E[[ny[ji]]]);
                res[[ji]] <- factor(lele[res[[ji]]],
                                    levels=lele);
            }
        }
    }
} else {
    erreur(list(nrow(E),mnbo),"The number of observations was not sufficient to perform the drawing!");
} 
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
draw8popula <- function(ny,E,nbs) 
#TITLE (dn) performs an empidata [multivariate] SYSTEMATIC draw
#DESCRIPTION
# Returns a systematic draw from E data frame. The returned variables
# are defined by ny. Similar to draw8empidata but no parents are
# allowed and the draw is systematic. By systematic, it is meant
# that the first nbs individuals are used, with cycling if nrow(E)<nbs.
#DETAILS 
#KEYWORDS
#PKEYWORDS 
#INPUTS
#{ny} << Name(s) of the variable(s) to simulate (with node name and
#        square brackets).>>
#{E} << The data frame to be used for the empidata distribution.
#       It must comprise indicated in ny BUT without node name and
#       brackets.>>
#{nbs} << The number of simulations to extract.>>
#[INPUTS]
#VALUE
# A data frame of size [nbs,length(ny)] with the simulated values
# in the right order.
#EXAMPLE
# rsdn3k("RESET"); # to comply R checking
# E <- data.frame(A=1:10,c=100:91); # the empirical distribution
# draw8popula(c("uu[A]","vv[c]"),E,5);
# draw8popula("w[A]",E,10);
# draw8popula("w[A]",E,12);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_12_23
#REVISED 10_02_01
#--------------------------------------------
{
#removing the node denomination
nyold <- ny;
ny <- nv2nv(ny)$var;
# some checking
if (!is.data.frame(E)) {erreur(E,"E is not a data.frame!");}
nbe <- nrow(E);
if (nbs < 1) {erreur(nbs,"asking for less than ONE simulation!");}
if (nbe < 1) {erreur(nbe,"The empidata data frame is empty!");}
ava <-  dimnames(E)[[2]];
if (!all(ny %in% ava)) {
    cat("The proposed variables to generate are:",ny,"\n");
    cat("The available variables in the data frame are:",ava,"\n");
    erreur(ny,"is(are) not in the empidata data frame!" );
}
#
# 
#
nbr <- (nbs %/% nbe) + 1;
qui <- rep(1:nbe,nbr)[1:nbs];
res <- E[qui,ny,drop=FALSE];
names(res) <- nyold;
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
draw8multinom <- function(n,size,proba,levels=NULL)
#TITLE (dn) simulates random draw from a multinomial
#DESCRIPTION
# As far as I understood, there is no possibility
# to use the standard rmultinom in a parallel way to
# get the complete set of simulations with a unique call.
# This is why this function was created, if necessary
# it could be replaced by some C code one day.\cr
# There is two ways to use the function. (i) When 
# is.null(levels) as a multivariate multinomials with
# different sizes, (ii) When levels is a non null
# character, then a categorical taking the levels
# values is returned.
#DETAILS
# When !is.null(levels) size must be a vector of
# ones.
#KEYWORDS
#PKEYWORDS simulation
#INPUTS
#{n} <<Number of draws to obtain.>>
#{size} <<Vector [n] of the sizes of the multinomials.>>
#{proba} <<Matrix [n,k] of the probabilities of the multinomials.>>
#[INPUTS]
#{levels} <<(=NULL) When a categoric variable must be returned,
#           levels provides its levels (since it is a factor).>>
#VALUE
# when is.null(levels) the integer matrix [n,k] of the draws is
# returned else a factor.
#EXAMPLE
# draw8multinom(10,1:10,matrix(1:10,10,3,byrow=TRUE));
# draw8multinom(10,rep(1,10),matrix(1:10,10,3,byrow=TRUE),levels=LETTERS[1:3]);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_30
#REVISED 10_02_01
#--------------------------------------------
{
# some checking
if (length(size) != n) {
     if (is.matrix(size)) {
         if (nrow(size) != n) {
             erreur(list(size,length(size)),"This function needs a vector of size n =",n,
                    "or a matrix with n rows!");
         } else {
            if (!all(outer(size[,1],rep(1,ncol(size)),"*") == size)) {
                 erreur(size,"This function expects a matrix with all columns are identical");
            }
         }
         size <- size[,1];
     } else {
         erreur(size,"This function needs a vector of size n =",n,
                "or a matrix with n rows!");
     }
}
if (nrow(proba) != n)  { erreur(dim(proba),"This function needs a proba matrix [n,*]!");}
k <- ncol(proba);
if (!is.null(levels)) {
    if (!all(size == 1)) {
        erreur(range(size),"When levels is not null, size must be a vector of ones");
    }
    if (length(levels) != k) {
        erreur(list(k,levels),"When levels is not null, its length must equal ncol(proba)");
    }
}
res <- matrix(NA,n,k);
for (i in sj(n)) { res[i,] <- rmultinom(1,size[i],proba[i,]);}
if (!is.null(levels)) {
    res <- res %*% cbind(1:k);
    res <- as.factor(res);
    levels(res) <- levels;
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
draw8Dirichlet <- function(n,A,a)
#TITLE (dn) simulates random draws from a Dirichlet
#DESCRIPTION
#  No more than a copy of rdirichlet found in \code{mc2d}
# package adapted for our parameterization. The \code{A}
# vector of values is the sum of usual \code{A_i}; The 
#  \code{a} vector of vectors (then a matrix) is the 
# proportions associated to the usual \code{A_i}.
#DETAILS
#KEYWORDS
#PKEYWORDS simulation
#INPUTS
#{n} <<Number of draws to obtain.>>
#{A} <<Vector [n] of the confidences given to each Dirichlet.>>
#{a} <<Matrix [n,k] of the proportions for each one.>>
#[INPUTS]
#VALUE
# a matrix [n,k] of the draws is returned
#EXAMPLE
# set.seed(1234);draw8Dirichlet(5,1:5,matrix(1:15,5));
# set.seed(1234);draw8Dirichlet(5,rep(1,5),matrix(1:15,5));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_04_02
#REVISED 09_04_02
#--------------------------------------------
{
# some checking
if (length(A) != n) {
     if (is.matrix(A)) {
         if (nrow(A) != n) {
             erreur(list(A,length(A)),"This function needs a vector of size n =",n,
                    "or a matrix with n rows!");
         } else {
            if (!all(outer(A[,1],rep(1,ncol(A)),"*") == A)) {
                 erreur(A,"This function expects a matrix with all columns are identical");
            }
         }
         A <- A[,1];
     } else {
         erreur(A,"This function needs a vector of size n =",n,
                "or a matrix with n rows!");
     }
}
if (sum(a<0) > 0) { erreur(a,"This function expect a matrix with only non negative values");}
if (nrow(a) != n)  { erreur(dim(a),"This function needs a proportion matrix of dim [n,*]!");}
# computing the parameter alpha
alpha <- (a / rowSums(a)) * as.vector(A);
# from here it is a non modifyed copy of rdirichlet
if (is.vector(alpha)) { alpha <- t(alpha);}
l <- dim(alpha)[2];
x <- matrix(rgamma(l * n, t(alpha)), ncol = l, byrow = TRUE);
# returning
x/rowSums(x);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
draw8edcg <- function(X,proba,lev=NULL,
                      parents=NULL,parentslength=NULL)
#TITLE (dn) simulates random draws for a
#           numcat distribution
#DESCRIPTION
#   This function simulates a categoric distribution.
# Similar to the multinomial distribution but the 
# size parameter is one and it is considered as a univariate
# distribution. Values are the numbers of drawn [\code{lev}] categories.
# If necessary this function could be replaced by some C code...
# Possible conditional distribution are introduced if
# !is.null(parents) using their order and unex convention. See the
# description provided in the function \code{help8ltype} for \code{numcat}.
#DETAILS
#KEYWORDS
#PKEYWORDS simulation
#INPUTS
#{X} <<already generated parents. If no one, must be a matrix
#      nx0 to indicate the number of draws to perform.>>
#{proba} <<Matrix [p,k] of the probabilities of the multinomials.
#          When there are no parents, p=1 because the distribution
#          is the same for all the draws. When there are parents,
#          it is the product of the numbers of categories of the
#          parents. By the way, the parents must be of nature
#          \code{categ}.>>
#[INPUTS]
#{lev} << When null a distribution is drawn with 
#            values from \code{1} to \code{ncol(proba)}.
#            When numeric transformed \code{as.character(round(lev))}.
#            When character, a categoric distributions is drawn with 
#            the categories given by \code{lev}, a character vector. (If
#            not null the length of lev must be equal to the column 
#            number of the \code{proba} matrix.)>>
#{parents} << When null no parents. If not, the parent names
#            as variables names if any. Be careful that their order
#            is significant for the interpretation of the proba 
#            matrix.>>
#{parentslength} << Only when there are parents. Then
#            The vector of lengths of levels of the parents.
#            This cannot be deduced from the simulated values
#            because some of the levels can be missing.>>
#VALUE
# a vector [n] of the draws is returned (it is a factor).
#EXAMPLE
# rsdn3k("RESET"); # For R checking
# X <- matrix(NA,12,0);
# proba <- matrix(1:5,1);
# draw8edcg(X,proba);
# UU <- sample(LETTERS[1:4],20,TRUE);
# X <- as.data.frame(UU);
# proba <- matrix(1:12,4);
# draw8edcg(X,proba);
# draw8edcg(X,proba,lev=letters[1:12]);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
# the name of the node is not provided in the returned data
# frame because, it is unknown.
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_30
#REVISED 10_02_01
#--------------------------------------------
{
n <- nrow(X);
# checking the parents
# di the dimensions of each parents
# pp their combination number
if (is.null(parents) | (length(parents)==0)) {
    pp <- 1;
    proba <- matrix(proba,1);
} else {
    pp <- 1; di <- rep(1,length(parents));
    for (ck in sjl(parents)) {
        pck <- parents[ck];
        if (pck %in% dimnames(X)[[2]]) {
            if (is.factor(X[,pck])) {
                di[ck] <- parentslength[ck];
                pp <- pp * di[ck];
            } else {
                erreur(pck,"Parents of a categoric variable must be 'categ'");
            }
        } else {
            cat("Available parents:",dimnames(X)[[2]],"\n");
            cat("Required parent:",pck,"\n");
            print(dim(X));
            erreur(pck,"This parent was not found in X columns!");
        }
    }
}
# checking the proba
if (!is.matrix(proba)) {
    # be aware that when there are no parents, proba was built as a matrix
    erreur(proba,"When there are parents, 'proba' must be a matrix!");
}
k <- ncol(proba);
# checking the levels
if (is.null(lev)) { lev <- 1:k;}
if (is.numeric(lev)) {
    lev <- as.character(lev);
}
if (k != length(lev)) {
    print(proba);
    cat("proba dimension is:",dim(proba),"\n");
    cat("number of levels is:",length(lev),"\n");
    cat("levels are:",lev,"\n");
    erreur(NULL,"'proba' and 'lev' have got different lengths!");
}
# checking the number of rows of proba
if (nrow(proba) != pp) {
    erreur(list(dim(proba),pp),"'proba' row number and parents seem inconsistent!");
}
# constructing the big proba matrix
if (pp == 1) { 
    iinn <- rep(1,n);
} else { iinn <- unex(X[,parents,drop=FALSE],di);}
PROBA <- proba[iinn,];
# drawing
resu <- rep(NA,n);
for (i in sj(n)) { resu[i] <- which(1==rmultinom(1,1,PROBA[i,]));}
# replacing by the desired values
resu <- factor(lev[resu],levels=lev);
res <- data.frame(row.names=1:length(resu));
res[[1]] <- resu;
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
draw8pred <- function(ind,E,nat,
                win=new("win",wgt=rep(1,length(nat)),
                        k=2,di=c(0,1),nb=c(1,Inf),ty="media1")
                    )
#TITLE (dn) returns the predicted individual for
#           an empidata node.
#DESCRIPTION
# The description of the algorithm is given into
# /rbsb/ manual. The prediction is monitored by the
# /win/.
#DETAILS
# When the global variable rbsb.esu is true
# then the selected subset is appended to rbsb.efi
#KEYWORDS
#PKEYWORDS empidata
#INPUTS
#{ind} <<Value of the individual to predict (one row
#        data.frame.>>
#{E} <<data.frame with the candidates in its rows.>>
#{nat} <<nature of the E variables (character).>>
#[INPUTS]
#{win} <<(=new("win",wgt=rep(1,length(nat)),
#                        k=2,di=1,nb=c(1,Inf),ty="median1")),
#        the /win/ to be used for the selection process.>>
#VALUE
# a list with components:\cr
# $pred: the predicted individual (with all E-variables
#    but with condi values forced to those of ind).\cr
# $nb_c: size of the sub-sample from which it was produced.\cr
# $di_x: the maximum distance of the considered candidates
#EXAMPLE
# rsdn3k("RESET"); # to comply R checking
# E <- data.frame(A=1:100,c=100:1); # the empirical distribution
# win <- new("win",wgt=1,k=0,di=c(0,1),nb=c(1,2));
# draw8pred(data.frame(A=20),E,c("conti","conti"),win);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_06_26
#REVISED 10_04_28
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    valid8win(win);
    check4tyle(ind,"data.frame",-1,"ind of draw8pred");
    check4tyle(  E,"data.frame",-1,"  E of draw8pred");
}
# getting the selected subset
DD <- draw8sele(ind,E,nat,win);
if (rbsb.esu) {
    sink(rbsb.efi,append=TRUE);
    form3line();
    print(DD$df);
    cat(" Finally the retained 'prediction' is:\n");
    sink();
}
#
# the algorithm
#
# constructing the prediction
res <- new("faux",orig="draw8pred",
                  defi="win@ty seems bad");
if (nrow(DD$df)==0) {
    res <- E[1,,drop=FALSE];
    res[,] <- NA;
} else {
    fait <- FALSE;
    #
    if (win@ty[1]=="random") {
        # just getting one among the possible
        if (nrow(DD$df)==1) { nu <- 1;
        } else { nu <- sample(1:nrow(DD$df),1);}
        res <- DD$df[nu,,drop=FALSE];
        # and forcing conditioning to 'ind'
        res[,names(ind)] <- ind;
        #
        fait <- TRUE;
    }
    #
    if (win@ty[1] %in% c("median*","mean*")) {
        # imposing the structure for the result
        res <- DD$df[1,,drop=FALSE];
        for (vv in sj(ncol(E))) { if (!(names(E)[vv]%in%names(ind))) { 
            if (rbsb.snp[nat[vv],"numeric"]) {
                # getting the median or the mean INDEPENDENTLY for numeric variables
                if (win@ty[1] %in% c("median*")) {
                    res[,vv] <- median(DD$df[,vv],na.rm=TRUE);
                }
                if (win@ty[1] %in% c("mean*")) {
                    res[,vv] <- mean(DD$df[,vv],na.rm=TRUE);
                }
            }
            if (rbsb.snp[nat[vv],"categoric"]) {
                # getting the most probable category
                # for categoric variables
                tata <- table(DD$df[,vv]);
                qtat <- which(max(tata) == tata)[1];
                res[,vv] <- DD$df[qtat,vv];
            }
        }}
        # and forcing conditioning to 'ind'
        res[,names(ind)] <- ind;
        #
        fait <- TRUE;
    }
    #
    if (win@ty[1] %in% c("median1")) {
        # imposing the structure for the result
        res <- DD$df[1,,drop=FALSE];
        # getting the values of the designated variable
        if(!(win@ty[2] %in% names(DD$df))) {
            erreur(list(DD$df,win@ty),
                   "The variable to get the median does not exist (draw8pred)");
        }
        nuv <- which(win@ty[2]==names(DD$df));
        vava <- DD$df[,win@ty[2]];
        # to be sure that the median will be a value, only
        # odd lenght are admitted
        if ((length(vava) %% 2)==0) { vava <- vava[-length(vava)];}
        # looking for the median candidate from the nuv-th variable
        if (rbsb.snp[nat[nuv],"numeric"]) {
            vmed <- median(vava,na.rm=TRUE);
            lequ <- which(vava == vmed);
        }
        if (rbsb.snp[nat[nuv],"categoric"]) {
            tata <- table(vava);
            lequ <- which(max(tata) == tata);
        }
        # designating one of the possible median values
        # here some randomness can occur
        ##### DOES NOT WORK WHEN length(lequ)==1 !!! #lequ <- sample(lequ,1);
        if (length(lequ) > 1) {lequ <- sample(lequ,1);}
        #
        res[1,] <- DD$df[lequ,,drop=FALSE];
        # and forcing conditioning to 'ind'
        res[,names(ind)] <- ind;
        #
        fait <- TRUE;
    }
    #
    if (!fait) {
        rapport(paste(win@ty,"was not detected as non acceptable (in 'draw8prede')"));
    }
}
#
# returning
list(pred=res,nb_c=nrow(DD$df),di_x=DD$di_x);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
simulate8bn <- function(bn,simu=100)
#TITLE (dn) simulates a /dn/ from a /bn/
#DESCRIPTION
# returns a matrix having simu rows and
# as many columns as variables in the bn, using 
# the joint probability distribution implicitely
# defined by the bn. \cr
# Some of the columns can be already computed, in
# that case there are skipped from the general loop.
# This applies only for the root nodes to save
# the consistency with the /bn/ structure.
#DETAILS
# A compatible order with the definition of the
# conditional probabilities is first computed
# by porder, then nodes are sucessively generated
# according to this order using the generating
# functions stored into @rlks (except for some
# specific cases like 'empidata', 'numcat'...\cr
# BE AWARE that if a multivariate root node is 
# included in the matrix simu, then ALL variables
# must be included and the fact is not checked. \cr
# Notice that the function relies on the column
# names of this matrix and also that the control of
# existing variables in case of multivariate nodes
# is not made a priori but at the moment of their
# use (then possibly generating an error message
# at an unexpected point for the user).
#KEYWORDS
#PKEYWORDS bn simulation
#INPUTS
#{bn}<<the bn to simulate>>
#[INPUTS]
#{simu} <<(=100) either (1) a numeric indicating
# the desired number of simulations or (2) a data.frame
# whose the number of rows indicates the desired
# number of simulations, and where columns can comprises
# already computed nodes.\cr
# Already computed nodes are accepted when they
# are root node for the bn. If there are not root, they 
# are not.\cr
# Notice that extra columns not being a node can be 
# incorporated, they will be returned as they are.\cr
# To better tackle the 'categ' variables, all results
# coming back from the call of a generating function 
# (associated to any node to simulate) are considered
# to be a data frame. If this is not the case, they
# are previously transformed in a data frame. Nevertheless
# the names of the variable are always imposed because
# some of the generating functions cannot do it.>>
#VALUE
# the resulting data frame with values outside 
# corresponding 'lpod's put to NA)
#EXAMPLE
# rsdn3k("RESET"); # For R convenience when checking
# simulate8bn(rbsb.bn1,10);
# simulate8bn(rbsb.bn2,10);
# simulate8bn(rbsb.bn3,10);
# simulate8bn(rbsb.bn4,10);
# simulate8bn(rbsb.bn5,10);
#REFERENCE
#SEE ALSO bn2dn
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 07_06_19
#REVISED 10_04_22
#--------------------------------------------
{
# imprime trigers useful intermediate printing
# it takes value from 0 (no printing) to 4
imprime <- rbsb.sii;
#imprime <- 1;
if (rbsb.mck) {valid8bn(bn);}
# looking for the variable order
ooo <- porder(bn);
# preparing the resulting structure 'res'
# also 'alc' is the list of already calculated node
#      when entering the function
if (is.data.frame(simu)) {
    nb.simu <- nrow(simu);
    res <- simu;
    # determining the already known ROOT nodes
    pano <- ends8gn(bn2gn(bn));
    extre <- c(names(pano$RaL),names(pano$RwL));
    alc <- unique(nv2nod(dimnames(simu)[[2]]));
} else {
    if (length(simu)!=1) {
        erreur(simu,"simu must be a data.frame or the number of simulations");
    }
    nb.simu <- max(1,round(simu));
    res <- data.frame(row.names=1:nb.simu);
    alc <- character(0);
}
# scanning every node
for (jbd in ooo) {
    njbd <- nv2ion(jbd,bn@nom,check=FALSE)@nk;
    vjbd <- nv2ion(jbd,bn@nom,"N",check=FALSE)@vk;
    if (imprime > 0) { cat("<<<noeud",jbd,"\n");}
    if (imprime > 1) { form3affiche(res[1:5,,drop=FALSE]);}
    # checking if the node is not already known
    if (!(jbd %in% alc)) {
	# generating the values ###################################
	speci <- c("popula","empidata","numcat","score");
	# specific generations
	if (bn@ntype[njbd] == "popula") {
            if (imprime > 1) { cat("ltype = popula\n");}
	    # drawing with empidata distribution in a systematic way
	    vava <- nanv(bn@nom,njbd);
            EEE <- get8daf(bn@ndaf[[njbd]]);
	    ax <- draw8popula(
                   vava,                # name(s) of the variable to simulate
		   EEE, # data frame with the empidata distribution
	           nrow(res));          # number of draws to perform
	    if (imprime > 3) {
		cat("<ax (popula)\n");
		form3affiche(ax);
		cat("ax>\n");
	    }
	}
	if (bn@ntype[njbd] == "empidata") {
            if (imprime > 1) { cat("ltype = empidata\n");}
	    # drawing with empidata distribution in a random way
	    ## getting the parents
	    papa <- parents8bn(bn,"n")[[jbd]];
            papa <- nv2ion(papa,bn@nom,"N",FALSE)@nvn;
	    ## getting the nature of the parents
            # There is no variable so the suffix 'rbsb.all' must be added
            # except when there is no parent
            if (length(papa) > 0) {
                papapa <- paste(papa,rbsb.csv,sep="");
            } else {
                papapa <- character(0);
            }
            pvari <- nv2ion(papapa,bn@nom,check=FALSE)@vk;
            nana <- bn@vnat[pvari];
            ## getting the selection criteria
            didi <- bn@nwin[[njbd]];
            ## supplementary check in case of (not adapted to nd/nv)
            # lpapa <- length(papa);
            # if (lpapa != length(didi@wgt)) {
            #   erreur(list(didi,papa),"dif criteria not consistent for node",
            #            jbd,
            #            "which must be empidataly drawn");
            # }
            vava <- nanv(bn@nom,njbd);
            # extracting the necessary data.frame
            Eax <- get8daf(bn@ndaf[[njbd]]);
            ax <- draw8empidata(
                   papa,                # parent names
                   vava,                # name(s) of the variable to simulate
                   Eax,                 # data frame with the empidata distribution
                   res,                 # already simulated matrix
                   nana,                # nature of the parents
                   didi,                # win criteria
                   bn@description@name);# bn identification
	    if (imprime > 3) {
		cat("<ax (empidata)\n");
		form3affiche(ax);
		cat("ax>\n");
	    }
	}
	if (bn@ntype[njbd] == "numcat") {
            if (imprime > 1) { cat("ltype = numcat\n");}
	    # drawing with categoric distribution defined numerically
            if (length(vjbd)!=1) {
                erreur(list(jbd,vjbd),"numcat nodes are supposed to be univariate!");
            }
            # computing the dimensions of the parents
            lonpar <- numeric(length(bn@vparent[[vjbd]]));
            for (ji in sjl(lonpar)) {
                # This is possible because categoric nodes are univariate
                # and the numcat parents are categoric
                papa <- nv2ion(bn@vparent[[vjbd]][ji],bn@nom,check=FALSE)@nn;
                npapa <- nv2ion(papa,bn@nom,"N",check=FALSE)@vk;
                lonpar[ji] <- length(bn@vpod[[npapa]]);
            }
            # drawing
	    ax <- draw8edcg(res,
			    bn@npara[[njbd]]$p,
			    bn@vpod[[vjbd]],
			    bn@vparent[[vjbd]],
                            lonpar
			   );
	    if (imprime > 3) {
		cat("<ax (numcat)\n");
		form3affiche(ax);
		cat("ax>\n");
	    }
	}
	if (bn@ntype[njbd] == "score") {
            if (imprime > 1) { cat("ltype = score\n");}
	    # transforming a numcat into a numeric
            papa <- parents8bn(bn,"n")[[jbd]];
	    ax <- bn@npara[[njbd]]$scores[res[,papa]];
            s1na <- sum(is.na(ax));
            ax <- as.numeric(ax);
            s2na <- sum(is.na(ax));
            if (s2na > s1na) {erreur(bn@npara[[njbd]]$scores,"Check the score, they must be numeric",w=rbsb.mwa);}
	    if (imprime > 3) {
		cat("<ax (score)\n");
		form3affiche(ax);
		cat("ax>\n");
	    }
	}
	# standard generations #########################################
	if (!any(bn@ntype[njbd] == speci)) {
	    # drawing with usual distribution
	    if (imprime > 2) {
		cat("<",jbd,">\n");
		form3affiche(bn@nfug[[njbd]]);
                form3affiche(res);
	    }
	    ax <- bn@nfug[[njbd]](res);
	    if (imprime > 3) {
		cat("<ax (standard)\n");
		form3affiche(ax);
		cat("ax>\n");
	    }
	}
        ################################################################
        # homogeneizing to data frame and imposing the names
        #    (further should not be necessary)
        if (!is.data.frame(ax)) { ax <- data.frame(ax);}
        names(ax) <- nanv(bn@nom,njbd);
        if (imprime > 1) { cat("The node",names(ax),"has just be generated\n");}
        if (imprime > 1) { form3affiche(ax[1:5,,drop=FALSE]);}
        ################################################################
	# applying the adherence to the possible domain
        for (jj in 1:ncol(ax)) {
            xx <- ax[,jj];
            nunu <- nv2ion(matrix(c(njbd,jj),2),bn@nom,check=FALSE)@vk;
	    domain <- bn@vpod[[nunu]];
            # eliminating the possible NA
            oupana <- which(!is.na(xx));
	    if (rbsb.snp[bn@vnat[nunu],"numeric"]) {
		nodo <- (xx[oupana] < domain[1]) | (xx[oupana] > domain[2]);
	    } else {
		if (rbsb.snp[bn@vnat[nunu],"categoric"]) {
		    nodo <- !apply(outer(xx[oupana],domain,"=="),1,any);
		}
	    }
            nnodo <- rep(FALSE,nb.simu);
            nnodo[!oupana] <- TRUE; # detected as NA
            nnodo[oupana[nodo]] <- TRUE; # not NA but out of the possible domain
            ax[nnodo,jj] <- NA;
	}
        ################################################################
	# checking the ratio of missing value
        if (prod(dim(ax))==0) {
            form3affiche(res[1:6,]);
            erreur(list(jbd,nanv(bn@nom,njbd)),"No values were simulated for this node!");
        }
	na_ra <- sum(is.na(ax))/prod(dim(ax));
	if (na_ra > rbsb.nar) {
	     if (imprime > 0) { form3affiche(ax);}
	     erreur(na_ra,"Too much NA values in node",
                           paste(nanv(bn@nom,njbd),collapse="//"),"?",w=rbsb.mwa);
	}
	# increasing the generated data frame
	if (imprime > 3) {
	    cat("<res-ax\n");
	    form3affiche(res);
	    form3affiche(ax);
	    cat("res-ax>\n");
	}
	res <- cbind(res,ax);
        if (imprime >= 2) {
            cat("dim(res)",dim(res),"\n");
        }
    } # ending the simulation of a new node
    if (imprime > 0) {cat("   noeud",jbd,">>>\n");}
} # ending the loop over the nodes [ooo]
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bn2dn <- function(bn,simu=100,score=TRUE)
#TITLE (dn) simulates data from a bn
#DESCRIPTION
# This function creates a /dn/ object simulating bn \code{simu} times.
#DETAILS
# In a first version, the function was performing a 
# genuine simulation from the distributions associated
# to each node of the bn. Then, it was found convenient
# to allow 'already known nodes' using simu as a matrix.
#KEYWORDS
#PKEYWORDS dn
#INPUTS
#{bn}<<the bn object with which to simulate>>
#[INPUTS]
#{simu} << Either (1) a \code{numeric} indicating
# the desired number of simulations, or (2) a data.frame
# whose the number of rows indicates the desired
# number of simulations, and where columns can comprises
# already computed nodes. More details are given in
# the documentation of \code{simulate8bn}.>>
#{score} << When TRUE, an additional variable scoring
# the likelihood of each individual according to the 
# variable domains is added. See the function \code{dn2score} 
# for more details.>>
#VALUE
# The resulting dn object
#EXAMPLE
# rsdn3k("RESET"); # For R convenience when checking
# print(bn2dn(rbsb.bn1,10),quoi="i",simu=100);
# print(bn2dn(rbsb.bn2,10),quoi="i",simu=100);
# print(bn2dn(rbsb.bn3,10),quoi="i",simu=100);
# print(bn2dn(rbsb.bn4,10),quoi="i",simu=100);
# print(bn2dn(rbsb.bn5,10),quoi="i",simu=100);
#REFERENCE
#SEE ALSO
#CALLING {simulate8bn}
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 07_08_24
#REVISED 10_02_02
#--------------------------------------------
{
# checking (1)
if (rbsb.mck) {
    valid8bn(bn);
}
# performing the simulation
sisi <- as.data.frame(simulate8bn(bn,simu));
nbv <- ncol(sisi);
vardn <- nv2ion(0,bn@nom,"N",check=FALSE)@nvn;
# checking (2)
if (rbsb.mck) {
    if (length(vardn)!=ncol(sisi)) {
        form3affiche(vardn);
        form3affiche(dimnames(sisi)[[2]]);
        rapport("difference between the number of variables simulated and expected!");
    }
}
# adaptating the nature and domain fields
nat <- rep("ii",nbv);
pod <- vector("list",nbv);
red <- vector("list",nbv);
cod <- vector("list",nbv);
invar <- icvar <- numeric(0);
for (jbd in sj(nbv)) {
    jd <- vardn[jbd];
    nume <- nv2ion(jd,bn@nom,check=FALSE)@vk;
    if (nume==0) {
        # There is no variable so the suffix 'rbsb.all' must be added
        jd <- paste(jd,rbsb.cpt["variables","opening"],rbsb.all,
                       rbsb.cpt["variables","closing"],sep="");
        nume <- nv2ion(jd,bn@nom,check=FALSE)@vk;
    }
    for (jjb in nume) {
        nat[ jjb]  <- bn@vnat[ jjb];
        pod[[jjb]] <- bn@vpod[[jjb]];
        red[[jjb]] <- bn@vred[[jjb]];
        cod[[jjb]] <- bn@vcod[[jjb]];
    }
    if (rbsb.snp[nat[jbd],"numeric"])   { invar <- c(invar,jbd);}
    if (rbsb.snp[nat[jbd],"categoric"]) { icvar <- c(icvar,jbd);}
}
# finishing the /dn/
comme <- "";
if (is.matrix(simu)) {
    comme <- "Possible already known values were introduced...";
}
ds <- new("des",name=bn@description@name,
                orig="Created by bn2dn",
                comm=comme);
dd <- new("dn",description=ds,
               nat=nat,pod=pod,red=red,cod=cod,
               df=sisi
         );
# adding the score variable
if (score) { dd <- score4dn(dd);}
# returning
if (rbsb.mck) { valid8dn(dd);}
dd;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
repartition3dn <- function(dn)
#TITLE (dn) scans the repartition of the values of dn@df
#DESCRIPTION
# Using the three domains, returns the repartition of
# the simulated values according to the limits
#DETAILS
#KEYWORDS
#PKEYWORDS xn
#INPUTS
#{dn} <<the rbsb object to scan>>
#[INPUTS]
#VALUE
#  returns a list comprising
#  (i) two components for the continuous variables
#    $colimi = matrix of variables x (the 6 limits)
#    $corepa = matrix of variables x (the 6 cases)
#  (ii) two components for the categoric variables
#    $calimi = list of lists for each variables
#    $carepa = matrix of variables x (the 4 cases)
#EXAMPLE
# rsdn3k("RESET"); # For R convenience when checking
# repartition3dn(rbsb.dn4);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_05_27
#REVISED 10_02_02
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    valid8dn(dn);
}
# investigating
vardn <- dimnames(dn@df)[[2]];
conti <- which(rbsb.snp[dn@nat,"numeric"]);
categ <- which(rbsb.snp[dn@nat,"categoric"]);
nbco <- length(conti); nbca <- length(categ);
# the continuous variables
colimi <- corepa <- matrix(NA,nbco,6);
dimnames(colimi) <- list(vardn[conti],
                  c("lpod[1]","lred[1]","lcod[1]",
                    "lcod[2]","lred[2]","lpod[2]"));
dimnames(corepa) <- list(vardn[conti],
                  c("NA","LOWER","lower",
                    "REF","upper","UPPER"));
for (vi in sj(nbco)) {
    v <- conti[vi];
    colimi[vi,] <- sort(c(dn@pod[[v]],dn@red[[v]],dn@cod[[v]]));
    nav <- which((is.na(dn@df[,v])) |
                 (dn@df[,v] < dn@pod[[v]][1]) |
                 (dn@df[,v] > dn@pod[[v]][2]));
    corepa[vi,1] <- length(nav);
    if (length(nav)>0) {x <- dn@df[-nav,v];
    } else { x <- dn@df[,v];}
    if (length(x) > 0) {
        colimi[vi,colimi[vi,]==-Inf] <- min(x) - 1;
        colimi[vi,colimi[vi,]== Inf] <- max(x) + 1;
        corepa[vi,-1] <- hist(x,breaks=colimi[vi,],
                           plot=FALSE)$counts;
    } else {
         corepa[vi,-1] <- 0;
    }
}
# the categoric variables
carepa <- matrix(0,nbca,4);
dimnames(carepa) <- list(vardn[categ],
                  c("NA","PoD","ReD","CoD"));
for (vi in sj(nbca)) {
    v <- categ[vi];
    uu <- table(dn@df[,v]);
    for (ii in sjl(uu)) {
        ww <- names(uu)[ii];
        if (ww %in% dn@cod[[v]]) {
            carepa[vi,"CoD"] <- uu[ii] + carepa[vi,"CoD"];
        } else {
            if (ww %in% dn@red[[v]]) {
                carepa[vi,"ReD"] <- uu[ii] + carepa[vi,"ReD"];
            } else {
		if (ww %in% dn@pod[[v]]) {
		    carepa[vi,"PoD"] <- uu[ii] + carepa[vi,"PoD"];
		} else {
                    carepa[vi,"NA"] <- uu[ii] + carepa[vi,"NA"];
                }
            }
        }
    }
}
calimi <- vector("list",0);
for (vi in sj(nbca)) {
    v <- categ[vi];
    lili <- list(PoD=character(0),ReD=character(0),Cod=character(0));
    lili[["PoD"]] <- setdiff(dn@pod[[v]],dn@red[[v]]);
    lili[["ReD"]] <- setdiff(dn@red[[v]],dn@cod[[v]]);
    lili[["CoD"]] <- dn@cod[[v]];
    calimi[[vardn[v]]] <- lili;
}
# returning
list(colimi=colimi,corepa=corepa,
     calimi=calimi,carepa=carepa);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
dn2score <- function(dn,qui=rbsb.num0)
#TITLE (dn) Computes the individual scores for a /dn/
#DESCRIPTION
# Using the \code{rbsb.scv} limits computes according
# to the nature of each variable an 'outlying' score
# for each row of \code{dn@df}.
#DETAILS
#KEYWORDS 
#PKEYWORDS 
#INPUTS
#{dn}<<The /dn/ object.>>
#[INPUTS]
#{qui} <<(\code{numeric} or \code{character}) indicates
#          the variable on which it must be computed.
#          default, all the variates.>>
#VALUE
# A numeric vector containing the scores in the 
# right order
#EXAMPLE
# rsdn3k("RESET"); ## for R checking convenience
# dn2score(rbsb.dn2);
# dn2score(rbsb.dn4,4);
# dn2score(rbsb.dn4,5);
# dn2score(rbsb.dn4,6);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_12_28
#REVISED 10_03_09
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    valid8dn(dn);
    check4tyle(qui,c("numeric","character"),-1,"within dn2score");
}
# initializing
score <- rep(rbsb.scv[1],nrow(dn@df));
nbv <- length(dn@df);
# determining the active variables
if (isvide(qui)) {
    qui <- sj(nbv);
} else {
    if (is.numeric(qui)) {qui <- numero(qui,sj(nbv));
    } else { qui <- numero(qui,names(dn@df));}                          
}
# computing
for (j in qui) {
    va <- dn@df[,j];
    # NA values
    nna <- !is.na(va);
    ona <- which(!nna);
    score[ona] <- score[ona] + rbsb.scv[4];
    # numeric variables
    if(rbsb.snp[dn@nat[j],"numeric"]) {
        po <- (va < dn@pod[[j]][1]) | (va > dn@pod[[j]][2]);
        po <- po & nna;
        score[po] <- score[po] + rbsb.scv[3];
        re <- (va < dn@red[[j]][1]) | (va > dn@red[[j]][2]);
        re <- re & nna & !po;
        score[re] <- score[re] + rbsb.scv[2];
        co <- (va < dn@cod[[j]][1]) | (va > dn@cod[[j]][2]);
        co <- co & nna & !po & !re;
        score[co] <- score[co] + rbsb.scv[1];
    }
    # categoric variables
    if(rbsb.snp[dn@nat[j],"categoric"]) {
        po <- va %in% dn@pod[[j]];
        po <- !po & nna;
        score[po] <- score[po] + rbsb.scv[3];
        re <- va %in% dn@red[[j]];
        re <- !re & nna & !po;
        score[re] <- score[re] + rbsb.scv[2];
        co <- va %in% dn@cod[[j]];
        co <- !co & nna & !po & !re;
        score[co] <- score[co] + rbsb.scv[1];
    }
}
# returning
score;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
dn2ustat <- function(dn,qui=rbsb.num0)
#TITLE (dn) Computes the univariate statistics for the variables of a /dn/
#DESCRIPTION
# Computes the univariate statistics for the variables of a /dn/ storing
# the result in a list of names: the components of \code{rbsb.sna}.
#DETAILS
#KEYWORDS 
#PKEYWORDS 
#INPUTS
#{dn}<<The /dn/ object.>>
#[INPUTS]
#{qui} <<(\code{numeric} or \code{character}) indicates
#          the variable on which it must be computed.
#          default, all the variates.>>
#VALUE
# A list for each type of variable natures, each component providing 
# standard statistics.
#EXAMPLE
# rsdn3k("RESET"); ## for R checking convenience
# dn2ustat(rbsb.dn2);
# dn2ustat(rbsb.dn4);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_12_28
#REVISED 10_03_09
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    valid8dn(dn);
    check4tyle(qui,c("numeric","character"),-1,"within dn2score");
}
# determining the active variables
nbv <- length(dn@df);
if (isvide(qui)) {
    qui <- sj(nbv);
} else {
    if (is.numeric(qui)) {qui <- numero(qui,sj(nbv));
    } else { qui <- numero(qui,names(dn@df));}                          
}
# initializing
res <- vector("list",length(rbsb.sna));
names(res) <- rbsb.sna;
# looping on the different kinds of nature
for (nn in rbsb.sna) {
    que <- which(dn@nat==nn);
    que <- intersect(que,qui);
    rere <- NULL;
    #
    if (nn %in% c("conti","integ")) {
        if (length(que) > 0) {
            rr <- df2ustat(dn@df[,que,drop=FALSE],nbmin=rbsb.mnu);
            rere <- matrix(NA,length(rr[[1]]),length(rr));
            dimnames(rere) <- list(names(rr[[1]]),names(rr));
            for (ri in sjl(rr)) {
                rere[,ri] <- rr[[ri]];
            }
        }
    }
    #
    if (nn %in% c("cateo","categ")) {
        if (length(que) > 0) {
            rere <- df2ustat(dn@df[,que,drop=FALSE],nbmin=rbsb.mnu);
        }
    }
    res[[nn]] <- rere;
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
dn2bstat <- function(dn,qui=rbsb.num0)
#TITLE (dn) Computes the bivariate statistics for the variables of a /dn/
#DESCRIPTION
# Computes the bivariate statistics for the variables of a /dn/ 
# The result is given by a pseudo-correlation matrix.
#DETAILS
# See \code{df2bstat} for the details.
#KEYWORDS 
#PKEYWORDS 
#INPUTS
#{dn}<<The /dn/ object.>>
#[INPUTS]
#{qui} <<(\code{numeric} or \code{character}) indicates
#          the variable on which it must be computed.
#          default, all the variates.>>
#VALUE
# A symmetric matrix of 'correlations'.
#EXAMPLE
# rsdn3k("RESET"); ## for R checking convenience
# rbsb.mnb <- 8;
# dn2bstat(rbsb.dn2);
# dn2bstat(rbsb.dn4);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_12_29
#REVISED 10_03_09
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    valid8dn(dn);
    check4tyle(qui,c("numeric","character"),-1,"within dn2score");
}
# determining the active variables
nbv <- length(dn@df);
if (isvide(qui)) {
    qui <- sj(nbv);
} else {
    if (is.numeric(qui)) {qui <- numero(qui,sj(nbv));
    } else { qui <- numero(qui,names(dn@df));}                          
}
# computing
res <- df2bstat(dn@df[,qui,drop=FALSE],nbmin=rbsb.mnb);
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
score4dn <- function(dn)
#TITLE (dn) Completes a dn with its score
#DESCRIPTION
# Completes a dn with its score, that is a last variable
# is added domains are computed according to the normal 
# distribution
#DETAILS
#KEYWORDS 
#PKEYWORDS 
#INPUTS
#{dn}<<The /dn/ object to be completed.>>
#[INPUTS]
#VALUE
# the completed /dn/.
#EXAMPLE
# rsdn3k("RESET"); ## for R checking convenience
# score4dn(rbsb.dn4);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_12_29
#REVISED 09_12_29
#--------------------------------------------
{
# checking
if (rbsb.mck) { valid8dn(dn);}
# computing the score
sco <- dn2score(dn);
# adding the variable
res <- dn;
res@df <- cbind(dn@df,sco);
names(res@df) <- c(names(dn@df),rbsb.scn);
res@description@comm <- c(dn@description@comm,"(completed with the outlying score)");
res@nat <- c(dn@nat,"conti");
res@pod <- c(dn@pod,list(range(sco)));
res@red <- c(dn@red,list(range(sco)));
res@cod <- c(dn@cod,list(quantile(sco,probs=c(0.05,0.95))));
# checking
if (rbsb.mck) { valid8dn(res);}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
draw8sele <- function(ind,E,nat,
                win=new("win",wgt=rep(1,length(nat)),
                        k=2,di=c(0,1),nb=c(1,Inf),ty="median1")
                    )
#TITLE (dn) returns the selected individuals for
#           an empidata node.
#DESCRIPTION
# The description of the algorithm is given into
# /rbsb/ manual. The selection is monitored by the
# /win/.
#DETAILS
# win@ty slot is not used since only the selection
# is performed.
#PKEYWORDS empidata
#KEYWORDS
#INPUTS
#{ind} <<Value of the individual to predict (one row
#        data.frame.>>
#{E} <<data.frame with the candidates in its rows.>>
#{nat} <<nature of the E variables (character).>>
#[INPUTS]
#{win} <<(=new("win",wgt=rep(1,length(nat)),
#                        k=2,di=1,nb=c(1,Inf),ty="median1"),
#        the /win/ to be used for the selection process.>>
#VALUE
# a list with components:\cr
# $df: a data.frame with the same columns as those of 'E'
# containing the selected subset of candidates. This one
# can be empty, in that case the returned data.frame has
# got no row.\cr
# $di_x: the maximum distance of the subset of returned candidates
#EXAMPLE
# rsdn3k("RESET"); # to comply R checking
# E <- data.frame(A=1:100,c=100:1); # the empirical distribution
# win <- new("win",wgt=1,k=0,di=c(0,1),nb=c(1,2));
# draw8sele(data.frame(A=20),E,c("conti","conti"),win);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
# in this version, a modification was introduced
# to always get an odd number of selected candidates
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_07_05
#REVISED 10_04_23
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    valid8win(win);
    check4tyle(ind,"data.frame",-1,"ind of draw8sele");
    check4tyle(  E,"data.frame",-1,"  E of draw8sele");
}
# when there is no candidate
if (nrow(E) == 0) {
    return(E);
}
# computing the distances for the selection
DIDI <- draw8dist(ind,E,nat,win);
# making the selection of retained individuals
Nd <- rep(NA,length(win@di));
for (ii in sjl(win@di)) {
    Nd[ii] <- sum(DIDI<=win@di[ii]);
}
Nd[1] <- 1+sum(DIDI<win@di[1]);
# looking for the best solution
nbste <- length(win@di) - 1;
eux <- numeric(0);
if (Nd[1] <= Nd[2]) { for (ii in sj(nbste)) { if (length(eux)==0) {
    bynb <- win@nb[1]:win@nb[1+ii];
    bydi <-     Nd[1]:    Nd[1+ii];
    eux <- intersect(bynb,bydi);
}}}
# nbre is the number of selected candidates
nbre <- length(eux);
# getting the happy candidates
if (nbre==0) {
    SSS <- integer(0);
    mdidi <- Inf;
} else {
    ooo <- order(DIDI);
    SSS <- ooo[eux];
    mdidi <- max(DIDI[SSS]);    
}
#
# returning
list(df=E[SSS,],di_x=mdidi);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
draw8dist <- function(ind,E,nat,
                win=new("win",wgt=rep(1,length(nat)),
                        k=2,di=c(0,1),nb=c(1,Inf),ty="median1")
                    )
#TITLE (dn) returns the distances for the selection to be
#           operated with an empidata node.
#DESCRIPTION
# The description of the algorithm is given into
# /rbsb/ manual. The selection is monitored by the
# /win/.
#DETAILS
# win@ty slot is not used since only the distance
# values are computed.
#KEYWORDS
#PKEYWORDS empidata
#INPUTS
#{ind} <<Value of the individual to predict (one row
#        data.frame.>>
#{E} <<data.frame with the candidates in its rows.>>
#{nat} <<natures of the E variables (character).>>
#[INPUTS]
#{win} << the /win/ to be used for the selection process.>>
#VALUE
# a numeric vector of length nrow(E) containing the distance
# for each row of E. Of course, it will be \code{numeric(0)}.
#EXAMPLE
# rsdn3k("RESET"); # to comply R checking
# E <- data.frame(A=1:100,c=100:1); # the empirical distribution
# win <- new("win",wgt=1,k=0,di=c(0,1),nb=c(1,2));
# draw8dist(data.frame(A=20),E,c("conti","conti"),win);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
# It seems that draw8dist can produce NA values which is odd?
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_07_06
#REVISED 10_04_30
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    valid8win(win);
    check4tyle(ind,"data.frame",-1,"ind of draw8dist");
    check4tyle(  E,"data.frame",-1,"  E of draw8dist");
}
nbc <- ncol(ind); nbv <- ncol(E);
# ??? is this transformation logical ???
names(ind) <- nv2nv(names(ind))$vva;
if (rbsb.mck) {
    if (length(union(names(E),names(ind)))!=nbv) {
        erreur(list(names(E),names(ind)),
               "E must comprise all ind variables");
    }
    if (nrow(ind) != 1) {
        erreur(ind,"One individual at once");
    }
    check4tyle(nat,"character",nbv,"nat in draw8dist");
    for (na in nat) { if (!(na %in% rbsb.sna)) {
        erreur(nat,"Not accepted nature!");
    }}
}
# positions of ind variables into E
posE <- match(names(ind),names(E));
# isolating the natures of the conditioning variables
natc <- nat[posE];
# Forseeing the degenerate case
if (win@k==0) { win@k <- 1; klimi <- TRUE; 
} else { klimi <- FALSE; }
#
# the algorithm
#
if (nrow(E)==0) {
    return(numeric(0));
}
# getting a matrix with rows associated to 
# those of E and columns associated to ind
# to store the differences
# ??? Be careful the differences for the factors
#     will be evaluated according to the uncontrolled
#     way of their numerical coding ???
# For equality versus non-equlity: no harm
#     but in the future?
didi <- matrix(NA,nrow(E),ncol(ind));
# the elementary distances
twni <- twci <- numeric(0);
for (ina in sjl(natc)) {
    # unknown types are ignored
    if (rbsb.snp[natc[ina],"numeric"]) {
        didi[,ina] <- win@wgt[ina]*abs(E[,posE[ina]]-ind[1,ina])^win@k;
        twni <- c(twni,ina);
    }
    if (rbsb.snp[natc[ina],"categoric"]) {
        didi[,ina] <- win@wgt[ina]*(as.character(E[,posE[ina]]) 
                                    != as.character(ind[1,ina]));
        twci <- c(twci,ina);
    }
}
# the global distances on the parent variables
DINU <- DICA <- 0;
SW <- sum(win@wgt[c(twni,twci)]);
if (SW <= 0) {
    erreur(list(win@wgt,win@wgt[c(twni,twci)],SW),
           "The sum of the used weigths is not strictly positive!");
}
# for the numeric part
if (length(twni)>0) {
    if (klimi) {
        DINU <- apply(didi[,twni,drop=FALSE],1,max)/SW;
    } else {
        DINU <- (apply(didi[,twni,drop=FALSE],1,sum)/SW)^(1/win@k);
    }
}
# for the categoric part
if (length(twci)>0) {
    DICA <- apply(didi[,twci,drop=FALSE],1,sum)/SW;
}
# the global distance
DIDI <- DINU + DICA;
# dealing with the NA
DIDI[is.na(DIDI)] <- Inf;
#form3affiche(cbind(E,didi,DINU,DICA,DIDI),T);
#
# returning
DIDI;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
dn2csv <- function(dn,fi)
#TITLE (dn) creates a csv file containing dn@df
#DESCRIPTION
# Produces a new text file named \code{fi} with
# the slot \code{dn@df}.
#DETAILS
#KEYWORDS IO
#PKEYWORDS 
#INPUTS
#{dn} <<The /dn/.>>
#{fi} <<The file name..>>
#[INPUTS]
#VALUE
# Nothing, the file is created. It can be read back 
# with \code{read.csv(fi,row.names=1,check.names=FALSE)}.
#EXAMPLE
# rsdn3k("RESET"); # to comply R checking
# dn2csv(rbsb.dn2,"toto.txt");
# read.csv("toto.txt",row.names=1,check.names=FALSE);
# unlink("toto.txt");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_04_27
#REVISED 10_04_27
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    valid8dn(dn);
    check4tyle(fi,"character",1,"file to be created 'fi' of dn2csv");
}
#
wold <- options()$width;
options(width=5000);
write.csv(dn@df,fi);
#
options(width=wold);
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
