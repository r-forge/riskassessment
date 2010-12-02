
###########################################
###########################################
########
#((((((( NEW S4 CLASS dn
########
###########################################
###########################################

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8dn <- function(object)
#TITLE checks a /dn/
#DESCRIPTION (dn)
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
# rebastaba3k("RESET"); # for R checking convenience
# valid8dn(rebastaba.dn3);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_11_25
#REVISED 10_06_23
#--------------------------------------------
{
    if (class(object)!="dn") {
        erreur(NULL,paste("This object is not of class 'dn' but '",class(object),"'",sep=""));
    }
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
    for (vv in bc(nbv)) {
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
    } else { erreur(res,w=rebastaba.mwa);}
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
#TITLE prints a /dn/
#DESCRIPTION (dn)
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
#{ndec} << number of decimals used to print the values of the numeric variables.>>
#{simu} <<, which simulated values to print?\cr
#          when 0: no simulated values are printed,\cr
#          when a vector: indicates the numbers of simulations to print
#                         (default = the first ten),\cr
#          when a value: the percentage to be printed
#                         (50 = half of the simulations)
#       >>
#VALUE
#  returns nothing but a printing is performed
#EXAMPLE
# rebastaba3k("RESET");
# print(rebastaba.dn2);
# rebastaba.mnu <- 8;
# print(score4dn(rebastaba.dn2));
# print(rebastaba.dn4,quoi="a");
# print(rebastaba.dn4,sorting="A");
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
if (rebastaba.mck) {
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
if (isvide(qui)) { qui <- bf(vardn);
} else {
    if (is.numeric(qui)) { qui <- numero(qui,bf(vardn));
    } else {qui <- numero(qui,vardn);}
}
# their translation for numeric and categoric variables
# into variable names
quinum <- vardn[intersect(which(rbsb.snp[x@nat,"numeric"]),qui)];
quicat <- vardn[intersect(which(rbsb.snp[x@nat,"categoric"]),qui)];
quitou <- vardn[qui];
nsou <- 49; # length of some tag line of the printing
form3repeat("-",nsou,TRUE);
# the title
if(expr3present("t",quoi)) {
  cat("dn object of name \"",x@description@name,"\"\n",sep="");
  ti <- paste("There are",nbva,"variables and",nbsi,"simulations");
  if (length(qui) < nbva) { ti <- paste(ti," but only ",length(qui),"variables are considered here.");}
  form3titre(ti); 
  form3repeat("-",nsou,TRUE);
}
# the size
if (expr3present("d",quoi)) {
    print(x@description,rebastaba.emdn);
}
# the variable names
if (expr3present("v",quoi)) {
  cat("  Variable names are:\n");
  for (i in 1:nbva) {
    if (i == 1) { cat("( ");} else {cat(" ; ");}
    cat(vardn[i]);
    if (i == nbva) { cat(" )\n");}
  }
  form3repeat("-",nsou,TRUE);
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
  form3repeat("-",nsou,TRUE);
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
  form3repeat("-",nsou,TRUE);
}}
# univariate statistics
if (expr3present("u",quoi)) { if (length(quitou)>0) {
  cat("The univariate statistics\n\n");
  print(dn2ustat(x,qui));
  form3repeat("-",nsou,TRUE);
}}
# bivariate statistics
if (expr3present("b",quoi)) { if (length(quitou)>0) {
  cat("The bivariate statistics\n\n");
  print(round(dn2bstat(x,qui),ndec));
  form3repeat("-",nsou,TRUE);
}}
invisible();    
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
plot8dnco <- function(x,red,cod,title="",type="smooth",...)
#TITLE univariate plot for a continuous variable
#DESCRIPTION (dn)
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
# rebastaba3k("RESET"); # for R checking convenience
# plot8dnco(rebastaba.dn4@df[[1]],red=rebastaba.dn4@red[[1]],cod=rebastaba.dn4@cod[[1]]);
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
if (rebastaba.mck) {
    check4tyle(x,"numeric",-1,message="A continuous variable is expected",na.rm=TRUE);
    check4tyle(red,"numeric",2,message="A range is expected");    
    check4tyle(cod,"numeric",2,message="A range is expected");    
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
#TITLE univariate plot for a categoric variable
#DESCRIPTION (dn)
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
# rebastaba3k("RESET"); # for R checking convenience
# plot8dnca(rebastaba.dn4@df[[5]],red=rebastaba.dn4@red[[5]],cod=rebastaba.dn4@cod[[5]]);
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
if (rebastaba.mck) {
    if (!is.factor(f)) {
        erreur(f,"A factor is expected for a categoric variable");
    }
    check4tyle(red,"character",-1,message="A range of character is expected");    
    check4tyle(cod,"character",-1,message="A range of character is expected");    
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
#TITLE bivariate plot for two continuous variables
#DESCRIPTION (dn)
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
# rebastaba3k("RESET"); # for R checking convenience
# plot8dncoco(rebastaba.dn4@df[,1:2],rred=list(rebastaba.dn4@red[[1]],rebastaba.dn4@red[[2]]),ccod=list(rebastaba.dn4@cod[[1]],rebastaba.dn4@cod[[2]]),xlab="A",ylab="B");
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
if (rebastaba.mck) {
    if (!(is.matrix(xx) | is.data.frame(xx))) {
        erreur(xx,"'xx' must be data frame or matrix");
    }
    if (ncol(xx) < 2) {
        erreur(xx,"'xx' must have at least two columns");
    }
    check4tyle(xx[,1],"numeric",-1,message="A continuous first variable is expected",na.rm=TRUE);
    check4tyle(xx[,2],"numeric",-1,message="A continuous second variable is expected",na.rm=TRUE);
    check4tyle(rred,"list",c(2,Inf),message="A list with at least two components is expected.");
    check4tyle(ccod,"list",c(2,Inf),message="A list with at least two components is expected.");
    check4tyle(rred[[1]],"numeric",2,message="A continuous first range is expected");    
    check4tyle(rred[[2]],"numeric",2,message="A continuous second range is expected");    
    check4tyle(ccod[[1]],"numeric",2,message="A continuous first range is expected");    
    check4tyle(ccod[[2]],"numeric",2,message="A continuous second range is expected");    
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
#TITLE bivariate plot for a continuous and a categoric variables
#DESCRIPTION (dn)
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
# rebastaba3k("RESET"); # for R checking convenience
# par(mfrow=c(2,1));
# plot8dncoca(rebastaba.dn4@df[,c(1,4)],rred=list(rebastaba.dn4@red[[1]],rebastaba.dn4@red[[4]]),ccod=list(rebastaba.dn4@cod[[1]],rebastaba.dn4@cod[[4]]),xlab="A",ylab="density",title="Variable D");
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
if (rebastaba.mck) {
    if (!is.data.frame(xx)) {
        erreur(xx,"'xx' must be a data frame");
    }
    if (ncol(xx) < 2) {
        erreur(xx,"'xx' must have at least two columns");
    }
    check4tyle(xx[,1],"numeric",-1,message="A continuous first variable is expected");
    if (!is.factor(xx[,2])) {
        erreur(xx,"A factor is expected as second variable");
    }
    check4tyle(rred,"list",c(2,Inf),message="A list with at least two components is expected.");
    check4tyle(ccod,"list",c(2,Inf),message="A list with at least two components is expected.");
    check4tyle(rred[[1]],"numeric",2,message="A continuous first range is expected");    
    check4tyle(rred[[2]],"character",-1,message="A categoric second range is expected");    
    check4tyle(ccod[[1]],"numeric",2,message="A continuous first range is expected");    
    check4tyle(ccod[[2]],"character",-1,message="A categoric second range is expected");    
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
#TITLE bivariate plot for two categoric variables
#DESCRIPTION (dn)
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
# rebastaba3k("RESET"); # for R checking convenience
# plot8dncaca(rebastaba.dn4@df[,c(4,5)],rred=list(rebastaba.dn4@red[[4]],rebastaba.dn4@red[[5]]),ccod=list(rebastaba.dn4@cod[[4]],rebastaba.dn4@cod[[5]]),title="Variables D & E");
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
if (rebastaba.mck) {
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
    check4tyle(rred,"list",c(2,Inf),message="A list with at least two components is expected.");
    check4tyle(ccod,"list",c(2,Inf),message="A list with at least two components is expected.");
    check4tyle(rred[[1]],"character",-1,message="A categoric first range is expected");    
    check4tyle(rred[[2]],"character",-1,message="A categoric second range is expected");    
    check4tyle(ccod[[1]],"character",-1,message="A categoric first range is expected");    
    check4tyle(ccod[[2]],"character",-1,message="A categoric second range is expected");    
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
#TITLE Plots the numeric or categoric variables of a dn object
#DESCRIPTION (dn)
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
# rebastaba3k("RESET"); # for R checking convenience
# plot(rebastaba.dn1);
# plot(rebastaba.dn1,vary=-1);
# plot(rebastaba.dn4,varx="A",vary="B");
# plot(rebastaba.dn4,varx="A",vary="D");
# plot(rebastaba.dn4,varx="D",vary="A");
# plot(rebastaba.dn4,varx="E",vary="F");
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
        for (i in bf(vvy)) { for (j in bf(vco)) {
            varx <- c(varx,vco[j]);
            vary <- c(vary,vvy[i]);
        }}
        # non common x common couples
        for (i in bf(vco)) { for (j in bf(vvx)) {
            varx <- c(varx,vvx[j]);
            vary <- c(vary,vco[i]);
        }}
        # non common x non common couples
        for (i in bf(vvy)) { for (j in bf(vvx)) {
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
if (rebastaba.mck) {
    if (!is.null(titles)) {
        check4tyle(titles,"character",nbg,message="A title must be given for each graph, if not NULL");
    }
    if (!is.null(stitles)) {
        check4tyle(stitles,"character",nbg,message="A sub-title must be given for each graph, if not NULL");
    }
    if (!is.null(xxlab)) {
        check4tyle(xxlab,"character",nbg,message="An xlab must be given for each graph, if not NULL");
    }
    if (!is.null(yylab)) {
        check4tyle(yylab,"character",nbg,message="An ylab must be given for each graph, if not NULL");
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
draw8empidata <- function(X,E,win,nat,bnid,
                           strict=FALSE) 
#TITLE performs an empidata [multivariate] draw or prediction
#DESCRIPTION (dn)
# According to parent values provided in \code{X}, returns draws
# for simulation from the \code{E} data frame. The returned variables
# are defined by \code{names(win@nat)}, the subset selection is defined by \code{win}.
# See the detail section for the management of missing values.\cr
# When draws are done individually (within a loop),
# each time a draw is done, a record indicating the size 
# of the candidate subset is appended to the file of name 'rebastaba.efi'.\cr
# Notice that the parent names (\code{names(win@swg)}) and variable
# to extract (\code{names(win@nat)}) are given at the variable level.
#DETAILS 
#  To allow the successive links of empidata nodes
#  the node name was eliminated from the correspondance. E.g. let 
#  suppose that we are dealing witht the sequence of 
#  \code{SEX -> E[HGT] -> F[WGT]} where \code{E} and \code{F} are
#  associated to empidata bases. In the adopted way, this implies
#  that the corresponding parents \code{E[SEX]} and \code{F[HGT]}
#  exists with the reduced names \code{SEX} for the base \code{E}
#  and \code{HGT} for the base \code{F}.\cr
#  Notice that implies that the same variable name can be used as
#  co-parents for a given node. E.g. \code{E1[HGT]} and \code{E2[HGT]}
#  cannot be simultaneous parents of \code{F[WGT]}.
#  \cr
#  Missing values can be dealt in two ways. Here are the
#  rules which are applied.\cr
#  Let us denote X[parents], E[parents] and E[variables], respectively,
#  the inherited parents from the previous simulated values, the
#  corresponding parents in the data base 
#  and the variables which are drawn.
#{1} <<When there are missing values in E[variables], either they
#      are left (strict=FALSE) either they are prevented, forbidding
#      these observations to be drawn.>>
#{2} <<When there are parents, any missingness in X[parents] leads to
#      missing values in all E[variables] since there is no
#      way to know the corresponding observations in E data base..>>
#{3} <<For the same reason, when there are parents, all observations
#      with missing values in E[parents] are discarded.>>
#KEYWORDS
#PKEYWORDS 
#INPUTS
#{X} << The matrix of values of the already simulated values.
#       It must comprises the
#       parents. If there is no parents, the matrix must be given (even
#       with no columns) just to provide the number of simulations by its
#       row number.>>
#{E} << The data frame to be used for the empidata distribution.
#       It must comprise all possible parents (WITH node name and
#       brackets) and variables (WITHOUT the node name and brackets).>>
#{win} << /win/ object to be used for the selection. Notice that
#       \code{win@swg} indicates the parents and \code{names(win@nat)}
#        indicates the variables of the nodes.>>
#{nat} << Named vector of the nature parents (possibly more,
#         usually those of the \code{X} columns).>>
#{bnid} << character identifying the bn (for indication in the
#          rebastaba.efi sinking.>>
#[INPUTS]
#{strict} << Must the elimination of missing values be common
#           to all variables of the node?>>
#VALUE
# A data.frame of size \code{nrow(X)} times \code{length(win@nat)}
#  with the simulated values in the right order.
#EXAMPLE
# rebastaba3k("RESET"); # to comply R checking
# E <- data.frame(A=1:100,c=100:1,w=round(sin(1:100),2)); # the empirical distribution
# X <- as.data.frame(matrix(20:29,10,1,dimnames=list(NULL,"A"))); # the covariate values
# win <- new("win",nat=structure("conti",.Names="c"),
#                  swg=structure(1,.Names="A"),skk=0,sdi=c(0,1),snb=c(1,2));
# draw8empidata(X,E,win,nat=structure("conti",.Names="A"),bnid="rebastaba.bn2");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_17
#REVISED 10_09_02
#--------------------------------------------
{
#
# some checking and preparation of constants
#
if (rebastaba.mck) {
    check4tyle(E,"data.frame",-1);
    check4tyle(X,"data.frame",-1);
    check4valid(valid8win(win),"draw8empidata");
    check4tyle(nat,"ncharacter",-1);
    if (!expr3present(parents8win(win),names(nat),exact=TRUE,how='a')) {
        erreur(list(win,nat),"Not all the parent variables are available in 'nat'");
    }
    if (!expr3present(nat,rbsb.sna,how='a')) {
        erreur(list(nat,rbsb.sna),"Some of the 'natures' given by 'nat' are not accepted");
    }
    check4tyle(bnid,"character",1);
    check4tyle(strict,"logical",1);
}
#
# getting the different names
#
nvparents <- parents8win(win);
vparents <- nv2nv(nvparents)$vva;
vari <- names(win@nat);
#
# dealing with missing values in E
#
# according to the strictness
if (strict) {
    # restriction of the drawing to the complete variable set
    if (length(vari) > 1) {
        E <- E[apply(!is.na(E[,vari]),1,all),,drop=FALSE];
    } else {
        E <- E[!is.na(E[,vari]),,drop=FALSE];
    }
}
# In case of parents, removing the incomplete observations of the base
if (length(vparents) > 0) {
    if (length(vparents) > 1) {
        E <- E[apply(!is.na(E[,vparents]),1,all),,drop=FALSE];
    } else {
        E <- E[!is.na(E[,vparents]),,drop=FALSE];
    }
}  
#
# two main constants
#
nbs <- nrow(X); nbe <- nrow(E);
#
# some degenerate cases
#
if (nbs < 1) {
    res <- as.data.frame(matrix(NA,0,length(vari)));
    names(res) <- vari;
    return(res);
}
if (nbe < 1) {
    res <- as.data.frame(matrix(NA,nbs,length(vari)));
    names(res) <- vari;
    return(res);
}
#
# Following the checking
#
if (rebastaba.mck) {
    ava <-  dimnames(E)[[2]];
    if (!all(vari %in% c(ava,names(rebastaba.ena)))) {
	cat("The proposed variables to generate are:",vari,"\n");
	cat("The available variables in the data frame are:",ava,"\n");
	cat("The specific variables for data frame are:",names(rebastaba.ena),"\n");
	erreur(vari,"some are missing!" );
    }
    if (!all(vparents %in% ava)) {
	cat("The proposed parents are: <",nvparents,">\n");
	cat("They were interpreted as: <",vparents,"> for the data frame correspondence\n");
	cat("But the available variables in the data frame are: <",ava,">\n");
	erreur(NULL,"The parents are not in the empidata data frame!" );
    }
    if (!all(nvparents %in% dimnames(X)[[2]])) {
	cat("parents are:",nvparents,"\n");
	cat("the variables in the simulated matrix are:",dimnames(X)[[2]],"\n");
	erreur(NULL,"The parents are not present in the already simulated variables!" );
    }
}
#
# Restricting the E data frame to the useful variables
#
uvari <- setdiff(vari,names(rebastaba.ena));
E <- E[,unique(c(vparents,uvari)),drop=FALSE];
#
# Giving the general trait of the drawing in rebastaba.efi file
#
sink(rebastaba.efi,TRUE);
dvar <- form3liste(vari,OPA="[ ",CPA=" ]",opa="",cpa="",sep=" / ")
form3titre(paste("/bn/ =",bnid," |  variable(s) =",dvar),10);
form3titre(paste("At time",now()),1);
form3titre(paste("empidata of size",nbs),1);
form3titre(paste("The draw is made from a population of size",nbe),1);
form3titre("The draw is monitored from the following /win/:",2);
print8win(win);
if (rebastaba.esu) {
    form3titre("As rebastaba.esu is TRUE selected subsets will be introduced for each draw\n",);
} else {
    form3titre("As rebastaba.esu is FALSE selected subsets will not be introduced for each draw\n",1);
}
sink();
#
# adding the computed variable to the base if necessary
#
if (names(rebastaba.ena)[1] %in% vari) {
    # subset sizes are required
    xxx <- rep(nrow(E),nrow(E));
    E <- cbind(E,xxx);
    dimnames(E)[[2]][ncol(E)] <- names(rebastaba.ena)[1];
    nat <- c(nat,"integ");
    names(nat)[length(nat)] <- names(rebastaba.ena)[1];
}
if (names(rebastaba.ena)[2] %in% vari) {
    # maximum distances are required
    xxx <- rep(NA,nrow(E));
    E <- cbind(E,xxx);
    dimnames(E)[[2]][ncol(E)] <- names(rebastaba.ena)[2];
    nat <- c(nat,"conti");
    names(nat)[length(nat)] <- names(rebastaba.ena)[2];
}
#
# Two types of draws are possible: globally or individually
#
if (win@rty[2] %in% c("systematic","random")) { globally <- TRUE;
} else { globally <- FALSE;}
#
if (globally) {
    ## the draw can be global
    res <- draw8predg(nbs,E,win);
} else {
    # preparing the resulting structure
    res <- as.data.frame(matrix(NA,nbs,length(vari)));
    names(res) <- vari;
    #
    # drawing with an awful loop
    #
    for (hd in bc(nbs)) {
        # we are working with the hd-th simulation
        # selecting
        EE <- draw8sele(X[hd,nvparents,drop=FALSE],E,
                        win@swg,win@skk,win@sdi,win@snb,nat);
        EE <- EE[,vari,drop=FALSE];
        # predicting
        tirage <- draw8predi(X[hd,nvparents,drop=FALSE],
                            EE,win,nat);
        #
        res[hd,] <- tirage;
    }
}
#
# restricting to the variables
#
if (length(vparents)>0) { res <- res[,vari,drop=FALSE];}  
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
draw8multinom <- function(n,size,proba,levels=NULL)
#TITLE simulates random draw from a multinomial
#DESCRIPTION (dn)
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
# When !is.null(levels) size is forced to be a vector of
# ones.
#KEYWORDS
#PKEYWORDS simulation
#INPUTS
#{n} <<Number of draws to obtain.>>
#{size} <<Vector [n] of the sizes of the multinomials.>>
#{proba} <<Matrix [n,k] of the probabilities of the multinomials.>>
#[INPUTS]
#{levels} << When a categoric variable must be returned,
#           levels provides its levels (since it is a factor).>>
#VALUE
# when is.null(levels) the integer matrix [n,k] of the draws is
# returned else a factor.
#EXAMPLE
# rebastaba3k("RESET"); # for the sake of R checking
# draw8multinom(10,1:10,matrix(1:10,10,3,byrow=TRUE));
# draw8multinom(10,rep(1,10),matrix(1:10,10,3,byrow=TRUE),levels=LETTERS[1:3]);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_30
#REVISED 10_06_30
#--------------------------------------------
{
# imposing
if (!is.null(levels)) { size <- rep(1,n);}
# some checking
if (rebastaba.mck) {
    if (!is.matrix(proba)) { erreur(proba,"'proba' must be a matrix");}
    if (nrow(proba) != n)  { 
        erreur(list(n,dim(proba)),"This function needs a proba matrix [n,*]!");
    }
    if (length(size) != n) {
	 if (is.matrix(size)) {
             # ??? I wonder if this possibility of matrix is very useful!!!
	     if (nrow(size) != n) {
		 erreur(list(dim(size),n),"When size is a matrix, it must have n rows!");
	     } else {
		if (!all(outer(size[,1],rep(1,ncol(size)),"*") == size)) {
		     erreur(size,"This function expects a matrix with equal columns");
		}
	     }
	     size <- size[,1];
	 } else {
	     erreur(size,"This function needs a vector of size n =",n,
		    "or a matrix with n rows!");
	 }
    }
}
#
k <- ncol(proba);
if (!is.null(levels)) {
    if (length(levels) != k) {
        erreur(list(dim(proba),levels),"When levels is not null, its length must equal ncol(proba)");
    }
}
res <- matrix(NA,n,k);
for (i in bc(n)) { res[i,] <- rmultinom(1,size[i],proba[i,]);}
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
#TITLE simulates random draws from a Dirichlet
#DESCRIPTION (dn)
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
#TITLE simulates random draws for a
#           numcat distribution
#DESCRIPTION (dn)
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
# rebastaba3k("RESET"); # For R checking
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
    for (ck in bf(parents)) {
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
for (i in bc(n)) { resu[i] <- which(1==rmultinom(1,1,PROBA[i,]));}
# replacing by the desired values
resu <- factor(lev[resu],levels=lev);
res <- data.frame(row.names=1:length(resu));
res[[1]] <- resu;
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
draw8predg <- function(nbs,EE,win)
#TITLE returns the predicted individuals for
#           an empidata node.
#DESCRIPTION (dn)
# returns the predicted individuals for
# an empidata node when they are drawn globally.
# The description of the algorithm is given into the
# /rebastaba/ manual. The prediction is monitored by the
# /win/.
#DETAILS
#KEYWORDS
#PKEYWORDS empidata
#INPUTS
#{nbs} <<number of individuals to predict.>>
#{EE} <<data.frame with the candidates in its rows.>>
#{win} << the /win/ to be used for the selection process.>>
#[INPUTS]
#VALUE
# a data.frame of \code{nbs} rows and all columns of \code{EE}.
#EXAMPLE
# rebastaba3k("RESET"); # to comply R checking
# EE <- data.frame(A=1:100,c=100:1); # the empirical distribution
# win <- new("win",nat=structure("conti",.Names="c"),swg=structure(1,.Names="A"),skk=0,sdi=c(0,1),snb=c(1,20));
# draw8predg(20,EE,win);
# win2 <- new("win",rty=c("*","systematic"));
# draw8predg(10,EE,win2);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_06_26
#REVISED 10_09_02
#--------------------------------------------
{
# preparing, eliminating the degenerate cases
if (ncol(EE)==0) {
    res <- as.data.frame(matrix(NA,nbs,0));
    return(res);
}
# checking that rebastaba.ena variables are not introduced
if (length(intersect(names(rebastaba.ena),names(EE)))>0) {
    erreur(names(rebastaba.ena),
           "These variables cannot be asked for global draw of @rty[2] =",
           win@rty[2]);
}
res <- as.data.frame(matrix(NA,nbs,length(EE)));
names(res) <- names(EE);
if (nrow(EE)==0) {
    return(res);
}
# 
# the algorithm
#
fait <- FALSE;
#
if (win@rty[2]=="random") {
    # just getting one among the possible
    # getting the weights
    if (length(win@rty) > 2) { ww <- abs(EE[,win@rmo]);
    } else { ww <- rep(1,nrow(EE));}
    ww <- ww / sum(ww);
    # drawing
    if (win@rty[1] == "1") {
        uu <- sample(nrow(EE),nbs,replace=TRUE,prob=ww);
        res <- EE[uu,,drop=FALSE];
    } else {
        for (ii in bc(ncol(EE))) {
            res[,ii] <- EE[sample(nrow(EE),nbs,replace=TRUE,prob=ww),ii];
        }
    }
    #
    fait <- TRUE;
}
#
if (win@rty[2] %in% c("systematic")) {
    # checking the nature compatibility with the variables
    if (length(win@rmo)>0) {if (!is.numeric(EE[,win@rmo])) {
        erreur(win,"A numeric variable was expected for @rmo");
    }}
    # computing the frequency
    if (length(win@rk2)==0) {
        f1 <- nrow(EE);
        f2 <- (nbs%%f1);
        #f3 <- f1%/%f2;
        # The previous fails sometime when f2 is zero ???
        f3 <- round(f1)%/%round(f2);
        freq <- min(f1-1,f3);
    } else { freq <- win@rk2;}
    # determing the monitoring value
    if (length(win@rmo)==0) { moni <- bc(nrow(EE));
    } else { moni <- EE[,win@rmo];}
    # drawing
    if (win@rty[1] == "1") {
        uu <- systematic(moni,nbs,freq);
        res <- EE[uu,,drop=FALSE];
    } else {
        for (ii in bc(ncol(EE))) {
	    uu <- systematic(bc(nrow(EE)),nbs,freq);
	    res[,ii] <- EE[uu,ii];
        }
    }
    #
    fait <- TRUE;
}
#
#
if (!fait) {
    rapport(paste("This win@rty[2] (",win@rty[2],") was not detected as compatible with 'draw8predg'",sep=""));
} else {
    # when asked, look for the closest individual
    if (win@rty[1]=="0") { 
        wiwi <- win;
        wiwi@rty[2] <- "random";
	# introducing as parents all variables of EE
        # with the required weights to compute the distance
	if (length(win@rwg) > 0) {
            wiwi@swg <- win@rwg;
            names(wiwi@swg) <- names(win@nat);
        } else {
            wiwi@swg <- rep(-1,length(win@nat));
            names(wiwi@swg) <- names(win@nat);
        }
        # Some weight can be negative, in that case they are
        # computed from the selected candidates with
        # possible drawbacks of non existing or null variances
        for (ii in bf(wiwi@swg)) { if (wiwi@swg[ii]<0) {
            nn <- names(wiwi@swg)[ii];
            ecty <- sqrt(var(EE[,nn],na.rm=TRUE));
            if (is.na(ecty)) { ecty <- 1;}
            # ??? the following point is not very convaincing ???
            # but the user would take his/her responsibility with
            # weights of his/her own!
            if (ecty <= 0) { ecty <- 1;}
            wiwi@swg[ii] <- ecty;
        }}
	wiwi@skk <- 0;
	wiwi@sdi <- c(0,Inf);
	wiwi@snb <- c(1,1);
        nati <- rep(rbsb.sna[4],length(EE));
        nati[sapply(EE,is.numeric)] <- rbsb.sna[1];
        names(nati) <- names(EE);
        for (ii in bc(nrow(res))) {
            res[ii,] <- draw8sele(res[ii,,drop=FALSE],EE,
                                  wiwi@swg,wiwi@skk,wiwi@sdi,wiwi@snb,nati);
        }
    }
}
#
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
draw8predi <- function(ind,EE,win,nat)
#TITLE returns the predicted individual for
#           an empidata node.
#DESCRIPTION (dn)
# returns the predicted individual for
# an empidata node when they must be drawn individually.
# The description of the algorithm is given into the
# /rebastaba/ manual. The prediction is monitored by the
# /win/.
#DETAILS
# Not all the checks are performed
#KEYWORDS
#PKEYWORDS empidata
#INPUTS
#{ind} <<Value of the individual to predict (one row
#        data.frame and only the variables associated
#        to parents) .>>
#{EE} <<data.frame with the candidates in its rows.>>
#{win} << the /win/ to be used for the selection process.>>
#{nat} <<nature of the \code{ind} variables (\code{named character}).>>
#[INPUTS]
#VALUE
# a one-row data.frame returning the predicted individual (with
#        \code{rebastaba.ena[1]} computed if asked).\cr
#EXAMPLE
# rebastaba3k("RESET"); # to comply R checking
# EE <- data.frame(A=1:100,c=100:1); # the empirical distribution
# win <- new("win",nat=structure("conti",.Names="c"),swg=structure(1,.Names="A"),skk=0,sdi=c(0,1),snb=c(1,20));
# draw8predi(data.frame(A=20),EE,win,structure("conti",.Names="A"));
# win2 <- new("win",nat=structure("conti",.Names="c"),swg=structure(1,.Names="A"),skk=0,
#                   sdi=c(0,10),snb=c(1,20),rty=c("*","quantile"),rk2=0.5);
# draw8predi(data.frame(A=20),EE,win2,structure("conti",.Names="A"));
# win3 <- new("win",nat=structure("conti",.Names="c"),swg=structure(1,.Names="A"),skk=0,sdi=c(0,10),snb=c(1,20),
#                   rty=c("0","stdev"));
# draw8predi(data.frame(A=20),EE,win3,structure("conti",.Names="A"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis & C. Bidot
#CREATED 09_06_26
#REVISED 10_09_08
#--------------------------------------------
{
# checking
if (rebastaba.mck) {
    if(!expr3present(names(ind),names(nat),exact=TRUE,how="a")) {
        erreur(list(win,nat),"'nat' seems incomplete (not all the parent variables)");
    }
    if(!expr3present(names(win@nat),names(EE),exact=TRUE,how="a")) {
        erreur(list(nat,names(EE)),"Not all variables are available in the 'empidata' frame");
    }
    if(!expr3present(variables8win(win),c(names(nat),win@rmo),exact=TRUE,how="a")) {
        varwin <- variables8win(win);
        erreur(list(varwin,nat),"'nat' seems incomplete (Not all the used variable for the draw process are available)");
    }
}
# preparing, eliminating the degenerate cases
if (ncol(EE)==0) {
    res <- as.data.frame(matrix(NA,1,0));
    return(res);
}
res <- as.data.frame(matrix(NA,1,length(EE)));
names(res) <- names(EE);
if (nrow(EE)==0) {
    return(res);
}
# 
# the algorithm
#
fait <- FALSE;
#
if (win@rty[2]=="random") {
    # just getting one among the possible
    # getting the weights
    if (length(win@rmo)>0) { ww <- abs(EE[,win@rmo]);
    } else { ww <- rep(1,nrow(EE));}
    ww <- ww / sum(ww);
    # drawing
    if (win@rty[1] == "1") {
        uu <- sample(nrow(EE),1,prob=ww);
        res <- EE[uu,,drop=FALSE];
    } else {
        uu <- sample(nrow(EE),ncol(EE),replace=TRUE,prob=ww);
        for (ii in bc(ncol(EE))) { res[1,ii] <- EE[uu[ii],ii];}
        if (win@rty[1]=="0") {
            res <- draw8sele(res,EE,win@rwg,win@rkk,c(0,Inf),c(1,1),nat);
        }
    }
    #
    fait <- TRUE;
}
#
if (win@rty[2] %in% c("median","mean","quantile","stdev")) {
    # checking the nature compatibility with the variables
    if (win@rty[1] == "1") {
        if (length(win@rmo)==0) {
            erreur(win,"The monitoring variable was not specified and is compulsory");
        }
        if (!is.numeric(EE[[win@rmo]])) {
            erreur(names(EE),"A numeric variable was expected for @rmo in EE");
        }
    } else {
        if (!all(sapply(EE,is.numeric))) {
            erreur(win,"Only numeric variables were expected in EE when @rty[1] ==",win@rty[1]," (e.g. the median is not defined for categoric variables)");
        }
    }
    # just computing some statistics
    if (win@rty[2]=="median")   {sta <- function(x) {median(x,na.rm=TRUE);}} 
    if (win@rty[2]=="mean")     {sta <- function(x) {mean(x,na.rm=TRUE);}}
    if (win@rty[2]=="quantile") {sta <- function(x) {quantile(x,probs=win@rk2,na.rm=TRUE);}}
    if (win@rty[2]=="stdev")    {sta <- function(x) {sqrt(var(x,na.rm=TRUE));}}
    # computing
    if (win@rty[1] == "1") {
        # extracting the reference value for the monitoring variable
        uu <- sta(EE[,win@rmo]);
        # looking for the closest individual
        ## computing the absolute differences
        aa <- abs(EE[,win@rmo]-uu);
        ## finding the individual number
        qq <- which.min(aa);
        # getting this individual
        res <- EE[qq,,drop=FALSE];
    } else {
        for (ii in bc(ncol(EE))) { res[1,ii] <- sta(EE[,ii]);}
        if (win@rty[1]=="0") {
            res <- draw8sele(res,EE,win@rwg,win@rkk,c(0,Inf),c(1,1),nat);
        }
    }
    #
    fait <- TRUE;
}
#
#
if (!fait) {
    rapport(paste("This win@rty[2] (",win@rty[2],") was not detected as compatible with 'draw8predi'",sep=""));
}
# adding the number of candidates
if (names(rebastaba.ena)[1] %in% names(EE)) {
    res[1,names(rebastaba.ena)[1]] <- nrow(EE);
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
simulate8bn <- function(bn,simu=100)
#TITLE simulates a /dn/ from a /bn/
#DESCRIPTION (dn)
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
#{simu} << either (1) a numeric indicating
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
# rebastaba3k("RESET"); # For R convenience when checking
# simulate8bn(rebastaba.bn1,10);
# simulate8bn(rebastaba.bn2,10);
# simulate8bn(rebastaba.bn3,10);
# simulate8bn(rebastaba.bn4,10);
# simulate8bn(rebastaba.bn5,10);
#REFERENCE
#SEE ALSO bn2dn
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 07_06_19
#REVISED 10_08_06
#--------------------------------------------
{
# imprime trigers useful intermediate printing
# it takes value from 0 (no printing) to 4
imprime <- rebastaba.sii;
#imprime <- 1;
if (rebastaba.mck) {check4valid(valid8bn(bn));}
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
	speci <- c("empidata","numcat","score");
	# specific generations
	if (bn@ntype[njbd] == "empidata") {
            if (imprime > 1) { cat("ltype = empidata\n");}
	    # drawing with empidata distribution in a random way
            ## getting the selection criteria
            didi <- bn@nwin[[njbd]];
	    ## getting the parents
	    papa <- parents8win(didi);
	    ## getting the nature of the parents
            pvari <- nv2ion(papa,bn@nom,"N",check=FALSE)@vk;
            nana <- bn@vnat[pvari];
            names(nana) <- papa;
            # getting the names of the variable to draw
            vava <- nanv(bn@nom,njbd);
            # extracting the necessary data.frame
            Eax <- get8daf(bn@ndaf[[njbd]]);
            ax <- draw8empidata(
                   res,                 # already simulated matrix
                   Eax,                 # data frame of the empidata
                   didi,                # win criteria
                   nana,                # nature of the parents
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
            for (ji in bf(lonpar)) {
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
            if (s2na > s1na) {erreur(bn@npara[[njbd]]$scores,"Check the score, they must be numeric",w=rebastaba.mwa);}
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
        if (imprime > 1) { cat("The node",names(ax),"has just been generated\n");}
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
	if (na_ra > rebastaba.nar) {
	     if (imprime > 0) { form3affiche(ax);}
	     erreur(na_ra,"Too much NA values in node",
                           paste(nanv(bn@nom,njbd),collapse="//"),"?",w=rebastaba.mwa);
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
#TITLE simulates data from a bn
#DESCRIPTION (dn)
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
# rebastaba3k("RESET"); # For R convenience when checking
# print(bn2dn(rebastaba.bn1,10),quoi="i",simu=100);
# print(bn2dn(rebastaba.bn2,10),quoi="i",simu=100);
# print(bn2dn(rebastaba.bn3,10),quoi="i",simu=100);
# print(bn2dn(rebastaba.bn4,10),quoi="i",simu=100);
# print(bn2dn(rebastaba.bn5,10),quoi="i",simu=100);
# print(bn2dn(rebastaba.bn6,10),quoi="i",simu=100);
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
if (rebastaba.mck) {
    check4valid(valid8bn(bn));
}
# performing the simulation
sisi <- as.data.frame(simulate8bn(bn,simu));
nbv <- ncol(sisi);
vardn <- nv2ion(0,bn@nom,"N",check=FALSE)@nvn;
# checking (2)
if (rebastaba.mck) {
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
for (jbd in bc(nbv)) {
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
if (rebastaba.mck) { check4valid(valid8dn(dd));}
dd;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
repartition3dn <- function(dn)
#TITLE scans the repartition of the values of dn@df
#DESCRIPTION (dn)
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
# rebastaba3k("RESET"); # For R convenience when checking
# repartition3dn(rebastaba.dn4);
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
if (rebastaba.mck) {
    check4valid(valid8dn(dn));
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
for (vi in bc(nbco)) {
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
for (vi in bc(nbca)) {
    v <- categ[vi];
    uu <- table(dn@df[,v]);
    for (ii in bf(uu)) {
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
for (vi in bc(nbca)) {
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
#TITLE Computes the individual scores for a /dn/
#DESCRIPTION (dn)
# Using the \code{rebastaba.scv} limits computes according
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
# rebastaba3k("RESET"); ## for R checking convenience
# dn2score(rebastaba.dn2);
# dn2score(rebastaba.dn4,4);
# dn2score(rebastaba.dn4,5);
# dn2score(rebastaba.dn4,6);
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
if (rebastaba.mck) {
    check4valid(valid8dn(dn));
    check4tyle(qui,c("numeric","character"),-1,message="within dn2score");
}
# initializing
score <- rep(rebastaba.scv[1],nrow(dn@df));
nbv <- length(dn@df);
# determining the active variables
if (isvide(qui)) {
    qui <- bc(nbv);
} else {
    if (is.numeric(qui)) {qui <- numero(qui,bc(nbv));
    } else { qui <- numero(qui,names(dn@df));}                          
}
# computing
for (j in qui) {
    va <- dn@df[,j];
    # NA values
    nna <- !is.na(va);
    ona <- which(!nna);
    score[ona] <- score[ona] + rebastaba.scv[4];
    # numeric variables
    if(rbsb.snp[dn@nat[j],"numeric"]) {
        po <- (va < dn@pod[[j]][1]) | (va > dn@pod[[j]][2]);
        po <- po & nna;
        score[po] <- score[po] + rebastaba.scv[3];
        re <- (va < dn@red[[j]][1]) | (va > dn@red[[j]][2]);
        re <- re & nna & !po;
        score[re] <- score[re] + rebastaba.scv[2];
        co <- (va < dn@cod[[j]][1]) | (va > dn@cod[[j]][2]);
        co <- co & nna & !po & !re;
        score[co] <- score[co] + rebastaba.scv[1];
    }
    # categoric variables
    if(rbsb.snp[dn@nat[j],"categoric"]) {
        po <- va %in% dn@pod[[j]];
        po <- !po & nna;
        score[po] <- score[po] + rebastaba.scv[3];
        re <- va %in% dn@red[[j]];
        re <- !re & nna & !po;
        score[re] <- score[re] + rebastaba.scv[2];
        co <- va %in% dn@cod[[j]];
        co <- !co & nna & !po & !re;
        score[co] <- score[co] + rebastaba.scv[1];
    }
}
# returning
score;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
dn2ustat <- function(dn,qui=rbsb.num0)
#TITLE Computes the univariate statistics for the variables of a /dn/
#DESCRIPTION (dn)
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
# rebastaba3k("RESET"); ## for R checking convenience
# dn2ustat(rebastaba.dn2);
# dn2ustat(rebastaba.dn4);
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
if (rebastaba.mck) {
    check4valid(valid8dn(dn));
    check4tyle(qui,c("numeric","character"),-1,message="within dn2ustat");
}
# determining the active variables
nbv <- length(dn@df);
if (isvide(qui)) {
    qui <- bc(nbv);
} else {
    if (is.numeric(qui)) {qui <- numero(qui,bc(nbv));
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
            rr <- df2ustat(dn@df[,que,drop=FALSE],nbmin=rebastaba.mnu);
            rere <- matrix(NA,length(rr[[1]]),length(rr));
            dimnames(rere) <- list(names(rr[[1]]),names(rr));
            for (ri in bf(rr)) {
                rere[,ri] <- rr[[ri]];
            }
        }
    }
    #
    if (nn %in% c("cateo","categ")) {
        if (length(que) > 0) {
            rere <- df2ustat(dn@df[,que,drop=FALSE],nbmin=rebastaba.mnu);
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
#TITLE Computes the bivariate statistics for the variables of a /dn/
#DESCRIPTION (dn)
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
# rebastaba3k("RESET"); ## for R checking convenience
# rebastaba.mnb <- 8;
# dn2bstat(rebastaba.dn2);
# dn2bstat(rebastaba.dn4);
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
if (rebastaba.mck) {
    check4valid(valid8dn(dn));
    check4tyle(qui,c("numeric","character"),-1,message="within dn2bstat");
}
# determining the active variables
nbv <- length(dn@df);
if (isvide(qui)) {
    qui <- bc(nbv);
} else {
    if (is.numeric(qui)) {qui <- numero(qui,bc(nbv));
    } else { qui <- numero(qui,names(dn@df));}                          
}
# computing
res <- df2bstat(dn@df[,qui,drop=FALSE],nbmin=rebastaba.mnb);
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
score4dn <- function(dn)
#TITLE Completes a dn with its score
#DESCRIPTION (dn)
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
# rebastaba3k("RESET"); ## for R checking convenience
# score4dn(rebastaba.dn4);
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
if (rebastaba.mck) { check4valid(valid8dn(dn));}
# computing the score
sco <- dn2score(dn);
# adding the variable
res <- dn;
res@df <- cbind(dn@df,sco);
names(res@df) <- c(names(dn@df),rebastaba.scn);
res@description@comm <- c(dn@description@comm,"(completed with the outlying score)");
res@nat <- c(dn@nat,"conti");
res@pod <- c(dn@pod,list(range(sco)));
res@red <- c(dn@red,list(range(sco)));
res@cod <- c(dn@cod,list(quantile(sco,probs=c(0.05,0.95))));
# checking
if (rebastaba.mck) { check4valid(valid8dn(res));}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
draw8sele <- function(ind,E,wgt,kk,di,nb,nat)
#TITLE returns the candidate individuals for
#           an empidata node.
#DESCRIPTION (dn)
# For a given individual (\code{ind}), the distances with all individuals
# in the rows of \code{E} are computed according to some requirement. From them
# a set of candidates of the \code{E} rows is returned.
# This set is comprised of candidates with distances greater or equal
# than di[1] and less or equal than di[2]. Their number is at 
# least equal to nb[1] and no more than nb[2]. When the number 
# of candidates is too small No candidates are returned; when it is
# too big the maximum number is returned with the smallest distances.
# \cr
# Details about the distance formula are given into
# the /rebastaba/ manual.
#DETAILS
# No check are made
#PKEYWORDS empidata
#KEYWORDS
#INPUTS
#{ind} <<Value of the individual to predict (one row
#        data.frame.>>
#{E} <<data.frame with the candidates in its rows.>>
#{wgt} << the weights to be used for the distance computation.>>
#{kk} << power coefficient for the distance computation.>>
#{di} << extreme values of acceptable distances.>>
#{nb} << minimum and maximum numbers of candidates.>>
#{nat} <<nature of the \code{ind} variables (named \code{character}).>>
#[INPUTS]
#VALUE
# a data.frame with the same columns as those of 'E'
# containing the selected subset of candidates sorted
# according to the distance. Except for those with names 
# given by \code{rebastaba.ena[2]} which are filled accordingly to the
# draw (their own distances).
#\cr
#  When empty
# the returned data.frame has got no row.\cr
#EXAMPLE
# rebastaba3k("RESET"); # to comply R checking
# E <- data.frame(A=1:100,c=100:1); # the empirical distribution
# draw8sele(data.frame(A=20),E,structure(1,.Names="A"),1,c(0,1),c(1,2),
#           structure("conti",.Names="A"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_07_05
#REVISED 10_09_02
#--------------------------------------------
{
# no checking
# when there is no candidate
if (nrow(E) == 0) {
    SSS <- numeric(0);
} else {
    # computing the distances for the selection
    DIDI <- draw8dist(ind,E,wgt,kk,nat);
    # when asked filling the distances
    if (names(rebastaba.ena[2]) %in% names(E)) {
      
        E[names(rebastaba.ena[2])] <- DIDI;
    }
    # making the selection from the distances
    SSS <- which((DIDI>=di[1])&(DIDI<=di[2]));
    # making the selection from the numbers
    if (length(SSS) < nb[1]) {
        SSS <- numeric(0);
    } else {
        DIDI <- DIDI[SSS];
        pppp <- order(DIDI);
        SSS <- SSS[pppp];
        if (length(SSS) > nb[2]) {
            SSS <- SSS[1:nb[2]];
        }
    }
}
# 
# logging some information when asked
#
if (rebastaba.esu) {
    sink(rebastaba.efi,append=TRUE);
    form3titre("For the individual:",2);
    print(ind);
    form3titre(paste("The subset of selection (size=",length(SSS),") was:",sep=""),2);
    print(E[SSS,setdiff(names(E),names(rebastaba.ena)[1])]);
    sink();
}
#
# returning
E[SSS,];
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
draw8dist <- function(ind,E,wgt,kk,nat)
#TITLE returns the distances for the selection to be
#           operated with an empidata node.
#DESCRIPTION (dn)
# The description of the algorithm is given into
# /rebastaba/ manual. The selection is monitored by the
# weights and power coefficient usually associated to a /win/.
#DETAILS No systematic check of the argument at this level
#KEYWORDS
#PKEYWORDS empidata
#INPUTS
#{ind} <<Value of the individual to predict (one row
#        data.frame.>>
#{E} <<data.frame with the candidates in its rows.>>
#{wgt} << Named weight(s) to be used for the computation of the distances.
#         When this argument is null, the same distance is returned
#         for all rows of \code{E}.>>
#{kk} << Power coefficient to be used for the computation of the distances.>>
#{nat} <<natures of the \code{ind} variables (\code{named character}).>>
#[INPUTS]
#VALUE
# a numeric vector of length \code{nrow(E)} containing the distance
# of \code{ind} with each row of \code{E}.
#EXAMPLE
# rebastaba3k("RESET"); # to comply R checking
# E <- data.frame(A=1:100,c=100:1); # the empirical distribution
# wgt <- structure(1,.Names="A");
# nat <- structure("conti",.Names="A");
# draw8dist(data.frame(A=20),E,wgt,1,nat);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
# It seems that draw8dist can produce NA values which is odd?
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_07_06
#REVISED 10_09_01
#--------------------------------------------
{
# no systematic checking
#
# degenerate case
if (isvide(wgt)) {
    # all individuals have the same value
    return(rep(1,nrow(E)));
}
# some constants
nbc <- ncol(ind); nbv <- ncol(E);
check4tyle(wgt,"nnumeric",c(1,Inf));
vadi <- nv2nv(names(wgt))$vva;
names(vadi) <- names(wgt);
# some precautionary checks
if (rebastaba.mck) {
    if (length(union(names(E),vadi))!=nbv) {
        erreur(list(names(E),wgt),
               "'E' must comprise all 'wgt' variables");
    }
    if (length(union(names(ind),names(vadi)))!=nbc) {
        erreur(list(names(ind),wgt),
               "'ind' must comprise all 'wgt' variables");
    }
    if (nrow(ind) != 1) {
        erreur(ind,"One individual at once for 'ind'");
    }
    check4tyle(nat,"ncharacter",c(length(wgt),Inf),message="nat in draw8dist");
    if (!(expr3present(nat,rbsb.sna,how="a"))) {
        erreur(nat,"Not accepted nature(s)!");
    }
    if (length(union(names(wgt),names(nat)))!=length(nat)) {
        erreur(list(wgt,nat),"All 'wgt' must be comprised into 'nat' in draw8dist");
    }
}
# Forseeing the degenerate case
if (kk==0) { kk <- 1; klimi <- TRUE; 
} else { klimi <- FALSE; }
#
# the algorithm
#
if (nrow(E)==0) {
    return(numeric(0));
}
#
# getting a matrix with rows associated to 
# those of E and columns associated to ind
# to store the differences
didi <- matrix(NA,nrow(E),length(vadi));
dimnames(didi) <- list(NULL,names(vadi));
# names of numeric and categoric involved variables
twni <- twci <- character(0); 
# the elementary distances
for (inaa in bf(vadi)) {
    # be aware that unknown types are ignored
    rna <- vadi[inaa];        # names for E (reduced)
    cna <- names(vadi)[inaa];  # complete names
    if (rbsb.snp[nat[cna],"numeric"]) {
        didi[,cna] <- wgt[cna]*abs(E[,rna]-ind[1,cna])^kk;
        twni <- c(twni,cna);
    }
    if (rbsb.snp[nat[cna],"categoric"]) {
        didi[,cna] <- wgt[cna]*(as.character(E[,rna]) 
                                    != as.character(ind[1,cna]));
        twci <- c(twci,cna);
    }
}
# the global distances on the parent variables
DINU <- DICA <- 0;
SW <- sum(wgt[c(twni,twci)]);
# for the numeric part
if (length(twni)>0) {
    if (klimi) {
        DINU <- apply(didi[,twni,drop=FALSE],1,max)/SW;
    } else {
        DINU <- (apply(didi[,twni,drop=FALSE],1,sum)/SW)^(1/kk);
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
#
# returning
DIDI;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
dn2csv <- function(dn,fi)
#TITLE creates a csv file containing dn@df
#DESCRIPTION (dn)
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
# rebastaba3k("RESET"); # to comply R checking
# dn2csv(rebastaba.dn2,"toto.txt");
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
if (rebastaba.mck) {
    check4valid(valid8dn(dn));
    check4tyle(fi,"character",1,message="file to be created 'fi' of dn2csv");
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
