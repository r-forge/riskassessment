
###########################################
###########################################
########
#((((((( NEW S4 CLASS arc
########
###########################################
###########################################

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8arc <- function(object)
#TITLE  checks a /arc/
#DESCRIPTION (ba)
#   This function checks /arc/ objects
#DETAILS
# It is the validity method for /arc/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The arc object to be validated.>>
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
#CREATED 09_10_03
#REVISED 10_06_23
#--------------------------------------------
{
    if (class(object)!="arc") {
        erreur(NULL,paste("This object is not of class 'arc' but '",class(object),"'",sep=""));
    }
    res <- character(0);
    # fatal
    if (nrow(object@fle)>0) {
        if (max(object@fle[,1:2]) > object@nbn) {
            res <- c(res,"Inconsistency of arcs with the node number (@nbn)!");
        }
        if (any(object@fle[,1:2] <= 0)) {
            res <- c(res,"A @fle matrix must not have non positive value in its first two columns!");
        }
    }
    if (ncol(object@fle) != 3) { res <- c(res,"A @fle matrix must have 3 columns!");}
    # warning
    if (nrow(object@fle)>0) {
        sco <- object@fle[,1] + object@nbn*object@fle[,2];
        if (length(sco) > length(unique(sco))) {
            res <- c(res,"duplicated arcs in @fle");
        }
    }
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=g4n.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

###########################################
setClass("arc",representation(
    nbn="numeric",       #  number of items
    fle="matrix"),        #  matrix with 3 columns
                         #  each row giving the dep. and arr. items
                         #  plus the decoration to apply to the arc.
               prototype(nbn=0,fle=matrix(NA,0,3)),
               validity=valid8arc
        );


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8arc <- function(x,...)
#TITLE  prints an /arc/
#DESCRIPTION (ba)
# prints the matrix defining arcs
#DETAILS
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<arc object>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.>>
#VALUE
# nothing a printing is issued
#EXAMPLE
# g4n3k("RESET"); # needed only for R checking, to be forgotten
# print(g4n.arc0);
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_10_03
#REVISED 09_10_03
#--------------------------------------------
{
# some checks
che <- valid8arc(x);
if (!identical(che,TRUE)) {
    erreur(che,"This /arc/ is not valid");
}
# printing
cat("There are",x@nbn,"nodes and",nrow(x@fle),"arcs defined by\n");
print(x@fle);
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "arc"), print8arc);



###########################################
###########################################
########
#((((((( NEW S4 CLASS pam
########
###########################################
###########################################


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8pam <- function(object)
#TITLE  checks a /pam/
#DESCRIPTION (ba)
#   This function checks /pam/ objects
#DETAILS
# It is the validity method for /pam/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The pam object to be validated.>>
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
#CREATED 09_10_03
#REVISED 10_06_23
#--------------------------------------------
{
    if (class(object)!="pam") {
        erreur(NULL,paste("This object is not of class 'pam' but '",class(object),"'",sep=""));
    }
    res <- character(0);
    nbn <- nrow(object@rlt);
    if (ncol(object@rlt) != nbn) { res <- c(res,dim(object@rlt),"A pam matrix must be squared!");}
    if (sum((object@rlt==0)|(object@rlt==1)) != nbn*nbn) {
        res <- c(res,"A pam matrix must be binary (0/1 values)!");
    }
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=g4n.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



###########################################
setClass("pam",representation(
    rlt="matrix"),       #  square matrix with as many rows as items
                         #  parents in rows, children in columns
                         #  1 if a ReLaTion, 0 otherwise.
               prototype(rlt=matrix(NA,0,0)),
               validity=valid8pam
        );


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8pam <- function(x,...)
#TITLE  prints a /pam/
#DESCRIPTION (ba)
# Nothing more than printing a /pam/ matrix (after checking it).
#DETAILS
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<pam object>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.>>
#VALUE
# nothing a printing is issued
#EXAMPLE
# g4n3k("RESET"); # needed only for R checking, to be forgotten
# print(g4n.pam0);
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_10_03
#REVISED 09_10_03
#--------------------------------------------
{
# some checks
che <- valid8pam(x);
if (!identical(che,TRUE)) {
    erreur(che,"This /pam/ is not valid");
}
# printing
cat("This /pam/ matrix is associated to",nrow(x@rlt),"nodes/variates\n");
print(x@rlt);
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "pam"), print8pam);



###########################################
###########################################
########
#((((((( NEW S4 CLASS pgr
########
###########################################
###########################################


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8pgr <- function(object)
#TITLE  checks a /pgr/
#DESCRIPTION (ba)
#   This function checks /pgr/ objects
#DETAILS
# It is the validity method for /pgr/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The pgr object to be validated.>>
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
#CREATED 09_10_04
#REVISED 10_06_23
#--------------------------------------------
{
    if (class(object)!="pgr") {
        erreur(NULL,paste("This object is not of class 'pgr' but '",class(object),"'",sep=""));
    }
    res <- character(0);
    #
    if (length(object@unitscale) != 1) { res <- c(res,"Bad unitscale");}
    if (length(object@cexscale) != 1) { res <- c(res,"Bad cexscale");}
    if (length(object@arrowlength) != 1) { res <- c(res,"Bad arrowlength");}
    if (length(object@sscale) != 1) { res <- c(res,"Bad sscale");}
    if (length(object@diar) != 1) { res <- c(res,"Bad diar");}
    #
    if (length(res)== 0) { res <- TRUE;
    } else {
        res <- c(res,"Incorrect definition of object",class(object));
        erreur(res,w=g4n.mwa);
    }
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



###########################################
setClass("pgr",representation(
    unitscale="numeric",    #
    padding="numeric",      # to preserve some margins around
                            # extreme values (1 = no margin, 1.1 = 10% of margins)
    cexscale="numeric",     # size for the characters on the plot
    arrowlength="numeric",  # size of the arrow heads
    sscale="numeric",       #
    kzoom="numeric",        # the changing in zoom for magnifying
    diar="numeric"),        # distance of arrow ends to nodes
               prototype(
    unitscale=20,
    padding=1.1,
    cexscale=1,
    arrowlength=0.12,
    sscale=1,
    kzoom=1.2,
    diar=0.05),
               validity=valid8pgr
);


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8pgr <- function(x,...,what="v",type=0)
#TITLE  prints the node-variable names
#DESCRIPTION (ba)
# prints the node-variable names of x (a 'nom' object)
#DETAILS
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<nom object>>
#[INPUTS]
#{\dots} << Further arguments to be passed to the print function.>>
#{what} << what to print ('v'=node-variable names;
#         'n' only node names)
#{type} << type of printing (0) a node a line;
#              (1) everything on the same line.>>
#VALUE
# nothing a printing is issued
#EXAMPLE
# g4n3k("RESET"); # needed only for R checking, to be forgotten
# print(new("pgr",
#    unitscale=20,
#    padding=1.1,
#    cexscale=1,
#    arrowlength=0.12,
#    sscale=1,
#    kzoom=1.2,
#    diar=0.05));
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_04_08
#REVISED 09_04_08
#--------------------------------------------
{
# some checks
che <- valid8pgr(x);
if (!identical(che,TRUE)) {
    erreur(che,"This /pgr/ is not valid");
}
# printing
cat("Printing a /pgr/ object:\n");
cat("   unitscale=",x@unitscale,"\n");
cat("   padding=",x@padding,"\n");
cat("   cexscale=",x@cexscale,"\n");
cat("   arrowlength=",x@arrowlength,"\n");
cat("   sscale=",x@sscale,"\n");
cat("   kzoom=",x@kzoom,"\n");
cat("   diar=",x@diar,"\n");
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "pgr"), print8pgr);



###########################################
###########################################
########
#((((((( NEW S4 CLASS pos
########
###########################################
###########################################


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8pos <- function(object)
#TITLE  checks a /pos/
#DESCRIPTION (ba)
#   This function checks /pos/ objects
#DETAILS
# It is the validity method for /pos/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The pos object to be validated.>>
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
#CREATED 09_10_04
#REVISED 10_06_23
#--------------------------------------------
{
    if (class(object)!="pos") {
        erreur(NULL,paste("This object is not of class 'pos' but '",class(object),"'",sep=""));
    }
    res <- character(0);
    #
    nbn <- nrow(object@posi);
    if (nbn > 0) {
        if (ncol(object@posi) != 4) {
            res <- c(res,paste(dim(object@posi),sep="/","@posi must have FOUR columns!"));
        }
        if (!is.numeric(object@view) | (length(object@view) != 2)) {
            res <- c(res,"@view must be numeric(2)!");
        }
        if (!is.numeric(object@zoom) | (length(object@zoom) != 4)) {
            res <- c(res,"@zoom must be numeric(4)!");
        }
        if (object@zoom[4] <= 0) {
            res <- c(res,"Magnifying coefficient of @zomm cannot be negative or nought!");
        }
    }
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=g4n.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



###########################################
setClass("pos",representation(
    posi="matrix",        # 3D coordinates of each node for plotting + 
                          # decoration. That is a (nbn,4) matrix.
    view="numeric",       # the two angles for the projection into R^2
                          # (0,0) means projection into (X,Y)
    zoom="numeric"),      # four values (center point after normalization on [-1,+1] instead
                          # of [min,MAX] and the the magnifying factor). (0,0,0,1) implies
                          # the standard fitted view.
               prototype(posi=matrix(NA,0,4),
                                    view=c(0,0),
                                    zoom=c(0,0,0,1)),
               validity=valid8pos
);


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8pos <- function(x,...)
#TITLE  prints a /pos/
#DESCRIPTION (ba)
# just defining the print.pos function
#DETAILS
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<the pos object to be printed>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.>>
#VALUE
# nothing a printing is issued
#EXAMPLE
# g4n3k("RESET"); # needed only for R checking, to be forgotten
# uu <- new("pos",posi=matrix(round(10*cos(1:20)),5));
# print8pos(uu);
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_10_04
#REVISED 09_10_04
#--------------------------------------------
{
# some checks
che <- valid8pos(x);
if (!identical(che,TRUE)) {
    erreur(che,"This /pos/ is not valid");
}
# printing
cat("Printing a /pos/ object:\n");
cat("   view=",x@view,"\n");
cat("   zoom=",x@zoom,"\n");
cat("   posi= (a matrix with 3D plus decoration code) \n");
print(x@posi);
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "pos"), print8pos);



###########################################
###########################################
########
#((((((( NEW S4 CLASS gn
########
###########################################
###########################################

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8gn <- function(object)
#TITLE checks a /gn/
#DESCRIPTION
#   This function checks /gn/ objects
#DETAILS
# It is the validity method for /gn/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The gn object to be validated.>>
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
#CREATED 09_10_14
#REVISED 10_06_23
#--------------------------------------------
{
    res <- character(0);
    if (class(object)!="gn") {
        erreur(NULL,paste("This object is not of class 'gn' but '",class(object),"'",sep=""));
    }
    #
    if (length(setdiff(slotNames("gn"),slotNames(object)))) {
        res <- c(res,paste("Not all slots (",slotNames("gn"),") are present",sep=""));
    }
    #
    rr <- valid8des(object@description);
    if (!identical(TRUE,rr)) { res <- c(res,rr);}
    #
    rr <- valid8nom(object@nom);
    if (!identical(TRUE,rr)) { res <- c(res,rr);}
    #
    if (!(object@item %in% c("n","v"))) {
        erreur(object@item,"'gn@item' must be 'n' or 'v'.");
    }
    #
    rr <- valid8arc(object@arc);
    if (!identical(TRUE,rr)) { res <- c(res,rr);}
    nbn <- nbnv(object,object@item);
    if (nbn != object@arc@nbn) {
        erreur(list(nbnv(object,object@item),object@arc@nbn),"Bad number of nodes in the @arc!");
    }
    # checking the positions
    rr <- valid8pos(object@pos);
    if (!identical(TRUE,rr)) { res <- c(res,rr);}
    if (nbn != nrow(object@pos@posi)) {
        erreur(list(nbnv(object),object@pos@posi),"Bad number of nodes in the @pos!");
    }
    # checking the graphical parameters
    rr <- valid8pgr(object@pgr);
    if (!identical(TRUE,rr)) { res <- c(res,rr);}
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=g4n.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



###########################################
setClass("gn",representation(
    description="des",    # description of the gn
    nom="nom",           # node names
    item="character",    # "n" when the graph is defined at the node level
                         # "v" when the graph is defined at the variable level
    pos="pos",           # positionning of the items for the representation and decoration
    arc="arc",           # definition of the arcs and decoration
                         # when @item='n' by the ion@nk
                         # when @item='v' by the ion@vk
    pgr="pgr")           # parameters for the graphical representation
        );
###########################################

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8gn <- function(x,...,quoi="snrt")
#TITLE prints an object gn
#DESCRIPTION
# Prints a gn object with more or less details
# according to the \code{quoi} argument.
#DETAILS
#KEYWORDS print
#PKEYWORDS gn
#INPUTS
#{x} <<the gn object to print>
#{\dots} <<Further arguments to be passed to the
# standard print function.>>
#[INPUTS]
#{quoi} <<(="snrt") character string giving the component to print.\cr
#          "t": the title,\cr
#          "s": the size,\cr
#          "d": the description,\cr
#          "n": the node names,\cr
#          "r": the arcs,\cr
#          "p": the positions and the view,\cr
#          "a": everything.>>
#VALUE
#  returns nothing but a printing is performed
#EXAMPLE
# 
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#??? allows to underline the direction of the arc with repect to the order nodes.
#??? double arrow, symmetrical links.
#??? put a star on self-links.
#AUTHOR J.-B. Denis
#CREATED 07_05_23
#REVISED 09_10_20
#--------------------------------------------
{
# checking
if (g4n.mck) {check4valid(valid8gn(x));}
if (expr3present("a",quoi)) { quoi <- "tdsnrp";}
# computing the dimensions
if (x@item=="n") {
    nbit <- nbnv(x,"n");
    item <- nv2ion(0,x@nom,"n")@nn;
    quoite <- "nodes";
} else {
    nbit <- nbnv(x,"v");
    item <- nv2ion(0,x@nom,"v")@nvn;
    quoite <- "variables";
}
nbar <- nrow(x@arc@fle);
ti <- paste("There are",nbit,quoite,"and",nbar,"arcs");
sou <- paste(form3repeat("-",nchar(ti)),"\n");
# the title
if (expr3present("t",quoi)) {
  cat(sou);
  cat("x object of name \"",x@description@name,"\"\n",sep="");
}
# the size
if (expr3present("s",quoi)) {
  cat(sou);
  cat(ti,"\n");
}
# the description
if (expr3present("d",quoi)) {
  print(x@description,rbsb.mep);
}
# the node names
if (expr3present("n",quoi)) {
  cat(sou);
  form3liste(item,OPA="",CPA="",sep="; ",imp=TRUE)
}
# the arcs
if ((expr3present("r",quoi)) & (nbar > 0)) {
  cat(sou);
  cat("  Arcs are:\n");
  for (i in 1:nbar) {
    cat(form3justifie(item[x@arc@fle[i,1]],12,3,FALSE),
        " ---> ",
        form3justifie(item[x@arc@fle[i,2]],12,1,FALSE),"\n");
  }
}
# the positions
if (expr3present("p",quoi)) {
  cat(sou);
  cat("  Positions are:\n");
  pos <- x@pos@posi[,1:3];
  dimnames(pos) <- list(item,c("X","Y","Z"));
  print(round(pos,rbsb.mnd));
  for (i in 1:nchar(ti)) {cat("-");}; cat("\n");
  cat("  Viewpoint is:\n");
  cat("Theta = ",x@pos@view[1]," (",x@pos@view[1]/pi*180,"deg.)\n")
  cat(" Phi  = ",x@pos@view[2]," (",x@pos@view[2]/pi*180,"deg.)\n")
  cat(sou);
  cat("  Current projection is:\n");
  posi <- pos2pos(x@pos)@posi[,1:2];
  dimnames(posi) <- list(item,c("XX","YY"));
  print(round(posi,rbsb.mnd));
}
cat(sou);
invisible();    
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
plot8gn <- function(x,y="useless",...,main=NULL,frame=FALSE)
#TITLE plots an object gn
#DESCRIPTION
# Standard plot of an object gn.\cr
# Following some coding found in slots x@pos and x@arc, decorations
# are added to the arcs and to the nodes. More precisely:\cr
# Each row of the matrix x@pos@posi is associated to a node. 
# The first three columns give the (X,Y,Z) coordinates (in /rebastaba/ graphs
# are placed in R to the 3 and the 4th column  indicates if 
# the node must be surrounded by nothing (0), a square (1), a circle (2).\cr
# x@arc@fle is a matrix with rows associated to arcs. The
# first column is the iin of the departure node and the second
# the iin of the arrival node. The 3rd column indicates the style of the arrow
# by three combined parameters com = 100*code+10*lty+lwd.
# code is the standard parameter of basic R arrows function, lty and
# lwd parameters are standard ones from par... The standard value of 
# com is 211.
#DETAILS
# Possible decorations for nodes are: square (1), circle (2).\cr
# x@arc@fle[,3] determines the style of the arcs: cdu\cr
#    c gives the kind of arrows (see R function arrow): {0,1,2,3}\cr
#    d gives the type of lines: {0,1,...,6}\cr
#    u gives the width of the line: [0.5,5]
#KEYWORDS
#PKEYWORDS plot gn
#INPUTS
#{x} <<the gn to plot>>
#{y} <<added useless argument to comply R requirement.>>
#{\dots} <<Further arguments to be passed to the plot function.>>
#[INPUTS]
#{main} <<(=NULL) main title for the graph, when NULL x@description@name
#         is taken as title.>>
#{frame} <<(=FALSE) to indicate if a frame has to be drawn>>
#VALUE
#  returns nothing but a plot is drawn
#EXAMPLE
# g4n3k("RESET"); # needed only for R checking, to be forgotten
# plot8gn(g4n.gn1);
#REFERENCE
#SEE ALSO
#CALLING {newposi} {plot8cgn} 
#COMMENT
#??? arc starting and arriving to the same node are not represented. Bidirectional arcs are.
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_05_23
#REVISED 09_10_20
#--------------------------------------------
{
if (g4n.mck) {check4valid(valid8gn(x));}
if (is.null(main)) { main <- x@description@name;}
# getting projection values
xx <- pos2pos(x@pos);
# computing the ranges of the representation
ra <- range8pos(xx,x@pgr@padding);
# preparing the canvas
if (frame) {
    plot(0,0,xlim=ra$x,ylim=ra$y,type="n",
         main=main,
         xaxt="n",yaxt="n",xlab="",ylab="",...);
} else {
    plot(0,0,xlim=ra$x,ylim=ra$y,type="n",
         main=main,
         axes=FALSE,xlab="",ylab="",...);
}
# drawing nodes and arcs
plot8cgn(x,xx@posi[,1:2]);
invisible();    
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "gn"), print8gn);
setMethod("plot", signature(x = "gn"),  plot8gn);
