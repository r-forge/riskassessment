
rs003k("reset");
rsba3k("reset");

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rsgn3k <- function(whi)
#TITLE (gn) assigns the constants for the rsgn layer
#DESCRIPTION
# defines or returns the constants used within /rbsbgn/. 
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
#           to return the names or the current values. The possible
#           values are \code{RESET}, \code{reset}, \code{names}, \code{definitions} or \code{values}.>>
# >>
#[INPUTS]
#VALUE
# When \code{whi=="RESET"} nothing (but the assignments are
# performed for all layers \code{rs00}, \code{rsba} and \code{rsgn}).
# When \code{whi=="reset"} nothing (but the assignments of 
# the layer \code{rsgn} are performed).
# When \code{whi=="names"} the names as a character vector.
# When \code{whi=="definitions"} the definitions as a named character vector.
# When \code{whi=="values"} the values through a named list.
#EXAMPLE
# rsgn3k("RESET"); # needed only for R checking, to be forgotten
# print(rbsb.msi);
## to get the short labels
# rsgn3k("names");
## to obtain the current values
# rsgn3k("values");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_10_13
#REVISED 09_11_05
#--------------------------------------------
{
# checking
if (!expr3present(whi,c("RESET","reset","names","definitions","values"))) {
    print.default(whi);
    stop("rsgn3k does not accept this argument");
}
#
if (whi=="RESET") { rsba3k("RESET");}
#
# definition of the different constants
sc <- character(0);
sc["gn0"] <- "null /gn/"; 
sc["gn1"] <- "Example 1 of /gn/"; 
sc["gn2"] <- "Example 2 of /gn/"; 
sc["gn3"] <- "Example 3 of /gn/"; 
sc["gn4"] <- "Example 4 of /gn/"; 
sc["gn5"] <- "Example 4 of /gn/"; 
sc["gg1"] <- "Demo 1 of /gn/"; 
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
    assign("rbsb.gn0",new("gn",
                 description=char2des("Null /gn/"),
                 nom=rbsb.nom0, 
                 item="n",
                 pos=rbsb.pos,
                 arc=rbsb.arc0,
                 pgr=rbsb.pgr),pos=.GlobalEnv);
    assign("rbsb.gn1",new("gn",
                 description=char2des("Example 1 of /gn/"),
                 nom=rbsb.nom1, 
                 item="n",
                 pos=new("pos",posi=matrix(c(1,2,1,1,2,2,0,0,0,2,1,1),3)),
                 arc=new("arc",nbn=3,fle=matrix(c(1,2,211),1)),
                 pgr=rbsb.pgr),pos=.GlobalEnv);
    assign("rbsb.gn2",new("gn",
                 description=char2des("Example 2 of /gn/"),
                 nom=rbsb.nom2, 
                 item="v",
                 pos=new("pos",posi=matrix(c(1,2,2,2,1,1,2,3,rep(0,4),rep(2,4)),4)),
                 arc=new("arc",nbn=4,fle=matrix(c(1,2,211),1)),
                 pgr=rbsb.pgr),pos=.GlobalEnv);
    assign("rbsb.gn3",new("gn",
                 description=char2des("Example 3 of /gn/"),
                 nom=rbsb.nom3, 
                 item="v",
                 pos=new("pos",posi=matrix(c(1,2,1,2,1,2,
                                             1,1,2,2,3,3,
                                             rep(0,6),rep(2,6)),6)),
                 arc=new("arc",nbn=6,fle=matrix(c(1,2,3,4,5,
                                                  2,3,4,5,6,
                                                  rep(211,5)),5)),
                 pgr=rbsb.pgr),pos=.GlobalEnv);
    assign("rbsb.gn4",new("gn",
                 description=char2des("Example 4 of /gn/"),
                 nom=rbsb.nom4, 
                 item="n",
                 pos=new("pos",posi=matrix(c(1,2,1,2,
                                             rep(0,2),rep(2,2)),2)),
                 arc=new("arc",nbn=2,fle=matrix(c(1,2,
                                                  rep(211,1)),1)),
                 pgr=rbsb.pgr),pos=.GlobalEnv);
    assign("rbsb.gn5",new("gn",
                 description=char2des("Example 5 of /gn/"),
                 nom=rbsb.nom5, 
                 item="n",
                 pos=new("pos",posi=matrix(c(1,2,1,2,
                                             rep(0,2),rep(2,2)),2)),
                 arc=new("arc",nbn=2,fle=matrix(c(1,2,
                                                  rep(211,1)),1)),
                 pgr=rbsb.pgr),pos=.GlobalEnv);
    assign("rbsb.gg1",new("gn",
                 description=char2des("Demo 1 of /gn/"),
                 nom=new("nom",x=char2list(letters[1:10],nam=LETTERS[1:10])), 
                 item="n",
                 pos=new("pos",posi=matrix(c(1,1,1,2,2,2,3,3,3,3,
                                             1,2,3,1,2,3,1,2,3,4,
                                             rep(0,10),rep(2,10)),10)),
                 arc=new("arc",nbn=10,fle=matrix(c(1,1,1,2,8,10,10,5,
                                                   2,5,4,3,6, 6, 9,6,
                                                   rep(211,8)),8)),
                 pgr=rbsb.pgr),pos=.GlobalEnv);
}
#
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

###########################################
###########################################
########
#((((((( NEW S4 CLASS gn
########
###########################################
###########################################

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8gn <- function(object)
#TITLE (gn) checks a /gn/
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
#REVISED 09_10_16
#--------------------------------------------
{
    res <- character(0);
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
    } else { erreur(res,w=rbsb.mwa);}
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
#TITLE (gn) prints an object gn
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
if (rbsb.mck) {valid8gn(x);}
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
sou <- paste(form3repete("-",nchar(ti)),"\n");
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
#TITLE (gn) plots an object gn
#DESCRIPTION
# Standard plot of an object gn.\cr
# Following some coding found in slots x@pos and x@arc, decorations
# are added to the arcs and to the nodes. More precisely:\cr
# Each row of the matrix x@pos@posi is associated to a node. 
# The first three columns give the (X,Y,Z) coordinates (in /rbsb/ graphs
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
# rsgn3k("RESET"); # needed only for R checking, to be forgotten
# plot8gn(rbsb.gn1);
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
if (rbsb.mck) {valid8gn(x);}
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

setMethod("plot", signature(x = "gn"),  plot8gn);
setMethod("print",signature(x = "gn"), print8gn);

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
parcours8gn <- function(gn,blocked=character(0),imon=TRUE)
#TITLE (gn) find the ascendances until blocking 
#DESCRIPTION
# exploring the parentship, returns a matrix with
# rows associated to nodes giving their ancestors
# until (if possible) the first blocking node
#DETAILS
# In a first step the matrix of chidrenhood is
# calculated which can be expansive when the number
# of nodes is high.
#KEYWORDS
#PKEYWORDS gn genealogy
#INPUTS
#{gn} <<The gn object to be dealt>>
#[INPUTS]
#{blocked} <<(= character(0)) those nodes
#              which are reference node for
#              blocking>>
#{imon} <<(=TRUE) must blocked node be included?>>
#VALUE
# A square matrix. Its rows corresponds to every node.
# Its columns are associated to possible
# ascendants: 0 if not, 1 if it is. The node itself is not included 
# that is the diagonal elements are zero. The row and column order are
# those of iin; the dimnames of this matrix are given by the node names.
#EXAMPLE
# rsgn3k("RESET"); # needed only for R checking, to be forgotten
# parcours8gn(rbsb.gg1);
#REFERENCE
#SEE ALSO genea
#CALLING
#COMMENT
#FUTURE
# It could be more logical (and efficient when the number
# of nodes is important) that the value was a list of vectors
# and not a matrix which can be very sparse.
#AUTHOR J.-B. Denis
#CREATED 07_09_18
#REVISED 09_06_17
#--------------------------------------------
{
if (rbsb.mck) {valid8gn(gn);}
# initialization
nbno <- nbnv(gn,gn@item); par <- res <- matrix(0,nbno,nbno);
par <- t(arc2pam(gn@arc)@rlt);
if (gn@item=="n") {
    moni <- nv2ion(blocked,gn@nom,"n",check=FALSE)@nk;
    nnom <- nanv(gn@nom,"n");
} else {
    moni <- nv2ion(blocked,gn@nom,"N",check=FALSE)@vk;
    nnom <- nanv(gn@nom,"v");
}
# loop for every node
for (jbd in 1:nbno) { if (!is.element(jbd,moni)) {
    qui <- numeric(0);
    ppp <- which(par[,jbd]==1);
    qui <- setdiff(union(qui,ppp),moni);
    ajout <- 1;
    if (length(qui) > 0) {while (length(ajout) > 0) {
        ajout <- numeric(0);
        for (jd in 1:length(qui)) {

            pp <- which(par[,qui[jd]]==1);
            ajout <- setdiff(union(ajout,pp),union(qui,moni));
        }
        qui <- union(qui,ajout);
    }}
    res[jbd,qui] <- 1;
}}
# including blocked nodes
if (imon) { for (jbd in 1:nbno) { if (!is.element(jbd,moni)) {
    bord <- c(jbd,which(res[jbd,]==1));
    if (length(bord) == 0) { erreur("from parcours8gn","Must comprise at least one node!");}
    mm <- numeric(0);
    for (jd in bord) {
        mm <- union(mm,intersect(which(par[,jd]==1),moni));
    }
    res[jbd,mm] <- 1;
}}}
# returning
dimnames(res) <- list(nnom,nnom);
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rmnd4gn <- function(gn,nk)
#TITLE (gn) removes a series of nodes from a gn object
#DESCRIPTION
# Removing the nodes and arcs associated to them,
# adjusting the slot arc consequently. Awaiting
# for the generalization of item in /gn/s, only 
# node /gn/, i.e. gn@item='n' are accepted here.
#DETAILS
# the initial @nom slot is reduced.
#KEYWORDS
#PKEYWORDS gn
#INPUTS
#{gn}<</gn/ object to be modified.>>
#{nk} <<vector of number nodes to remove. Must be
#        given in 'nk' form (see nv2ion).>>
#[INPUTS]
#VALUE
# The reduced object gn
#EXAMPLE
# rsgn3k("RESET"); # needed only for R checking, to be forgotten
# print(rmnd4gn(rbsb.gn1,1));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#????
# In fact there is another (also very interesting) way to remove a node; it 
# consists not to remove node and associated arcs, but to transform the arcs
# of the parents and children nodes to keep the transmission (more precisely,
# indirect links can become direct ones). Must also be implemented.
#AUTHOR J.-B. Denis
#CREATED 07_09_06
#REVISED 09_10_20
#--------------------------------------------
{
# checking
if (rbsb.mck) {valid8gn(gn);}
# some preparation
if (is.character(nk)) {
    nk <- nv2ion(nk,gn@nom,check=FALSE)@nk;
}
items <- gn2item(gn,check=FALSE);
nnrr <- nrow(gn@arc@fle);
if (nnrr > 0) {
    aa <- matrix(items[gn@arc@fle[,1:2]],nnrr);
} else { aa <- matrix(NA,0,0);}
nk <- sort(unique(nk));
mk <- setdiff(sjl(items),nk);
n2r <- items[nk];
n2k <- setdiff(items,n2r);
# removing the nodes in the slot nom
for (ii in sjl(n2r)) {
    gn@nom <- rmnd4nom(gn@nom,n2r[ii]);
}
# removing the corresponding positions
gn@pos@posi <- gn@pos@posi[mk,,drop=FALSE];
# removing the corresponding arcs under variable names
# to avoid a difficult permutation to apply
r2r <- numeric(0);
for (ii in sj(nrow(aa))) {
    if (!(aa[ii,1] %in% n2k)) { r2r <- c(r2r,ii);}
    if (!(aa[ii,2] %in% n2k)) { r2r <- c(r2r,ii);}
}
r2r <- unique(r2r);
r2k <- setdiff(sj(nrow(aa)),r2r);
# here the numbers must be renumbered
nou <- sjl(items);
for (ii in sjl(items)) {
    nou[ii] <- ii - sum(nk < ii);
}
gn@arc@fle <- gn@arc@fle[r2k,,drop=FALSE];
gn@arc@fle[,1:2] <- nou[gn@arc@fle[,1:2]];
gn@arc@nbn <- length(n2k);
# returning
gn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rmar4gn <- function(gn,ion)
#TITLE (gn) removes a series of arcs from a gn object
#DESCRIPTION
# Removing the arcs associated to a subset of nodes,
# but keeping the nodes (contrary to rmnd4gn)
#DETAILS
#KEYWORDS
#PKEYWORDS gn
#INPUTS
#{gn}<<object gn defining the pruned graph>>
#{ion} <<vector of nodes the arc of which must be 
#        be removed, either the names or the
#        internal identification.>>
#[INPUTS]
#VALUE
# The reduced object gn
#EXAMPLE
# rsgn3k("RESET"); # needed only for R checking, to be forgotten
# uu <- rbsb.gn1;
# print(uu@arc);
# print(rmar4gn(uu,"A")@arc);
#REFERENCE
#SEE ALSO rmnd4gn
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_17
#REVISED 09_06_17
#--------------------------------------------
{
if (rbsb.mck) {valid8gn(gn);}
# translating into numbers
if (gn@item=="n") {
    qui <- nv2ion(ion,gn@nom,"n",check=FALSE)@nk;
} else {
    qui <- nv2ion(ion,gn@nom,"N",check=FALSE)@vk;
}
# detecting the arcs to remove
iuq <- numeric(0);
# just removing the related arcs
if (length(qui > 0)) { for (ar in sj(nrow(gn@arc@fle))) {
    afr <- gn@arc@fle[ar,1]; ato <- gn@arc@fle[ar,2];
    if (is.element(afr,qui) | is.element(ato,qui)) {
        iuq <- c(iuq,ar);
    }
}}
if (length(iuq) > 0) { gn@arc@fle <- gn@arc@fle[-iuq,,drop=FALSE];}
# returning
gn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
gn2item <- function(gn,check=FALSE)
#TITLE (gn) returns the item names of a /gn/ object
#DESCRIPTION
# (see slot @item of /gn/.)
#DETAILS
#KEYWORDS
#PKEYWORDS gn
#INPUTS
#{gn} <<The gn object.>>
#[INPUTS]
#{check} <<Must checking be undertaken (conditionally
#          to \code{rbsb.mck}).>>
#VALUE
# a character providing the item names.
#EXAMPLE
# rsgn3k("RESET"); # needed only for R checking, to be forgotten
# gn2item(rbsb.gn1);
# gn2item(rbsb.gn2);
# gn2item(rbsb.gg1);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
# This function has been produced for future
# evolutions of the /gn/ items. Not only 
# nodes or variables as presently.
#AUTHOR J.-B. Denis
#CREATED 09_05_06
#REVISED 09_05_06
#--------------------------------------------
{
# checking
if (check) { if (rbsb.mck) {valid8gn(gn);}}
# creating
res <- nanv(gn,gn@item);
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
plot8cgn <- function(gn,xy)
#TITLE (gn) draws nodes and arcs of a gn with already computed xy values
#DESCRIPTION
# Internal drawing of plot.gn, isolated because it
# can be used for multiple plots. The plot is supposed
# to be already open and the coordinates of nodes already
# prepared. See plot8gn for the details about decorations.
#DETAILS
# The reason of xy besides the @pos of the gn object is to
# give the possibility of different projections with the 
# same positions.
#KEYWORDS
#PKEYWORDS plot gn
#INPUTS
#{gn} <<The gn to plot>>
#{xy} <<Matrix of (x,y) (nbn x 2) coordinates for the nodes.>>
#[INPUTS]
#VALUE
#  returns nothing but something is drawn on the open plot
#EXAMPLE
#REFERENCE
#SEE ALSO plot.gn
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_06_27
#REVISED 09_10_20
#--------------------------------------------
{
# checking
if (rbsb.mck) {valid8gn(gn);}
if (!all(dim(xy) == c(nbnv(gn,gn@item),2))) {
    erreur(NULL,"xy not consistent in dimensions!");
}
# getting the node/variable names
nom <- gn2item(gn);
# ancillary function(s)
raccou <- function(x0,y0,x1,y1,di=gn@pgr@diar) {
  c(geom3pointi(x0,y0,x1,y1,di),
    geom3pointi(x1,y1,x0,y0,di));
}
nbno <- nbnv(gn,gn@item);
if (nbno < 2) {
    erreur(nbno,"For this number of nodes no graph is issued",w=rbsb.mwa);
    return(invisible);
}
nbar <- nrow(gn@arc@fle);
# drawing the nodes
for (i in sj(nbno)) {
    xx <- xy[i,1]; yy <- xy[i,2];
    la <- nom[i]; de <- gn@pos@posi[i,4];
    text(xx,yy,la,cex=gn@pgr@cexscale);
    if (!is.na(de)) {
        tail <- 6*gn@pgr@cexscale;
        if (de == 1) { points(xx,yy,pch=22,cex=tail);}
        if (de == 2) { points(xx,yy,pch=21,cex=tail);}
    }
}
# drawing the arcs
for (i in sj(nbar)) {
    n1 <- gn@arc@fle[i,1]; n2 <- gn@arc@fle[i,2];
    ax <- raccou(xy[n1,1],xy[n1,2],
                 xy[n2,1],xy[n2,2]);
    # style of the arcs
    com <- gn@arc@fle[i,3];
    # getting the heads, line type and width of the arrows
    code <- com %/% 100; com <- com - 100*code;
    code <- round(code); code <- min(max(code,0),3);
    lty <- com %/% 10; com <- com - 10*lty;
    lty <- round(lty); lty <- min(max(lty,0),6);
    lwd <- com; lwd <- min(max(lwd,0.5),5);
    arrows(ax[1],ax[2],ax[3],ax[4],
           length=gn@pgr@arrowlength,
           lwd=lwd,lty=lty,code=code);
}
invisible();    
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
cycles4gn <- function(gn)
#TITLE (gn) reduces the graph to the portion with cycles
#DESCRIPTION
# Just removing from the graph those nodes (and
# associated arcs) which cannot belong to a cycle
# because they are extreme (either without parent or
# without children).\cr
#DETAILS
#KEYWORDS
#PKEYWORDS gn genealogy
#INPUTS
#{gn}<<object gn defining the graph>>
#[INPUTS]
#VALUE
# The reduced object gn
#EXAMPLE
# rsgn3k("RESET"); # needed only for R checking, to be forgotten
# cycles4gn(rbsb.gg1); # returns the null /gn/
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
# The graph associated to a BN must reduce to nothing
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_06
#REVISED 07_09_07
#--------------------------------------------
{
if (rbsb.mck) {valid8gn(gn);}
rn <- 1;
# loop for removing the nodes
while ((rn > 0) & (nbnv(gn,gn@item) > 0)) {
    rn <- 0;
    nar <- nrow(gn@arc@fle);
    nbd <- nbnv(gn,gn@item);
    arc <- matrix(0,nbd,2);
    if (nar > 0) { for (ar in 1:nar) {
        arc[gn@arc@fle[ar,1],1] <- 1;
        arc[gn@arc@fle[ar,2],2] <- 1;
    }}
    crit <- arc[,1]+arc[,2];
    retire <- (1:nbd)[crit<2];
    rn <- length(retire);
    if (rn > 0) { gn <- rmnd4gn(gn,retire);}
}
# null case
if (nbnv(gn,gn@item)==0) { gn <- rbsb.gn0;}
# returning
gn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
path4gn <- function(gn,n1,n2,quel=1,elono=TRUE)
#TITLE (gn) reduces the graph to the portion in between two node sets
#DESCRIPTION
# Just removing from the graph those arcs (and nodes) which
# does not belong to a path going from a node to another one.\cr
# As graph comprising cycles can be introduced, both directions
# can be considered.
#DETAILS
# To get all descendants of n1, it suffices to put n2 as the complete
# set of nodes
#KEYWORDS
#PKEYWORDS gn genealogy
#INPUTS
#{gn} <<The object gn to consider.>>
#{n1} <<First node set to consider>>
#{n2} <<Second node set to consider>>
#[INPUTS]
#{quel} <<(=1) which directions to consider ? 
#           1 for n1 to n2; 2 for n2 to n1 and 3
#           for both directions>>
#{elono} <<(=TRUE) are eliminated other nodes ?>>
#VALUE
# The reduced object gn.
# Notice that if a node belongs to n1 (n2) but is not
# linked to any node of n2 (n1) it will be eliminated if elono is TRUE.
#EXAMPLE
# rsgn3k("RESET"); # needed only for R checking, to be forgotten
# path4gn(rbsb.gg1,"E","J",3,FALSE);
# plot(path4gn(rbsb.gg1,"A",LETTERS[1:10],3,FALSE));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
# The graph associated to a BN must not have path in both directions
# (because cycles are prohibited)
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_07
#REVISED 07_11_07
#--------------------------------------------
{
if (rbsb.mck) {valid8gn(gn);}
#
if (!is.element(quel,1:3)) { quel <- 1;}
if (gn@item=="n") {
    n1 <- nv2ion(n1,gn@nom,"n",check=FALSE)@nk;
    n2 <- nv2ion(n2,gn@nom,"n",check=FALSE)@nk;
} else {
    n1 <- nv2ion(n1,gn@nom,"N",check=FALSE)@vk;
    n2 <- nv2ion(n2,gn@nom,"N",check=FALSE)@vk;
}
# looking for lignee
if (length(n1)*length(n2) > 0) {
    bah <- numeric(0);
    lig <- minbar8gn(gn);
    for (jbd in 1:2) { 
    if ((quel==jbd) | (quel == 3)) {
	bas <- hau <- numeric(0);
        if (jbd == 1) { pa <- n1; en <- n2;}
        else          { pa <- n2; en <- n1;}
	for (i1 in pa) {
	    bas <- union(bas,which(lig[i1,] >= 0));
	}
	for (i2 in en) {
	    hau <- union(hau,which(lig[,i2] >= 0));
	}
        bah <- union(bah,intersect(bas,hau));
    }}
    if (elono) {
        gn <- rmnd4gn(gn,setdiff(1:nbnv(gn,gn@item),bah));
    }
    else {
        gn <- rmar4gn(gn,setdiff(1:nbnv(gn,gn@item),bah));
    }
}
gn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
gn2path <- function(gn,d,a)
#TITLE (gn) returns all paths between two nodes
#DESCRIPTION
# To return all paths between two nodes, this function
# prepares the parentship matrix and calls
# the recursive function pam2path
#DETAILS
#KEYWORDS
#PKEYWORDS gn genealogy
#INPUTS
#{gn} <<a gn object>>
#{d} <<departure node (ion)>>
#{a} <<arrival node (ion)>>
#[INPUTS]
#VALUE
# a list of vectors with all possible paths
# (nodes are identified by iins)
#EXAMPLE
# rsgn3k("RESET"); # needed only for R checking, to be forgotten
# gn2path(rbsb.gg1,"A","F");
#REFERENCE
#SEE ALSO
#CALLING {pam2path}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_17
#REVISED 09_06_17
#--------------------------------------------
{
if (rbsb.mck) {valid8gn(gn);}
# getting the node numbers
d <- nv2ion(d,gn@nom,"n",check=FALSE)@nk;
a <- nv2ion(a,gn@nom,"n",check=FALSE)@nk;
if ((length(d) != 1) | (length(a) != 1)) {
    erreur(NULL,"Only ONE departure and ONE arrival nodes!");
}
# constructing the parentship matrix
mg <- arc2pam(gn@arc);
# calling pam2path and returning
pam2path(mg,d,a);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
pam2gn <- function(pam,description=rbsb.des0,
                   nom=rbsb.cha0)
#TITLE (gn) from a pam object generates a gn object
#DESCRIPTION
# From a pam object (parentship matrix) generates a gn object. 
# Standard names can be provided for the nodes.
#DETAILS
#KEYWORDS
#PKEYWORDS gn genealogy pam
#INPUTS
#{pam} <<The pam obect>>
#[INPUTS]
#{description} <<(=rbsb.des0) description for the future gn>>
#{nom} <<(=rbsb.cha0) Names for the nodes>>
#VALUE
# a gn object
#EXAMPLE
# rsgn3k("RESET"); # needed only for R checking, to be forgotten
# pam2gn(rbsb.pam1);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_27
#REVISED 09_10_20
#--------------------------------------------
{
# checking
if (rbsb.mck) {valid8pam(pam);}
if (isvide(description)) {
    description <-
    new("des",name=paste("From",deparse(substitute(pam))));
}
nbno <- nrow(pam@rlt);
if (nbno == 0) {
    res <- rbsb.gn0;
} else {
    if (length(nom) < nbno) { nom <- form3names(nbno);
    } else { nom <- nom[1:nbno];}
    res <- new("gn",description=description,
                    nom=char2nom(nom),
                    item="n",
                    arc=pam2arc(pam),
                    pos=new("pos",posi=matrix(0,nbno,4),
                                  zoom=c(0,0,0,1),
                                  view=c(0,0)),
                    pgr=rbsb.pgr);
    res <- position4gn(res);
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
arc4gn <- function(gn,from,to,add=TRUE)
#TITLE (gn) adds or remove some arc(s) to or from a gn object
#DESCRIPTION
# Adds [removes] zero, one arc or a series of arcs to [from]
# a gn and returns the gn so modified. When arcs to add already
# exist, nothing is done. When arc to remove does not exist, 
# nothing is done.
#DETAILS
#KEYWORDS
#PKEYWORDS gn
#INPUTS
#{gn} <<The gn object>>
#{from} <<vector of ion describing the starting nodes of the arcs to add
#         or remove. >>
#{to}   <<vector of ion describing the   ending nodes of the arcs to add
#         or remove. >>
#[INPUTS]
#{add} <<(=TRUE) Adding? (or removing).>>
#VALUE
# The modified gn object.
#EXAMPLE
# rsgn3k("RESET"); # needed only for R checking, to be forgotten
# gn1 <- rbsb.gg1;
# gn2 <- arc4gn(gn1,c("A","C"),c("E","E"));
# gg1 <- arc4gn(gn2,"A","B",FALSE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_28
#REVISED 09_06_17
#--------------------------------------------
{
# checking
if (rbsb.mck) {valid8gn(gn);}
if (length(from) != length(to)) {
    erreur(list(from,to),"'from' and 'to' must have equal lengths");
}
# getting a iin coding
if (gn@item=="n") {
    ff <- nv2ion(from,gn@nom,"n",check=FALSE)@nk;
    tt <- nv2ion(to  ,gn@nom,"n",check=FALSE)@nk;
} else {
    ff <- nv2ion(from,gn@nom,"N",check=FALSE)@vk;
    tt <- nv2ion(to  ,gn@nom,"N",check=FALSE)@vk;
}
# modifying the structure
pam <- arc2pam(gn@arc);
add <- add * 1;
for (fl in sjl(from)) {
    pam@rlt[ff[fl],tt[fl]] <- add;
}
gn@arc <- pam2arc(pam);
# returning
gn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
relation4gn <- function(gn,rel,cl="equal")
#TITLE (gn) formats the arc of a gn from a relation matrix
#DESCRIPTION
# From a completely described gn and a matrix of
# scores between all nodes of it, modifies the format
# of the arcs to take into account the strength of 
# their relations.
#DETAILS
# (1) No arcs are added, only existing arcs are considered.\cr
# (2) The set of existing arcs is categorized into four
#     classes : poor (dashed), small (continuous 1),
#     medium (continuous 2) and strong (continuous 3).\cr
# (3) The classes are defined with three limits defined
#     by cl. cl = 'equal' divides the arcs into approximately
#     four equally numbered categories. cl = 'propto' divides the
#     range of categories into four equal scaled categories. 
#     cl = numeric(3) imposed the three limit to use.\cr
# (4) NA valued arcs by rel are valued as the "minimum - 1"
#     before determining the categories.
#KEYWORDS
#PKEYWORDS gn graph
#INPUTS
#{gn} <<The gn to be modified.>>
#{rel} <<The matrix of scores for the relationships. It is
#        a non symmetric matrix, rows are considered parents
#        of the columns.>>
#[INPUTS]
#{cl}<<(='equal') See the details section.>>
#VALUE The modified gn
#EXAMPLE
# rsgn3k("RESET"); # needed only for R checking, to be forgotten
# g0 <- rbsb.gg1;  
# nbn <- nbnv(g0,g0@item);  
# gg <- relation4gn(g0,matrix(1:nbn^2,nbn));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_10_16
#REVISED 08_10_16
#--------------------------------------------
{
# checking
if (rbsb.mck) {valid8gn(gn);}
if (!is.matrix(rel)) {
    erreur(rel,"'rel' is expected to be a matrix.");
}
nbn <- nbnv(gn,gn@item);
if (!all(dim(rel) == c(nbn,nbn))) {
    erreur(c(dim(rel),nbn),"Non compatible dimension for matrix rel.");
}
# extracting the fle matrix
fle <- gn@arc@fle;
# extracting the corresponding score values
kk <- fle[,1] + nbn*(fle[,2]-1);
sco <- rel[kk];
# replacing the missing values
if (sum(!is.na(sco)) == 0) {
    erreur(rel,"All relations of use are NA!");
}
mii <- min(sco,n.rm=TRUE);
sco[is.na(sco)] <- mii - 1;
# preparing the class limitis
if (is.character(cl)) {
    if (cl == "propto") {
        ra <- range(cl);
        cl <- seq(ra[1],ra[2],le=5)[-c(1,5)];
    } else {
        cl <- quantile(sco,seq(0,1,le=5)[-c(1,5)]);
    }
} else {
    if (!is.numeric(cl)) {
        erreur(cl,"cl must be character or numeric.");
    } else {
        if (length(cl) == 3) {
            cl <- sort(cl);
        } else { erreur(cl,"cl must be of length 3");}
    }
}
# modifying the format of the arcs
cla <- 1 + (sco > cl[1]) + (sco > cl[2]) + (sco > cl[3]);
lar <- 1 + (sco > cl[2]) + (sco > cl[3]);
typ <- 1 + (sco <= cl[1]);
gn@arc@fle[,3] <- 200 + 10*typ + lar;
# returning
gn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
list2gn <- function(li,gnd=1,nod=sjl(li)[-1])
#TITLE (gn) transforms a consistent list into a new /gn/ object
#DESCRIPTION
# Just analyzing the components of the list
# (consistent names have to be used) which are supposed
# to be character and tackle them to produce consistent
# slots of a ds object and description of the nodes...
# The main use of this function is to generate /gn/ read from text files
# with the function \code{file2list}.\cr
# Be aware that the gn item are nodes, not variables (i.e. all nodes
# are univariate with empty variable names.
#DETAILS
# The order of the node is irrelevant, and cycles are possible.\cr
# For details about the field see the code or 'rebastaba.etg?.dat'
# examples.
#KEYWORDS
#PKEYWORDS gn
#INPUTS
#{li} <<The list to be transformed into a gn object.
# The gnd-th component must be the description of the gn;
# the nod components are supposed to be the nodes
# when the remaining arguments are left to default values.>>
#[INPUTS]
#{gnd} <<(=1) the number of the li component to be
#         interpreted as the description of the gn.>>
#{nod} <<(=sjl(li)[-1] the numbers of the li to be
#         considered as defining a node.>>
#VALUE
# The generated /gn/ object
#REFERENCE
#SEE ALSO
#CALLING  {list2des}  {sjl}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_04_06
#REVISED 09_07_13
#--------------------------------------------
{
# checking
check4tyle(li,"list",-1,"The first argument must be a list");
# getting the gn description
na <- li[[gnd]]$name;
if (isvide(na)) { na <- names(li)[nod[gnd]];}
des <- list2des(li[[gnd]],na);
# getting the node names and the
nom <- character(0);
for (jbd in nod) {
    nn <- li[[jbd]];
    if (isvide(nn$name)) {
        nn$name <- names(li)[jbd];
    }
    if (!is.character(nn$name) | (length(nn$name)!=1)) {
        erreur(nn$name,"A node name must be a scalar character");
    }
    if (nn$name %in% nom) {
        erreur(list(nom,nn$name),"The proposed node name was already given");
    }
    nom <- c(nom,nn$name);
}
# getting the parentship matrix and the possible coordinates
nbn <- length(nom);
pam <- matrix(0,nbn,nbn);
dimnames(pam) <- list(nom,nom);
pos <- new("pos",posi=matrix(0,nbn,4),view=c(0,0),zoom=c(0,0,0,1));
if (nbn > 0) { for (jbd in nod) {
    nn <- li[[jbd]];
    if (!isvide(nn$parent)) {
        pp <- char2chars(nn$parent);
        if (!all(pp %in% nom)) {
            erreur(list(nom,pp),"Some proposed parents are not nodes");
        }
        pam[pp,nn$name] <- 1;
    }
    if (!isvide(nn$child)) {
        cc <- char2chars(nn$child);
        if (!all(cc %in% nom)) {
            erreur(list(nom,cc),"Some proposed children are not nodes");
        }
        pam[nn$name,cc] <- 1;
    }
    if (!isvide(nn$posi)) {
        po <- as.numeric(char2chars(nn$posi));
        if (!is.numeric(po)) {
            erreur(nn,"some coordinates ($posi) are not numeric");
        }
        if (!(length(po) %in% 2:3)) {
            erreur(nn,"the number of coordinates ($posi) was not expected",w=rbsb.mwa);
        }
        if (length(po)>0) {
            po <- c(po,rep(0,3));
            pos@posi[which(nn$name==nom),1:3] <- po[1:3];
        }
    }
}}
arc <- pam2arc(new("pam",rlt=pam));
# building it up
gn <- new("gn",description=des,nom=char2nom(nom),item="n",
               pos=pos,arc=arc,pgr=rbsb.pgr);
# checking the result
if (rbsb.mck) {valid8gn(gn);}
# returning
gn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
position4gn <- function(gn,heur="a")
#TITLE (gn) proposes positions for the nodes of a gn object
#DESCRIPTION
# Following some heuristics computes new positions for a /gn/
#DETAILS
#KEYWORDS
#PKEYWORDS gn plot
#INPUTS
#{gn} <<gn object>>
#[INPUTS]
#{heur} <<(='a') indicates which heuristic must be used.
#         (a) for acp on the arc,
#         (h) adapted for hierarchical trees from roots,
#         (h2) idem but with vertical alternance,
#         (H) adapted for hierarchical trees from leafs,
#         (H2) idem but with vertical alternance.
#VALUE
# The same 'gn' object with the new positions
#REFERENCE
#FUTURE 
# Ideas for more heuristics
#{*} <<add two other axes>>
#{*} <<Random>>
#{*} <<on a lattice with some heuristic>>
#{*} <<on concentric circles around a given node,
#      possibly chosen for its properties>>
#{*} <<take into account the direction of arcs>>
#SEE ALSO
#CALLING
#COMMENT
# The algorithm does not seem very effective! Think about other ideas.
#AUTHOR J.-B. Denis
#CREATED 07_05_22
#REVISED 09_10_16
#--------------------------------------------
{
# some checks
if (rbsb.mck) {valid8gn(gn);}
heurp <- c("a","h","h2","H","H2");
if (sum(heur==heurp)!=1) {
    erreur(list(heur,heurp),"argument 'heur' took an inexpected value");
}
# constructing the relationship matrix
eps <- 0.00001;
np <- nbnv(gn,gn@item);
if (np <= 0) {return(gn);}
if (heur=="a") {
    fleches <- gn@arc@fle;
    if (nrow(fleches) > 0) { 
        rrr <- matrix(0,np,np);
        for (i in 1:np) { rrr[i,i] <- 2;}
        for (i in 1:nrow(fleches)) { rrr[fleches[i,1],fleches[i,2]] <- 1;}
        rrr <- rrr + t(rrr) + eps;
        # deducing a distance matrix
        rrr <- 1/rrr - 1/max(rrr);
        # centring the distance matrix
        uu <- apply(rrr,1,mean);
        nn <- rep(1,np);
        rrr <- rrr - outer(uu,nn) - outer(nn,uu) + mean(rrr);
        # finding its first singular vector
        vv <- svd(rrr)$u[,1];
        # deducing the order
        oo <- order(vv);
    }
    else { # no arc
        oo <- 1:np;
    }
    # computing the new positions
    theta <- (1:np)*2*pi/np;
    theta <- theta[oo];
    gn@pos@posi <- cbind(cos(theta),sin(theta),0,0);
    gn@pos@zoom <- c(0,0,0,1);
    gn@pos@view <- rep(0,2);
}
#
if (heur%in% c("h","h2","H","H2")) {
    root <- expr3present("h",heur);
    altv <- expr3present("2",heur);
    if (cycle8pam(gn@pam)) {
        erreur(gn,paste("This graph has got cycle(s), use another heurisitics that",heur));
    }
    ax <- function(x) { if(diff(range(x))>0) {(x-min(x))/(max(x)-min(x));}else{0.5;}}
    # finding the roots
    nbn <- nbnv(gn,gn@item);
    nopla <- ends8gn(gn);
    roots  <- c(nopla$RwL,nopla$RaL);
    leaves <- c(nopla$LwR,nopla$RaL);
    if (length(roots) == 0) {
        rapport("No root for this gn but no cycles were previously detected?");
    }
    if (length(leaves) == 0) {
        rapport("No leave for this gn but no cycles were previously detected?");
    }
    # finding the minimum distance to the roots or leaves as ordinates
    dd <- minbar8gn(gn);
    dd[dd<=0] <- max(dd);
    X <- Y <- rep(NA,nbn);
    Y[roots] <- 0;
    for (ii in sj(nbn)) { if (is.na(Y[ii])) {
        Y[ii] <- min(dd[roots,ii]);
    }}
    # scoring successively the abscissae
    X[roots] <- ax(sjl(roots));
    nbe <- max(Y);
    for (ii in sj(nbe)) { if (length(Y==ii)>0) {
        qq <- which(Y==ii);
        rr <- rep(0,length(qq));
        for (jj in sjl(qq)) {
            jjj <- qq[jj];
            for (kk in sj(nbn)) { if (!is.na(X[kk])) {
                rr[jj] <- rr[jj] + X[kk]*(dd[kk,jjj]==ii);
            }}
        }
        X[qq] <- ax(rr);
    }}
    if (!root) {
        # modifying the ordinates accordingly
        Y <- NA;
        Y[leaves] <- 0;
        for (ii in sj(nbn)) { if (is.na(Y[ii])) {
            Y[ii] <- min(dd[ii,leaves]);
        }}
    }
    if (altv) {
        # alternating vertically
        ry <- range(Y);
        for (y in ry[1]:ry[2]) {
            sel <- which(Y==y);
            if (length(sel) > 1) {
                qui <- sel[(rank(X[sel]) %% 2) == 0];
                Y[qui] <- Y[qui]+0.25;
            }
        }
    }
    # constructing the gn
    gn@pos@posi <- cbind(X,Y,0,0);
    gn@pos@zoom <- c(0,0,0,1);
    gn@pos@view <- rep(0,2);
}
# returning
gn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
plot3D8gn <- function(gn,...)
#TITLE (gn) plots an object gn according to its 3 dimensions
#DESCRIPTION
# The figure is divided into 4 quadrants, 3 of them are used to 
# draw the graph according to its (X,Y), (X,Z) and (Z,Y) coordinates
#DETAILS
#KEYWORDS
#PKEYWORDS plot gn
#INPUTS
#{gn} <<the gn to plot>>
#[INPUTS]
#{...} <<to give when first calling to the standard plot function>>
#VALUE
#  returns nothing but a plot is drawn
#EXAMPLE
# rsgn3k("RESET"); # needed only for R checking, to be forgotten
# plot3D8gn(rbsb.gg1);
#REFERENCE
#SEE ALSO
#CALLING {plot8cgn} 
#COMMENT
#FUTURE
# In the last quadrant, plots the ACP with the first two axis derived 
# from the pseudo-distance given by mindis8pam (or pam matrix)...
#AUTHOR J.-B. Denis
#CREATED 07_07_17
#REVISED 07_11_07
#--------------------------------------------
{
if (rbsb.mck) {valid8gn(gn);}
  # preparing the plots
  nbno <- length(gn@nom);
  mm <- 0.1; ra <- rep(NA,3);
  for (i in 1:3) { ra[i] <- diff(range(gn@pos@posi[,i])); }
  ma <- mm*ra;
  mi <- ra[3]+2*ma[3];
  xr <- c(0,ra[1]+2*ma[1]+mi);
  yr <- c(0,ra[2]+2*ma[2]+mi);
  plot(0,0,xlim=xr,ylim=yr,type="n",
       axes=FALSE,xlab="",ylab="",...);
  # axis
  text(xr[2],mi,"X",cex=3);
  text(mi,yr[2],"Y",cex=3);
  text(xr[1],mi,"Z",cex=3);
  text(mi,yr[1],"Z",cex=3);
  abline(h=mi);abline(v=mi);
  # last quadrant
  xgr <- mi/2; ygr <- mi/2;
  tgr <- paste(nbno,"nodes and",nrow(gn@arc@fle),"arcs");
  text(xgr,ygr,tgr);

  # drawing nodes and arcs (X,Y)
  gd <- gn; 
  calage <- c(mi+ma[1]-min(gd@pos@posi[,1]),
              mi+ma[2]-min(gd@pos@posi[,2]));
  xy <-  gn@pos@posi[,1:2] + 
                  matrix(calage,nbno,2,byrow=TRUE);
  plot8cgn(gd,xy);
  # drawing nodes and arcs (X,Z)
  gd <- gn; 
  calage <- c(mi+ma[1]-min(gd@pos@posi[,1]),
                 ma[3]-min(gn@pos@posi[,3]));
  xy <- gn@pos@posi[,c(1,3)] +
                 matrix(calage,nbno,2,byrow=TRUE);
  plot8cgn(gd,xy);
  # drawing nodes and arcs (Z,Y)
  gd <- gn; 
  calage <- c(ma[3]-min(gd@pos@posi[,3]),
              mi+ma[2]-min(gd@pos@posi[,2]));
  xy <- gn@pos@posi[,c(3,2)] +
                  matrix(calage,nbno,2,byrow=TRUE);
  plot8cgn(gd,xy);
invisible();    
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rgn <- function(des,type="hier",
                 nno=c(5,10),nar=c(10,20),
                 nom=LETTERS,dire="nat",lev=1)
#TITLE (gn) creates a pseudo-random gn object
#DESCRIPTION
#      Creates a gn object which characteristics are
# randomly generated following the requirements of the call.
# Also positions illuminating the struture are proposed.
# Normally the produced gn is connected and without cycles.
# When the request is not possible, a warning is issued and
# some compatible option is provided. Using this function with
# the same set.seed starting point (and the same request) must 
# produce identical gn.\cr
# Different types of gn are possible, their characteristics
# are given by arguments and pseudo-randomness. The type argument
# describes the structure of the links without care about the
# directions (as edges). The possible types are:\cr
#{chain} <<All nodes are aligned in a simple chain. So a relationship
#          exists between nno and nar: nno = nar +1. Then nno will
#          be drawn and nar deduced.>>
#{ring} <<Like the chain type but a cycle is done with the
#         relationships and nno = nar.>>
#{hier} <<A hierarchy in the sense that the nodes can be disposed in
#         a dendrogram structure; a particular case of this is the "star"
#         structure when there only a branching point and the branches can
#         comprise several nodes.>>
#{audit} <<A hierarchy with two levels. For this specific case, 'nno' is 
#          the number of grouping levels and 'nar' is the number of 
#          terminal nodes for each grouping levels. Names of nodes are
#          determined accordingly.>>
#{square} <<Not so simple to describe. Nodes are placed in the cell of
#           square matrix and edges between nodes respects this structure.
#           Arcs (including their number) are deduced from the node
#           positions. Run some cases and you will understand seing the 
#           plot.>>
#{tetra} <<Four of the nodes are on the vertices of a tetrahedron and the
#           other nodes are on its edges.>>
#{total} <<The maximum number of arcs, nno*(nno-1)/2, is provided.
#          So nar will not be considered.>>
# It is suggested that the seed be initialized before calling this function.
#DETAILS
#         Of course if too much arcs are ordered, the probability of a
#         non-cycle graph may be very small, even null.\cr
#         To prevent this, a local nb_trial limits the number of draws,
#         if this limit is reached then a fatal error is issued.
#KEYWORDS
#PKEYWORDS gn
#INPUTS
#{des} <<Either the name or the description (preferably) for the 
#        newly created gn.>>
#[INPUTS]
#{type} <<(="hier") type of the gn, see the description field for details>>
#{nno} <<(=c(5,10) range within which must be the number of nodes. Of course
#         the range can be null, in that case the number of nodes is directly
#         provided by the user. If not, it is drawn according to a uniform
#         distribution (on integer from nbno[1] to nbno[2]).\cr
#         When audit, it gives the number of grouping nodes.>>
#{nar} <<(=c(10,20) range within which must be the number of arcs. Similar
#         to nbar but nno being fixed nar must satisfy some constraints. These
#         constraints may also depend on the type of gn.\cr
#         When audit, it gives the number of nodes for each grouping nodes.>>
#{nom} <<(=LETTERS) vector of names to be used for the nodes.>>
#{dire} <<(="nat") indicates the directions of the arcs between the
#          nodes of a bn. There are two possibilities: "nat" for natural
#          directions and "ran" for random under the restriction of no 
#          cycles.\cr
#          For 'audit' the two choices are 'nat' for putting the first node
#          as root else it will be the synthesis.\cr
#          No effect when type ="ring".>>
#{lev} <<(=1) maximum level of hierarchy; only when type == "hier".>>
#VALUE
#EXAMPLE
# set.seed(1234);
# rsgn3k("RESET"); # needed only for R checking, to be forgotten
# r1 <- rgn(char2des("r1"),type="hier");
# r2 <- rgn(char2des("r2"),type="square");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
# It is good to have only ONE rgn function, so that this one
# can generate a large variety of gns. Nevertheless, it is not
# that easy that the same collection of argument can apply to
# every one. An idea for the future could be to have only three
# arguments : des, type and argu where argu is a list where the 
# components might be different according to the type.
#AUTHOR J.-B. Denis
#CREATED 07_10_08
#REVISED 09_10_20
#--------------------------------------------
{
# some precautions
nno <- round(range(nno)); nar <- round(range(nar));
if (min(c(nno,nar)) < 1) {
    erreur(list(nno,nar),"At least one node and one arc are necessary!");
}
#
if (type=="audit") {
    # a new type differently based
    ## determining the number of grouping nodes
    if (nno[1]==nno[2]) {nbgn <- nno[1];
    } else { nbgn <- sample(nno[1]:nno[2],1);}
    ## determining the number of nodes within each
    if (nar[1]==nar[2]) { nbno <- rep(nar[1],nbgn);
    } else {
        nbno <- sample(nar[1]:nar[2],nbgn,replace=TRUE);
    }
    ## determining the names of the nodes
    nbn <- 1+nbgn+sum(nbno);
    if (length(nom) >= nbn) { noms <- nom[1:nbn];
    } else {
        noms <- rep(NA,nbn);
        if (dire=="nat") { noms[1] <- "<<SYNTH>>";
        } else { noms[1] <- "<<ROOT>>";}
        noms[1+1:nbgn] <- form3names(nbgn);
        KK <- 1+nbgn;
        for (i in 1:nbgn) {
            noms[KK+1:nbno[i]] <- paste(noms[1+i],"(",1:nbno[i],")",sep="");
            KK <- KK + nbno[i];
        }
        noms[1+1:nbgn] <- paste("<",noms[1+1:nbgn],">",sep="");
    }
    ## determining the positions
    posi <- matrix(0,nbn,4);
    posi[1,1]              <- 1;
    posi[1+1:nbgn,2]       <- -1;
    posi[1+1:nbgn,1]       <- (-1/2 + (1:nbgn))*2/nbgn;
    posi[-c(1:(nbgn+1)),2] <- -2;
    posi[-c(1:(nbgn+1)),1] <- (-1/2 + (1:sum(nbno)))*2/sum(nbno);
    ## determining the arcs for this gn
    nbar <- nbn-1;
    mar <- new8arc(nbn,nbar);
    mar@fle[1:nbgn,1:2] <- c(rep(1,nbgn),1+1:nbgn);
    KK <- nbgn;
    for (i in 1:nbgn) {
        mar@fle[KK+1:nbno[i],1] <- rep(1+i,nbno[i]);
        mar@fle[KK+1:nbno[i],2] <- 1+KK + (1:nbno[i]);
        KK <- KK + nbno[i];
    }
    if (dire == "nat") {
        uu <- mar@fle[,1];
        mar@fle[,1] <- mar@fle[,2];
        mar@fle[,2] <- uu;
    }
    pos <- new("pos",posi=posi,zoom=c(0,0,0,1),view=c(0,0));
    ## returning
    gg <- new("gn",
              description=char2des(des),
              nom=char2nom(noms),
              item="n",
              arc=mar,
              pos=pos,
              pgr=rbsb.pgr);
    return(gg);
}
# trying to avoid cycles
nb_trial <- 1000;
# numbers of nodes and arcs
if (nno[1] == nno[2]) { nbno <- nno[1];
} else { nbno <- sample(nno[1]:nno[2],1);}
if (type %in% c("chain","hier")) { nbar <- nbno - 1;
} else {
    if (type == "ring") { nbar <- nbno;
    } else {
        if (type == "total") { nbar <- nbno*(nbno-1)/2;
        } else {
            if (nar[1] == nar[2]) { nbar <- nar[1];
            } else { nbar <- sample(nar[1]:nar[2],1);}
        }
    }
}
# names for the nodes
if (length(nom) < nbno) { nom <- form3names(nbno);
} else { nom <- nom[1:nbno];}
# the different types
ltype <- c("chain","ring","hier","square","tetra","total","audit");
if (!(type %in% ltype)) {
    cat("You ask a gn of type",type,"but this type is not yet implemented\n");
    cat("Available type are:",ltype,"\n");
    erreur(NULL,"type not available!");
}
mar <- new8arc(nbno,nbar);
posi <- matrix(0,nbno,4);
lim <- matrix(NA,2,3);
vie <- rep(0,2);
if (type == "chain") {
    for(jbd in 1:nbar) {
        if (dire == "nat") { fle <-  c(jbd,jbd+1);
        } else { fle <- sample( c(jbd,jbd+1),2);}
        mar@fle[jbd,1:2] <- fle;
    }
    pp <- arc2pam(mar);
    for(jbd in 1:nbno) {
         uu <- sum(pp@rlt[jbd,]) - sum(pp@rlt[,jbd]);
         posi[jbd,1:3] <- c(jbd,uu,0);
    }
}
if (type == "ring") {
    mar@fle[nbar,1:2] <- sample(c(nbar,1));
    for(jbd in 1:(nbar-1)) {
        mar@fle[jbd,1:2] <- sample( c(jbd,jbd+1),2);
    }
    if(cycle8pam(arc2pam(mar))) { mar@fle[1,1:2] <- mar@fle[1,c(2,1)];}
    for(jbd in 1:nbno) {posi[jbd,1:3] <- c(cos(2*pi/nbno*jbd),
                                       sin(2*pi/nbno*jbd),0);}
}
if (type == "hier") {
    # parentship
    enf <- rep(0,nbno); niv <- rep(NA,nbno); names(niv) <- nom;
    enf[1] <- niv[1] <- 0; nbf <- 1;
    for (jbd in 2:nbno) { while(is.na(niv[jbd])) {
        jd <- sample(which(!is.na(niv)),1);
        if ((niv[jd] <= lev) | (enf[jd] == 0)) {
            niv[jbd] <- niv[jd] + 1;
            enf[jd] <- enf[jd] + 1;
            mar@fle[jbd-1,1:2] <- c(jd,jbd);            
        }
    }}
    # positions
    mmm <- arc2pam(mar);
    xx <- yy <- rg <- rep(NA,nbno); names(rg) <- nom;
    mlev <- max(niv);
    xx[1] <- yy[1] <- 0; rg[1] <- 1;
    for (jbd in 1:mlev) {
        # dealing with level jbd
        qui <- which(niv == jbd);
        val <- rep(0,length(qui));
        for (jd in sjl(qui)) {
            che <- pam2path(mmm,1,qui[jd])[[1]];
            ch <- che[-length(che)];
            for (sd in 1:length(ch)) {
                qq <- ch[sd];
                val[jd] <- val[jd] + rg[qq]*nbno^(mlev-niv[qq]);
            }
        }
        rg[qui] <- rank(val,ties.method="random");
        xx[qui] <- rg[qui] - mean(rg[qui]);
        yy[qui] <- -jbd;
    }
    posi <- cbind(xx,yy,rep(0,nbno),rep(0,nbno));
    # randomness
    if (dire == "ran") {
        for (jbd in 1:nrow(mar@fle)) {
            mar@fle[jbd,1:2] <- sample(mar@fle[jbd,1:2],2);
        }
    }
}
if (type == "square") {
    # square size
    k <- ceiling(sqrt(nbno));
    # ensuring connectedness
    ppo <- matrix(0,k,k);
    ppo[1,] <- 1:k; ppo[2:k,1] <- (k+1):(2*k-1);
    ppo <- ppo[sample(k),sample(k)];
    # positionning the remaining nodes
    for (jbd in sj(nbno-2*k+1)) {
        jd <- jbd + 2*k - 1;
        uu <- sample(nbno - jd + 1,1);
        vv <- which(ppo == 0)[uu];
        ppo[vv] <- jd;
    }
    # defining arcs and positions
    mar <- new8arc(nbno,0);
    for (i in sj(k)) { for (j in sj(k)) { if (ppo[i,j] > 0) {
        # by column
        if (i < k) {
            uu <- ppo[(i+1):k,j];
            if (sum(uu) > 0) {
                qq <- which(uu > 0)[1];
                mar@fle <- rbind(mar@fle,c(uu[qq],ppo[i,j],211));
            }
        }
        # by row
        if (j < k) {
            uu <- ppo[i,(j+1):k];
            if (sum(uu) > 0) {
                qq <- which(uu > 0)[1];
                mar@fle <- rbind(mar@fle,c(uu[qq],ppo[i,j],211));
            }
        }
        posi[ppo[i,j],1:3] <- c(k-j,i,0);
    }}}
    # randomness
    if (dire == "ran") {
        for (jbd in 1:nrow(mar@fle)) {
            mar@fle[jbd,1:2] <- sample(mar@fle[jbd,1:2],2);
        }
        ppa <- arc2pam(mar);
        nbt <- 1;
        while(cycle8pam(ppa)) {
            if (nbt > nb_trial) {
                erreur(NULL,"Seems difficult not to get cycles!");
            }
            nbt <- nbt + 1;
            for (jbd in 1:nrow(mar@fle)) {
                 mar@fle[jbd,1:2] <- sample(mar@fle[jbd,1:2],2);
            }
            ppa <- arc2pam(mar);
        }
    }
}
if (type == "tetra") {
    erreur(type,"to be done");
}
if (type == "total") {
    kk <- 0;
    for (jbd in 1:(nbno-1)) { for (jd in (jbd+1):nbno) {
        kk <- kk+1;
        mar@fle[kk,1:2] <- c(jbd,jd);
    }}
    for(jbd in 1:nbno) {posi[jbd,1:3] <- c(cos(2*pi/nbno*jbd),
                                       sin(2*pi/nbno*jbd),0);}
}
# returning
pos <- new("pos",posi=posi,zoom=c(0,0,0,1),view=c(0,0));
#
gg <- new("gn",
          description=char2des(des),
          nom=char2nom(nom),
          item="n",
          arc=mar,
          pos=pos,
          pgr=rbsb.pgr);
gg;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
modify4gn <- function(gn)
#TITLE (gn) interactive modifications of a gn
#DESCRIPTION
# graphical interface to modify (i) the positions, 
# (ii) the arcs and (iii) the nodes of an object of class gn.
# Besides, it provides the facility to change the viewpoint
# from which is drawn the graph (positions of a gn object
# are defined in R to the 3).
# \cr The modified gn is returned.
#\cr
# To exit, just click the grey button 'E'
#\cr
# Action are started by the left hand buttons. Their codes are:\cr
# "rst" for resetting (cancel all modifications),\cr
# "MN" for moving the position of a node (the user must click 
# twice: for indicating the node and then the new desired
# position),\cr
# "AA" for adding an arc (the user must click twice to indicate the
# starting node then the ending node), \cr
# "RA" for removing an existing arc (the user must click twice to
# indicate the arc to be removed), \cr
# "AN" for adding a node (the user must click once to indicate the
# position of the new node), its name is automatically provided 
# but can be changed afterwards, \cr
# "RN" for removing a node (the user must click once, on the node
# to be removed), \cr
# "+" for narrowing the zoom (the user must click once, on the point
# where the zoom must be centered; the zooming coefficient is 
# driven by the @pgr@kzoom parameter),\cr
# "-" for widing the zoom (the user must click once, on the point
# where the zoom must be centered; the zooming coefficient is 
# driven by the @pgr@kzoom parameter),\cr
# "Rot" for rotating the representation in R to the three (the
# user must click once, in the direction of the rotation and more or less
# close to the center to indicate the angle to perform; the two big 
# circles which appears on the diagram are associated to rotations of
# 5 and 10 degrees.
#DETAILS
#KEYWORDS
#PKEYWORDS plot gn
#INPUTS
#{gn} <<the gn object gn to be interactively modifyed.>>
#[INPUTS]
#VALUE
# The modified gn by the actions performed by the user.
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
# The initial basic algorithm is inspired from some deal function (thanks to them)
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_05_22
#REVISED 09_05_06
#--------------------------------------------
{
# checking
if (rbsb.mck) {valid8gn(gn);}
# defining ancillary functions
quequoi <- function(point) {
    if (length(point) != 2) {
        erreur(NULL,paste("Expecting TWO coordinates, possibly the",
	"graphic device	is not open for interactive way?",
        "if so, just execute 'dev.off()'"));
    }
    # returns an integer.
    # if negative, it is the number of a pressed button
    # if positive, it is the number of the closest node
    if ((point[1] >= xre[2]) | (point[1] < xre[1])) {
        di <- (point[1] - posbut[,1])^2 + (point[2] - posbut[,2])^2;
        res <- -which(min(di) == di)[1];
    } else {
        di <- (point[1] - gd@pos@posi[,1])^2 + (point[2] - gd@pos@posi[,2])^2;
        res <- which(min(di) == di)[1];
    }
     res;
}
dessinne <- function() {
    #######################################################
    # recomputing necessary parameters for a new order
    #######################################################
    newp <- pos2pos(gd@pos);
    rara <- range8pos(newp);
    # ranges for the graph
    xre <- rara$x; yre <- rara$y;
    # global ranges
    mx <- (xre[2] - xre[1])*pbut;
    xr <- c(xre[1]-mx,xre[2]+mx);
    my <- (yre[2] - yre[1])*pbut;
    yr <- c(yre[1]-my,yre[2]+my);
    # indicating the view
    xvi <- (xr[1]+xre[1])/2;
    yvi <- yre[1] + 4*my;
    tvi <- paste("Theta =",
                 round(gd@pos@view[1]*180/pi),"deg.",
                 "    and    Phi =",
                 round(gd@pos@view[2]*180/pi),"deg.");
    # indicating the graph
    xgr <- xre[1] + 4*mx;
    ygr <- (yr[2]+yre[2])/2 + my*0.3;
    tgr <- paste(nbno,"nodes and",nrow(gd@arc@fle),"arc(s)",
                 "   {{{",round(gd@pos@zoom[4],1),"}}}");
    # indicating the contextual message
    xme <- xre[1] + 4*mx;
    yme <- (yr[1]+yre[1])/2 - my*0.3;
    # defining the positions and the sizes of the buttons
    posbut <- sizbut <- matrix(NA,nbut,2);
    for (i in 1:(nbut-1)) { 
	posbut[i,1] <- (xre[2]+xr[2])/2;
	posbut[i,2] <- yre[2] - i*my*1;
	sizbut[i,1] <- mx*kbut;
	sizbut[i,2] <- my*kbut;
    }
    posbut[nbut,1] <- (xre[1]+xr[1])/2;
    posbut[nbut,2] <- (yre[1]+yr[1])/2;
    sizbut[nbut,1] <- mx*kbut;
    sizbut[nbut,2] <- my*kbut;
    # defining the color of the buttons
    colbut <- matrix(c("white","black"),nbut,2,byrow=TRUE);
    # starting the plot
    plot(0,0,type="n",#axes=FALSE,
         xlim=xr,ylim=yr,
         xlab="",ylab="");
    # drawing the graph
    plot8cgn(gd,newp@posi[,1:2]);
    # adding a working frame
    abline(h=yre,lty=3); abline(v=xre,lty=3);
    # displaying the projection view
    text(xvi,yvi,tvi,srt=90);
    # displaying the graph traits
    text(xgr,ygr,tgr);
    # displaying the contextual message
    text(xme,yme,messa[message]);
    # displaying the buttons
    colbut[nbut,1] <- "gray";
    for (jd in 1:(nbut-1)) {
        if (mode==conbut[jd]) {colbut[jd,] <- c("black","white");
        } else { colbut[jd,] <- c("white","black"); }
    }
    for (jbd in 1:nbut) {
        symbols(posbut[jbd,1],posbut[jbd,2],
                rectangles=matrix(sizbut[jbd,],1,2),
                bg=colbut[jbd,1],
                add=TRUE,inches=FALSE);
        text   (posbut[jbd,1],posbut[jbd,2],
                labels=conbut[jbd],col=colbut[jbd,2]);
    }
list(xre=xre,yre=yre,posbut=posbut);
}
  nbno <- nbnv(gn,gn@item);
  # parametering the button sizes and some margin size
  pbut <- 0.10; kbut <- 0.8;
  # calibration of the rotation
  anglemax <- pi/2;
  # starting
  par(mfrow=c(1,1));
  messa <- c("(1/2) Click the node to move",
             "(2/2) Click where to move the node",
             "(1/2) Click the starting node",
             "(2/2) Click the ending node",
             "(1/2) Click the starting node",
             "(2/2) Click the ending node",
             "(1/1) Click where to put the new node",
             "(1/1) Click the node to remove",
             "(1/1) Click the center of the magnyfication",
             "(1/1) Click the center for the reduction",
             "(1/1) Click direction and angle of the rotation");
  names(messa) <- c("MN","MN2","AA","AA2",
                    "RA","RA2","AN","RN",
                    "+","-","Rot");
  # intitial mode  
  mode <- "MN"; message <- "MN";
  # defining the contents of the buttons
  nbut <- 10;
  conbut <- c("rst","MN","AA","RA","AN","RN","+","-","Rot","Exit");
  gd <- gn;
  ### BEGINNING OF MODIFICATION LOOP
  while(TRUE) {
    #######################################################
    # drawing an updated screen
    #######################################################
    uuu <- dessinne();
    xre <- uuu$xre; yre <- uuu$yre; posbut <- uuu$posbut;
    ########################################################
    # according to the mode different actions are awaited
    ########################################################
    # exiting
    if (mode == "Exit") { break;}
    # resetting
    if (mode == "rst") {
        gd <- gn; mode <- "MN"; next; message <- mode;
    }
    # moving a node
    if (mode=="MN") {
        mode <- "MN"; message <- mode; 
        uuu <-  dessinne();
        xre <- uuu$xre; yre <- uuu$yre; posbut <- uuu$posbut;
        # we must get the node to move
        ou <- unlist(locator(1));
        ni <- quequoi(ou);
        if (ni < 0) {
            if (-ni == nbut) { break;}
            mode <- conbut[-ni];  message <- mode;
            next;
        }
        # we must get the point to move there
        message <- "MN2"; 
        uuu <-  dessinne();
        xre <- uuu$xre; yre <- uuu$yre; posbut <- uuu$posbut;
        ou <- unlist(locator(1));
        nu <- quequoi(ou);
        if (nu < 0) {
            if (-nu == nbut) { break;}
            mode <- conbut[-nu]; message <- mode;
            next;
        } else {
            gd@pos@posi[ni,1:2] <- ou;
        }
    }
    # adding an arc
    if (mode=="AA") {
        mode <- "AA"; message <- mode; 
        uuu <-  dessinne();
        xre <- uuu$xre; yre <- uuu$yre; posbut <- uuu$posbut;
        # we must get the starting node
        ou <- unlist(locator(1));
        ni <- quequoi(ou);
        if (ni < 0) {
            if (-ni == nbut) { break;}
            mode <- conbut[-ni]; message <- mode;
            next;
        }
        # we must get the ending node
        message <- "AA2"; 
        uuu <-  dessinne();
        xre <- uuu$xre; yre <- uuu$yre; posbut <- uuu$posbut;
        ou <- unlist(locator(1));
        nj <- quequoi(ou);
        if (nj < 0) {
            if (-nj == nbut) { break;}
            mode <- conbut[-nj]; message <- mode;
            next;
        } else {
            pam <- arc2pam(gd@arc);
            pam@rlt[ni,nj] <- 1;
            gd@arc <- pam2arc(pam);
            mode <- "AA"; message <- mode; 
            uuu <-  dessinne();
            xre <- uuu$xre; yre <- uuu$yre; posbut <- uuu$posbut;
            next;
        }
    }
    # removing an arc
    if (mode=="RA") {
        mode <- "RA"; message <- mode; 
        uuu <-  dessinne();
        xre <- uuu$xre; yre <- uuu$yre; posbut <- uuu$posbut;
        # we must get the starting node
        ou <- unlist(locator(1));
        ni <- quequoi(ou);
        if (ni < 0) {
            if (-ni == nbut) { break;}
            mode <- conbut[-ni]; message <- mode;
            next;
        }
        # we must get the ending node
        message <- "RA2"; 
        uuu <-  dessinne();
        xre <- uuu$xre; yre <- uuu$yre; posbut <- uuu$posbut;
        ou <- unlist(locator(1));
        nj <- quequoi(ou);
        if (nj < 0) {
            if (-nj == nbut) { break;}
            mode <- conbut[-nj]; message <- mode;
            next;
        } else {
            pam <- arc2pam(gd@arc);
            pam@rlt[ni,nj] <- 0;
            gd@arc <- pam2arc(pam);
            mode <- "RA"; message <- mode; next;
        }
    }
    # adding a node
    if (mode=="AN") {
        mode <- "AN"; message <- mode; 
        uuu <-  dessinne();
        xre <- uuu$xre; yre <- uuu$yre; posbut <- uuu$posbut;
        # we must get where to put a supplementary node
        ou <- unlist(locator(1));
        ni <- quequoi(ou);
        if (ni < 0) {
            if (-ni == nbut) { break;}
            mode <- conbut[-ni]; message <- mode;
            next;
        } else {
            nena <- form3names(1,gn2item(gd));
            gd <- and4gn(gd,nena);
            gd@pos@posi[nbnv(gd,gd@item),1:2] <- ou;
            mode <- "AN"; message <- mode; next;
        }
    }
    # removing a node
    if (mode=="RN") {
        mode <- "RN"; message <- mode; 
        uuu <-  dessinne();
        xre <- uuu$xre; yre <- uuu$yre; posbut <- uuu$posbut;
        # we must get the node to remove
        ou <- unlist(locator(1));
        ni <- quequoi(ou);
        if (ni < 0) {
            if (-ni == nbut) { break;}
            mode <- conbut[-ni]; message <- mode;
            next;
        } else {
            gd <- rmnd4gn(gd,ni);
            mode <- "RN"; message <- mode; next;
        }
    }
    # zooming more
    if (mode=="+") {
        mode <- "+"; message <- mode; 
        uuu <-  dessinne();
        xre <- uuu$xre; yre <- uuu$yre; posbut <- uuu$posbut;
        # we must get the center point for applying the zoom
        ou <- unlist(locator(1));
        ni <- quequoi(ou);
        if (ni < 0) {
            if (-ni == nbut) { break;}
            mode <- conbut[-ni]; message <- mode;
            next;
        } else {
            ou[1] <- -1 + 2*(ou[1]-min(gd@pos@posi[,1]))/
            (max(gd@pos@posi[,1])-min(gd@pos@posi[,1]));
            ou[2] <- -1 + 2*(ou[2]-min(gd@pos@posi[,2]))/
            (max(gd@pos@posi[,2])-min(gd@pos@posi[,2]));
            gd@pos@zoom <- c(ou,gd@pos@zoom[3],gd@pos@zoom[4]*gd@pgr@kzoom);
            mode <- "+"; message <- mode; next;
        }
    }
    # zooming less
    if (mode=="-") {
        mode <- "-"; message <- mode; 
        uuu <-  dessinne();
        xre <- uuu$xre; yre <- uuu$yre; posbut <- uuu$posbut;
        # we must get the center point for applying the zoom
        ou <- unlist(locator(1));
        ni <- quequoi(ou);
        if (ni < 0) {
            if (-ni == nbut) { break;}
            mode <- conbut[-ni]; message <- mode; 
            next;
        } else {
            ou[1] <- -1 + 2*(ou[1]-min(gd@pos@posi[,1]))/
            (max(gd@pos@posi[,1])-min(gd@pos@posi[,1]));
            ou[2] <- -1 + 2*(ou[2]-min(gd@pos@posi[,2]))/
            (max(gd@pos@posi[,2])-min(gd@pos@posi[,2]));
            gd@pos@zoom <- c(ou,gd@pos@zoom[3],gd@pos@zoom[4]/gd@pgr@kzoom);
            mode <- "-"; message <- mode; next;
        }
    }
    # rotating
    if (mode=="Rot") {
        mode <- "Rot"; message <- mode; 
        uuu <-  dessinne ();
        xre <- uuu$xre; yre <- uuu$yre; posbut <- uuu$posbut;
        # indicating the center (start for rotation)
        # center of the plot and radius
        centx <- mean(xre); centy <- mean(yre);
        rayon <- sqrt(diff(xre)^2+diff(yre)^2);
        for (jd in 1:3) { points(centx,centy,cex=jd);}
        # putting calibration rotation of 5 deg. and 10 deg.
        symbols(rep(centx,2),rep(centy,2),
                circles=0.5*rayon*(diff(xre)/anglemax)*pi/180*c(5,10),
                add=TRUE,inches=FALSE);
        # we must get Theta and Phi angles
        ou <- unlist(locator(1));
        ni <- quequoi(ou);
        if (ni < 0) {
            if (-ni == nbut) { break;}
            mode <- conbut[-ni];
            next;
        } else {
            theta <- ou[2] / diff(xre) * anglemax;
            phi   <- ou[1] / diff(xre) * anglemax;
            gd@pos@view <- gd@pos@view + c(theta,phi);
            gd@pos <- pos2pos(gd@pos);
            mode <- "Rot"; next;
        }
    }
} ### ENDING THE CLICKING GAME
dev.off();
gd;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
read8gn <- function(fi)
#TITLE (gn) produced a /gn/ from a text file
#DESCRIPTION
# The text file must follow the structure compatible
# with \code{file2list}. The first component of the file
# must be the description of the /gn/; the remaining 
# components are supposed to be the nodes.
#DETAILS
# See \code{rebastaba.etg?.txt} files to get 
# straightforward examples.
#KEYWORDS
#PKEYWORDS gn file
#INPUTS
#{fi} <<The name of the file to be considered.>>
#[INPUTS]
#VALUE
# The generated /gn/ object
#REFERENCE
#SEE ALSO
#CALLING  {file2list} {list2gn}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_04_06
#REVISED 09_04_06
#--------------------------------------------
{
# reading the list
li <- file2list(fi);
# getting the gn
gn <- list2gn(li);
# precautionary checking
if (rbsb.mck) {valid8gn(gn);}
# returning
gn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
minbar8gn <- function(gn)
#TITLE (gn) finds the minimum number of arcs between nodes of a gn object
#DESCRIPTION
# Finds the minimum number of arcs between nodes of a gn object.
# To do so, the parentship is explored and a matrix is returned.
# This matrix is similar to the pam matrix but is richer. Its
# rows and columns are associated to each node. [i,j] is the minimum
# number of arcs to follow to go from node i to node j. When this
# is not possible, a value of -1 is introduced.
#DETAILS
# Own nodes are considered as zero distance children (in
# other words the diagonal of the resulting matrix is not
# -1 but 0).
#KEYWORDS utilities
#PKEYWORDS gn genealogy
#INPUTS
#{gn} <<The gn object to be investigated>>
#[INPUTS]
#VALUE
# a matrix (see DESCRIPTION field)
#EXAMPLE
# rsgn3k("RESET"); # needed only for R checking, to be forgotten
# minbar8gn(rbsb.gg1);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
# The transposed matrix has a similar interpretation
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_07
#REVISED 09_06_09
#--------------------------------------------
{
if (rbsb.mck) {valid8gn(gn);}
nbno <- nbnv(gn,gn@item); 
noms <- gn2item(gn);
# particular cases
if (nbno < 2) {
    return(matrix(0,nbno,nbno,dimnames=list(noms,noms)));
}
# initialization
res <- matrix(-1,nbno,nbno);
res <- res + diag(nbno);
dimnames(res) <- list(noms,noms);
# doing it
nbf <- 0; # nbr of arcs to obtain the child
nba <- 1; # nbr of added children during the loop
while (nba > 0) {
    nbf <- nbf + 1; nba <- 0;
    for (jbd in 1:nbno) {
        enf <- which(res[jbd,] != -1); # children for nodes jbd
        pos <- which(apply(outer(gn@arc@fle[,1],enf,"=="),1,sum)==1); # starting arcs
        nenf <- gn@arc@fle[pos,2]; # possible new children
        nenf <- setdiff(nenf,enf);# actual new children
        if (length(nenf) > 0) {
            nba <- nba + length(nenf);
            res[jbd,nenf] <- nbf;
        }
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
and4gn <- function(gn,names=1,pos=NULL)
#TITLE (gn) adds a series of nodes to a gn object
#DESCRIPTION
# This function adds a series of nodes to a /gn/
# without any links giving them the names into
# argument 'names'. They are unidimensional with
# empty variable names.
#DETAILS
# With respect to the internal order, the new nodes
# are introduced at the end.
#KEYWORDS
#PKEYWORDS gn nd
#INPUTS
#{gn}<<The object gn to be completed>>
#[INPUTS]
#{names} <<(=1) when numeric of length 1, it
#               indicates the number of nodes to add
#               (and the name will be automatically
#               determined by form3names function.
#               When character it is the vector
#               of the new names and its length gives
#               the number of node to add.>>
#{pos} <<(=NULL) gives the positions to add.
#        When null, they will be automatically provided.
#        When a matrix, it must comprise as many rows as
#        new nodes and 2 (x,y) or 3 (x,y,z) columns.>>
#VALUE
# The completed object gn
#EXAMPLE
# rsgn3k("RESET"); # needed only for R checking, to be forgotten
# and4gn(rbsb.gg1,c("U","V","W"));  
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_11_12
#REVISED 09_05_06
#--------------------------------------------
{
# checking and preparing
if (rbsb.mck) {valid8gn(gn);}
if (gn@item!="n") {
    erreur(gn,"For the moment, adn4gn does not work when the slot item is not 'n'");
}
# about the new names
items <- gn2item(gn);
if (is.numeric(names)) {
    check4tyle(names,"any",1,"When numeric, names must be a single integer.");
    names <- form3names(names,items);
}
if (!all(outer(items,names,"!="))) {
    erreur(list(gn@nom,names),"Some names for new nodes already exists!");
}
nbnn <- length(names);
# about the new positions to give
if (!isvide(pos)) {
    if (!is.matrix(pos)) {
        erreur(pos,"When non null, 'pos' must be a matrix!");
    }
    if((nrow(pos) != nbnn) | !(ncol(pos) %in% c(2,3))) {
        erreur(dim(pos),"When a matrix, pos must have 2 or 3 columns",
               "and a number of rows identical to the number",
               "of nodes to add");
    }
}
###
### adapting the /gn/
#
# completing the nodes
for (nn in names) { gn@nom@x[[nn]] <- ""; }
#
# completing the arcs
gn@arc@nbn <- length(gn2item(gn,check=FALSE));
#
# completing the positions
if (is.null(pos)) {
    angles <- (1:nbnn)*2*pi/nbnn;
    xr <- range(gn@pos@posi[,1]);
    yr <- range(gn@pos@posi[,2]);
    cen <- c(mean(xr),mean(yr));
    rad <- c(diff(xr),diff(yr))/2;
    pos <- cbind(rad[1]*cos(angles),rad[2]*sin(angles)) + 
           outer(rep(1,nbnn),cen,"*");
}
if (ncol(pos == 2)) { pos <- cbind(pos,rep(0,nbnn),rep(0,nbnn));
} else { pos <- cbind(pos,rep(0,nbnn));}
gn@pos@posi <- rbind(gn@pos@posi,pos);
# returning
gn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
ends8gn <- function(gn)
#TITLE (gn) returns the root and leaf nodes/variables of a gn
#DESCRIPTION
# From agn structure, the set of nodes/variables
# is partitioned in four classes:\cr
# (i) the root and leaf nodes/variables,\cr
# (ii) the root but not leaf nodes/variables,\cr
# (iii) the leaf but not root nodes/variables,\cr
# (iv) the remaining nodes/variables.\cr
# Of course, some of the classes can be empty.
#DETAILS No check about \code{gn}
#KEYWORDS
#PKEYWORDS bn gn
#INPUTS
#{gn}<<The gn.>>
#[INPUTS]
#VALUE
# a list of four named numeric vectors with the iin
# of the corresponding nodes/variables and the names as names.
# The four items are $RaL (root and leaf), $RwL (root
# but not leaf), $LwR (leaf but not root) and $rema (the
# remaining nodes/variables).
#EXAMPLE
# rsgn3k("RESET"); # needed only for R checking, to be forgotten
# ends8gn(rbsb.gg1);
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_01
#REVISED 09_10_16
#--------------------------------------------
{
# determining the number of nodes/variables
nbn <- nbnv(gn,gn@item);
# preparing the resulting list
res <- vector("list",4);
names(res) <- c("RaL","RwL","LwR","rema");
for (i in 1:4) { res[[i]] <- numeric(0);}
# getting the parentship
pare <- neighbours8gn(gn,"parents");
chil <- neighbours8gn(gn,"children");
# filling the list
par <- chi <- numeric(nbn);
for (nn in sj(nbn)) {
    par <- length(pare[[nn]]);
    chi <- length(chil[[nn]]);
    if (par == 0) {
        if (chi == 0) {
            res$RaL <- c(res$RaL,nn);
        } else {
            res$RwL <- c(res$RwL,nn);
        }
    } else {
        if (chi == 0) {
            res$LwR <- c(res$LwR,nn);
        } else {
            res$rema <- c(res$rema,nn);
        }
    }
}
# adding the names
nana <- gn2item(gn);
for (i in 1:4) { names(res[[i]]) <- nana[res[[i]]];}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
neighbours8gn <- function(gn,what="parents")
#TITLE (gn) returns the direct parents (or children) of a gn nodes
#DESCRIPTION
# Returns the parents/children of all nodes (or all variables
# according to \code{gn@item}) of 'gn'.
#DETAILS
# Parents means direct parents; children means direct children
#KEYWORDS
#PKEYWORDS genealogy
#INPUTS
#{gn}<<the gn object.>>
#[INPUTS]
#{what} << either \code{parents} or \code{children} accordingly.>>
#VALUE
# A list of character vectors containing the names of the parents,
# each component of the list is associate to an item (node or
# variable). When there is no parent, character(0) is returned.
#EXAMPLE
# rsgn3k("RESET"); # needed only for R checking, to be forgotten
# neighbours8gn(rbsb.gg1,"parents");
# neighbours8gn(rbsb.gg1,"children");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_10_16
#REVISED 09_10_16
#--------------------------------------------
{
# checking
if (rbsb.mck) {valid8gn(gn);}
if (!expr3present(what,c("parents","children"))) {
    erreur(what,"'what' must be 'parents' or 'children'");
}
# getting the parents or children
nbn <- gn@arc@nbn;
if (nbn==0) { return(rbsb.lis0); }
res <- vector("list",nbn);
nnom <- gn2item(gn);
for (ii in sj(nbn)) { res[[ii]] <- character(0);}
for (ii in sj(nrow(gn@arc@fle))) {
    chi <- gn@arc@fle[ii,2];
    chn <- nnom[chi];
    pai <- gn@arc@fle[ii,1];
    pan <- nnom[pai];
    #
    if (expr3present(what,"parents")) {
        res[[chi]] <- c(res[[chi]],pan);
    } else {
        res[[pai]] <- c(res[[pai]],chn);
    }
}
# naming the result
names(res) <- nnom;
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
