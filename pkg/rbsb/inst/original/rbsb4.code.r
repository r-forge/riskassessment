 
###########################################
###########################################
########
#((((((( NEW S4 CLASS empirical
########
###########################################
###########################################

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8empirical <- function(object)
#TITLE  checks an /empirical/
#DESCRIPTION
#   This function checks an /empirical/ object
#DETAILS
# It is the validity method for /empirical/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The empirical object to be validated.>>
#[INPUTS]
#VALUE
# TRUE when the object seems acceptable
# else a character describing the error(s)
#EXAMPLE
# rbsb3k("RESET"); # for R checking compliance (useless)
# valid8empirical(rbsb.epi1);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_10
#REVISED 11_01_13
#--------------------------------------------
{
    if (class(object)!="empirical") {
        erreur(NULL,paste("This object is not of class 'empirical' but '",class(object),"'",sep=""));
    }
    #
    rrr <- valid8des(object@des);
    if (identical(rrr,TRUE)) {
      res <- character(0);
    } else {
      res <- rrr;
    }
    #
    res <- c(res,check4tyle(object@repa,"numeric",2,c(0,Inf),message="@repa not accepted"));
    if (sum(object@repa) <= 0) {
      res <- c(res,"@repa cannot be c(0,0), a 'true' probability is wanted");
    }
    #
    res <- c(res,check4tyle(object@sup,"numeric",2,message="@repa not accepted"));
    if (diff(object@sup)<0) {
      res <- c(res,"The @sup must defined an interval");
    }
    #
    res <- c(res,check4tyle(object@xw,"matrix",-1,message="@xw not accepted"));
    if (is.matrix(object@xw)) {
      if (object@repa[1] > 0) {
        if (nrow(object@xw) == 0) {
          res <- c(res,"When @repa[1]>0, @xw cannot be empty");
        }
      }
      if (ncol(object@xw)!=2) {
        res <- c(res,"@xw must have two columns");
      }
      if (any(object@xw[,2]<0)) {
        res <- c(res,"@xw[,2] must be non negative weights");
      }
    }
    if (any(is.na(object@xw)) |
        any(is.infinite(object@xw))) {
      res <- c(res,"All values of @xw must be defined");
    }
    #
    res <- c(res,check4tyle(object@xy,"matrix",-1,message="@xy not accepted"));
    if (is.matrix(object@xy)) {
      if (object@repa[2] > 0) {
        if (nrow(object@xy) < 3) {
          res <- c(res,"When @repa[2]>0, @xy must have at least 3 columns");
        }
      }
      if (ncol(object@xy)!=2) {
        res <- c(res,"@xy must have two columns");
      }
      if (any(diff(object@xy[,1])<0)) {
        res <- c(res,"@xy[,1] must be non decreasing");
      }
      if (any(object@xy[,2]<0)) {
        res <- c(res,"@xy[,2] must be non negative weights");
      }
      if (nrow(object@xy)>1) { if(any(object@xy[c(1,nrow(object@xy)),2]!=0)) {
        res <- c(res,"First and Last element of @xy must be null");
      }}
    }
    if (any(is.na(object@xy)) |
        any(is.infinite(object@xy))) {
      res <- c(res,"All values of @xy must be defined");
    }
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=rbsb.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


###########################################
# description
setClass("empirical", representation(
    des ="des",        # description of the object
    sup ="numeric",    # the support for the distribution
                       # when some values of 'xw' and 'xy'
                       # are outside the support (the bounds
                       #  are included), they are eliminated.
    repa="numeric",    # repartition between the discrete [1]
                       #    and continuous [2] parts
                       #    irrespectively of the support, this
                       #    means that the repartition is applied
                       #    before truncating.
    xw="matrix",     # matrix with two columns (x,p) describing
                       #    the discrete density
    xy="matrix"      # matrix of two columns (x,y) describing
                       #    the continuous density
                         ),
                      prototype=list(
    des=new("des",name="empi"),
    sup=c(0,10),
    repa=c(0.3,0.7),
    xw=matrix(c(1:3,rep(1,3)),3),
    xy=matrix(c(0,0,1,0,1,0),3)
                         ),
                      validity=valid8empirical
        );
#

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8empirical <- function(x,...,what="rswc",empha=0)
#TITLE  prints an /empirical/
#DESCRIPTION
#   This function prints in an /empirical/ object.
#DETAILS
# Global constant \code{rbsb.mwi} is used to justify the paragraphes.
# It is the specific print for /empirical/ objects.
#KEYWORDS classes
#INPUTS
#{x} <<The \code{empirical} object to print.>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.>>
#{what} <<the fields to print:
#          \code{a}=all fields,
#          \code{d}=description,
#          \code{s}=support,
#          \code{r}=repartition,
#          \code{w}=discrete part,
#          \code{c}=continuous part.>>
#{empha} <<Emphasize level of printing;
#           between 0 and 10>>
#VALUE
# nothing but a print is performed
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
# For the moment, only empha==0 is done
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_10
#REVISED 11_01_10
#--------------------------------------------
{
if (rbsb.mck) {
  # some checking
  che <- valid8empirical(x);
  if (!identical(che,TRUE)) {
      erreur(che,"/empirical/ is not valid");
  }
}
empha <- round(max(0,min(10,empha)));
#
empha <- 0;
what <- tolower(what);
if (expr3present("a",what)) { what <- "dsrwc";}
# preparing the constant according to the emphasis
if (empha == 0) {
}
dimnames(x@xw) <- list(NULL,c("X","W"));
dimnames(x@xy) <- list(NULL,c("X","Y"));
#
# printing
if (expr3present("d",what)) {
  print8des(x@des,empha=empha);
}
#
if (expr3present("s",what)) {
  form3titre("Support:",empha);
  form3line(0,wid=1);
  cat("       From",x@sup[1],"to",x@sup[2],"\n");
}
#
if (expr3present("r",what)) {
  form3titre("Repartition:",empha);
  form3line(0,wid=1);
  cat("      ",x@repa[1],"for discrete and",x@repa[2],"for continuous\n");
}
#
if (expr3present("w",what)) {
  if (x@repa[1] > 0) {
    form3titre("The discrete part:",empha);
    form3line(0,wid=1);
    print(x@xw);
  } else {
    form3titre("No discrete part",empha);
    form3line(0,wid=1);
  }
}
#
if (expr3present("c",what)) {
  if (x@repa[2] > 0) {
    form3titre("The continuous part:",empha);
    form3line(0,wid=1);
    print(x@xy);
  } else {
    form3titre("No continuous part",empha);
    form3line(0,wid=1);
  }
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
plot8empirical <- function(x,y="useless",...,
                           what="dc",
                           ticks="s",
                           xli=NULL,ylid=NULL,ylic=NULL,main=NULL)
#TITLE  plots an /empirical/
#DESCRIPTION
#   This function plots an /empirical/ object.
#DETAILS
#KEYWORDS classes
#INPUTS
#{x} <<The \code{empirical} object to plot.>>
#[INPUTS]
#{y} << For compatibility with the generic plot function. Useless here.>
#{\dots} <<Further arguments to be passed to the plot function.>>
#{what} <<What to plot? \code{d} for density and \code{c} for cumulative.>>
#{ticks} <<Markers of the discontinuity of the function. When numerical,
#          0 means no ticks if not the height of the ticks.
#          When character, \code{s} means until the curve,
#          \code{S} the same but ticks will have a minimum length, if not nothing.>>
#{xli} <<possible imposed \code{xlim} for the density and the cumulative>>
#{ylid} <<possible imposed \code{ylim} for the density>>
#{ylic} <<possible imposed \code{ylim} for the cumulative>>
#{main} <<Some prepared title for the plot.>>
#VALUE
# nothing but a plot is performed
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_10
#REVISED 11_01_21
#--------------------------------------------
{
if (rbsb.mck) {
  # some checking
  che <- valid8empirical(x);
  if (!identical(che,TRUE)) {
      erreur(che,"/empirical/ is not valid");
  }
}
# normalizing
x <- normalize8empirical(x);
# looking for the frame
if (is.null(xli)) {
  xli <- numeric(0);
  if (x@repa[1]>0) { xli <- c(xli,x@xw[,1]);}
  if (x@repa[2]>0) { xli <- c(xli,x@xy[,1]);}
  if (length(xli)==0) { return();}
  xli <- geom3lmargin(xli);
} else {
  check4tyle(xli,"numeric",2,message="Bad 'xli'");
}
if (is.null(ylid)) {
  ylid <- numeric(0);
  if (x@repa[1]>0) { ylid <- c(ylid,x@xw[,2]);}
  if (x@repa[2]>0) { ylid <- c(ylid,x@xy[,2]);}
  ylid <- range(0,ylid);
} else {
  check4tyle(ylid,"numeric",2,message="Bad 'ylid'");
}
if (is.null(ylic)) {
  ylic <- 0:1;
} else {
  check4tyle(ylic,"numeric",2,message="Bad 'ylic'");
}
if (is.null(main)) {
  titd <- paste(x@des@name,"(density)");
  titc <- paste(x@des@name,"(cumulative)");
} else {
  titd <- main;
  titc <- main;
}
#
#
### THE DENSITY
#
if (expr3present("d",what)) {
  plot(0,0,
       xlab="values",ylab="density",
       xlim=xli,ylim=ylid,main=titd,type="n");
  abline(h=0);
  #
  # drawing the continuous part
  if (x@repa[2] > 0) {
    NN <- 1000;
    xxx <- seq(xli[1],xli[2],length=NN);
    yyy <- continuous2density(x@xy,xxx);
    polygon(xxx,yyy,density=NA,col="lightgrey")
    lines(xxx,yyy);
  }                        
  #
  # drawing the discrete part
  if (x@repa[1] > 0) {
    for (ww in bc(nrow(x@xw))) {
      xx <- x@xw[ww,1];
      yy <- x@xw[ww,2]
      lines(c(xx,xx),c(0,yy),lwd=5);
    }
  }
}
#
#
### THE CUMULATIVE
#
if (expr3present("c",what)) {
  plot(0,0,
       xlab="values",ylab="probability",
       xlim=xli,ylim=ylic,main=titc,type="n");
  abline(h=c(0,1));
  #
  # computing the cumulative
  N <- 1000;
  ran <- range5empirical(x)$remp;
  if (diff(ran)==0) {
    # to tackle properly Dirac distributions
    ran <- (xli+ran)/2;
  }
  xxx <- seq(ran[1],ran[2],length=N);
  cumu <- cbind(xxx,pempirical(xxx,x));
  # adding a starting point when the discrete part
  # has got the lowest value
  if (cumu[1,2] > 0) {
    cumu <- rbind(c(cumu[1,1],0),cumu);
  }
  #
  # drawing it
  lines(cumu,lwd=2);
  #
  # adding the ticks
  xt <- unique(c(x@xw[,1],x@xy[,1]));
  if (is.numeric(ticks)) {
    if (ticks > 0) {
      for (xx in xt) {
        lines(rep(xx,2),c(0,min(1,ticks)),lty=3);
      }
    }   
  }
  if (is.character(ticks)) {
    if (tolower(ticks)=="s") {
      for (xx in xt) {
        ix <- which(min(abs(xx-cumu[,1]))==
                    abs(xx-cumu[,1])
                   )[1];
        xxx <- cumu[ix,1]; yyy <- cumu[ix,2];
        if (ticks=="S") { yyy <- max(0.05,yyy);}
        lines(rep(xxx,2),c(0,yyy),lty=3);
      }
    }   
  }
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#
#

setMethod("print",signature(x = "empirical"), print8empirical);

setMethod( "plot",signature(x = "empirical"), plot8empirical);

#
#
#

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
normalize8empirical <- function(empi)
#TITLE  normalizes an /empirical/
#DESCRIPTION
#   This function normalizes an /empirical/ object,
# that is returns the object with weight transformed  
# into probabilities (taking care of the two parts)
#DETAILS
# Also a sorting of the discrete values is performed
# and multiple identical values are merged. Zero values
# are eliminated as much as possible. Also are values outside
# the proposed support.
#KEYWORDS classes
#INPUTS
#{empi} <<The \code{empirical} object to normalize.>>
#[INPUTS]
#VALUE
# the transformed /empirical/
#EXAMPLE
# rbsb3k("RESET"); # for R checking compliance (useless)
# print(normalize8empirical(new("empirical")));
# e1 <- new("empirical",des=char2des("e1"),sup=c(0,10),repa=c(10,0),xw=matrix(c(1,2,1,2,1,1:5),ncol=2));
# print(e1);
# print(normalize8empirical(e1));
# e2 <- new("empirical",des=char2des("e2"),sup=c(2.5,3.5),repa=c(0,5),xy=matrix(c(1:5,0,1,1,1,0),ncol=2));
# print(e2);
# print(normalize8empirical(e2));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_10
#REVISED 11_01_14
#--------------------------------------------
{
# some checking
if (rbsb.mck) {
  che <- valid8empirical(empi);
  if (!identical(che,TRUE)) {
    erreur(che,"/empirical/ is not valid");
  }
}
res <- empi;
# the repartition
res@repa <- res@repa / sum(res@repa);
#
## the discrete part
if (res@repa[1] == 0) {
  res@xw <- matrix(0,0,2);
} else {
  # eliminating the zero y values
  ok <- which(res@xw[,2]>0);
  res@xw <- res@xw[ok,,drop=FALSE];
  # putting things in order
  oo <- order(res@xw[,1]);
  res@xw <- res@xw[oo,,drop=FALSE];
  # merging identical x values
  dd <- diff(res@xw[,1]);
  for (gg in rev(bf(dd))) {
    if (dd[gg] == 0) {
      # gg and gg+1 are identical
      ## 1: merging
      res@xw[gg,2] <- res@xw[gg,2] + res@xw[gg+1,2];
      ## 2: removing
      res@xw <- res@xw[-(gg+1),,drop=FALSE];
    }
  }
  res@xw[,2] <- res@xw[,2]/sum(res@xw[,2]) * res@repa[1];
}
#
## the continuous part
if (res@repa[2] == 0) {
  res@xy <- matrix(0,0,2);
} else {
  # computing first the sum
  my <- (res@xy[-1,2] + res@xy[-nrow(res@xy),2]) / 2;
  susu <- sum(diff(res@xy[,1]) * my);
  res@xy[,2] <- res@xy[,2]/susu * res@repa[2];
}
#
## removing values outside the support
ra <- range(res@xw[,1],res@xy[,1]);
eli <- (ra[1] < res@sup[1]) | (res@sup[2] < ra[2]);
if (eli) {
  if (res@repa[1]>0) {
    # discrete part
    ok <- which((res@sup[1] <= res@xw[,1]) &
                (res@xw[,1] <= res@sup[2]));
    res@xw <- res@xw[ok,,drop=FALSE];
  }
  if (res@repa[2]>0) {
    # continuous part
    rem <- numeric(0);
    # continuous part on the lower side
    cb <- sum(res@xy[,1] < res@sup[1]);
    if (cb > 0) {
      # something has to be removed
      if (cb == nrow(res@xy)) {
        # everything
        res@xy <- matrix(0,0,2);
      } else {
        # creating the new lower bound
        if (res@xy[cb,2] > 0) {
          # a new value has to be given
          res@xy[cb,2] <- res@xy[cb,2] + diff(res@xy[cb+(0:1),2]) *
                                         (res@sup[1]-res@xy[cb,1])/
                                         diff(res@xy[cb+(0:1),1]);
        }
        res@xy[cb,1] <- res@sup[1];
        res@xy[ 1,1] <- res@sup[1];
        # something to leave out
        if (cb > 2) {
          rem <- c(rem,2:(cb-1));
        }
      }
    }
    # continuous part on the upper side
    cb <- sum(res@xy[,1] > res@sup[2]);
    if (cb > 0) {
      # something has to be removed
      if (cb == nrow(res@xy)) {
        # everything
        res@xy <- matrix(0,0,2);
      } else {
        nr <- nrow(res@xy)-cb+1;
        # creating the new upper bound
        if (res@xy[nr,2] > 0) {
          # a new value has to be given
          res@xy[nr,2] <- res@xy[nr-1,2] + diff(res@xy[nr-1+(0:1),2]) *
                                         (res@xy[nr+1,1]-res@sup[2])/
                                         diff(res@xy[nr-1+(0:1),1]);
        }
        res@xy[nr     ,1] <- res@sup[2];
        res@xy[nr+cb-1,1] <- res@sup[2];
        # something to leave out
        if (cb > 2) {
          rem <- c(rem,nr+(1:(cb-2)));
        }
      }
    }
    # removing the useless row if any
    if (length(rem)>0) {
      res@xy <- res@xy[-rem,];
    }
  }
}
if (res@repa[2]>0) {
  #
  ## removing useless cordinates for the continuous part
  dd <- function(x) {
    x <- diff(x)==0;
    x <- (1-x)*bf(x);
    x <- diff(x)==0;
    1+which(x);
  }
  uu <- dd(res@xy[,2]);
  while (length(uu) > 0) {
    res@xy <- res@xy[-uu,,drop=FALSE];
    uu <- dd(res@xy[,2]);
  }
}
#
# in case of elimination a new normalization is necessary
if (eli) {
  res <- Recall(res);
}
#
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
range5empirical <- function(empi)
#TITLE  returns the ranges of an /empirical/
#DESCRIPTION
#   Returns the different supports of an /empirical/
# that is the minimum segment where stand
# the variation of the random variable,
# taking into account (or not) the \code{@sup} slot.
# See the section 'value' for details.
#DETAILS
# \code{empi} is not normalized, so the range
# can exceed the \code{@sup}.
#KEYWORDS 
#INPUTS
#{empi} <<The \code{empirical} to consider.>>
#[INPUTS]
#VALUE
# A list with four components:
# \code{$rcon}: the range of the continuous part
# (code{c(NA,NA)} if it does not exist).
# \code{$rdis}: the set of possible values for the
# discrete part (code{numeric(0)} if it does not exist).
# \code{$remp}: the range of both parts not taking into
# account the slot \code{@sup}.
# \code{$rang}: the range of both parts taking into
# account the slot \code{@sup}, identical to \code{$remp}
# after normalisation.
#EXAMPLE
# rbsb3k("RESET"); # for R checking compliance (useless)
# range5empirical(new("empirical"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_18
#REVISED 11_01_18
#--------------------------------------------
{
if (rbsb.mck) {
  # some checking
  che <- valid8empirical(empi);
  if (!identical(che,TRUE)) {
    erreur(che,"/empirical/ is not valid");
  }
}
#
# continuous part
if (empi@repa[2]>0) {
  rcon <- range(empi@xy[,1]);
} else {
  rcon <- c(NA,NA);
}
#
# discrete part
if (empi@repa[1]>0) {
  rdis <- unique(empi@xw[,1]);
} else {
  rdis <- numeric(0);
}
#
# global ranges
remp <- range(c(rcon,rdis),na.rm=TRUE);
rang <- range(c(remp,empi@sup));
#
# returning
list(rcon=rcon,rdis=rdis,remp=remp,rang=rang);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
continuous2density <- function(xyco,x,dupli="m")
#TITLE  density of the continuous part
#DESCRIPTION
#   This function returns the density
# of a continous part of an /empirical/ for the
# values of \code{x}.
#DETAILS
# Values are not normalized, to obtain them
# just use \code{normalize8empirical} before
# extracting the continuous part.
#KEYWORDS 
#INPUTS
#{xyco} <<The \code{empirical@xy} to consider.>>
#{x} << Values for which the values must be computed.>>
#[INPUTS]
#{dupli} << When a \code{x} value is exactly in a
#           vertical part of the density. Several choices
#           are possible monitored by \code{dupli}.
#           If "d", the point will be duplicated,
#              "1", the first value will be chosen,
#              "2", the second value will be chosen,
#              "m", the mean value will be chosen.>>
#VALUE
# The resulting values. When \code{dupli=="d"}, they
# are not returned as a vector but as a matrix the two columns
# of which are the possibly duplicated \code{x} and
# the resulting values.
#EXAMPLE
# rbsb3k("RESET"); # for R checking compliance (useless)
# xx <- seq(0,6,0.3);
# xy <- matrix(c(1:5,0,1,3,3,0),5);
# cbind(xx,continuous2density(xy,xx));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_10
#REVISED 11_01_13
#--------------------------------------------
{
# some checking
if (rbsb.mck) {
  veri <- new("empirical",repa=0:1,xy=xyco,sup=range(xyco));
  che <- valid8empirical(veri);
  if (!identical(che,TRUE)) {
      erreur(che,"This 'xyco' is not valid");
  }
  #
  check4tyle(dupli,"character",1,c("d","1","2","m"),"'dupli' not accepted");
}
#
# degenerate cases
if (nrow(xyco) == 0) {
  return(rep(0,length(x)));
}
if (length(x) == 0) { return(numeric(0));}
#
ix <- findInterval(x,xyco[,1]);
#
res <- rep(0,length(x));
#
dup <- matrix(NA,0,3);
for (ii in bc(nrow(xyco)-1)) {
  uu <- which(ix == ii);
  if (length(uu)>0) {
    ma <- xyco[ii+(0:1),,drop=FALSE];
    if (ma[1,1]==ma[2,1]) {
      # the jumping case (vertical segment)
      res[uu] <- ma[2,2];
    } else {
      if (ma[1,2]==ma[2,2]) {
        # the horizontal case (uniform part)
        res[uu] <- mean(ma[,2]);
        if (dupli=="1") {res[uu] <- ma[1,2];}
        if (dupli=="2") {res[uu] <- ma[2,2];}
        if (dupli=="d") {
          res[uu] <- NA;
          dup <- rbind(dup,cbind(uu,matrix(ma[,2],length(uu),2,byrow=TRUE)));
        }
      } else {
        # the linearly [in|de]creasing case
        res[uu] <- ma[1,2] + diff(ma[,2]) *
                             (x[uu]-ma[1,1])/diff(ma[,1]);
      }
    }
  }
}
#
# the delicate case of duplication
if (dupli=="d") {
  res <- cbind(x,res);
  if (nrow(dup) > 0) {
    dup <- dup[order(dup[,1]),];
    res <- rbind(res,matrix(NA,nrow(dup),2));
    nn <- nrow(res);
    for (dd in rev(bc(nrow(dup)))) {
      ii <- dup[dd,1];
      i1 <- bd(ii,nn-1);
      res[i1+1,] <- res[i1,] ;
      res[ii,] <- dup[dd,2:3];
    }
  }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
discrete2density <- function(xwdi,x)
#TITLE  density of the discrete part
#DESCRIPTION
#   This function returns the density
# of a discrete part of an /empirical/ for the
# values of \code{x}.
#DETAILS
# Values are not normalized, to obtain them
# just use \code{normalize8empirical} before
# extracting the continuous part.
#KEYWORDS 
#INPUTS
#{xwdi} <<The \code{empirical@xw} to consider.>>
#{x} << Values for which the values must be computed.>>
#[INPUTS]
#VALUE
# The resulting values. If the \code{xwdi}
# was not normalized, possible multiple defined
# weight are cumulated.
#EXAMPLE
# rbsb3k("RESET"); # for R checking compliance (useless)
# xx <- c(4,6,3);
# xw <- matrix(c(1:3,3:4,0,1,3,3,0),5);
# cbind(xx,ww=discrete2density(xw,xx));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_13
#REVISED 11_01_13
#--------------------------------------------
{
# some checking
if (rbsb.mck) {
  veri <- new("empirical",repa=1:0,xw=xwdi,sup=range(xwdi));
  che <- valid8empirical(veri);
  if (!identical(che,TRUE)) {
      erreur(che,"This 'xwdi' is not valid");
  }
}
#
# degenerate cases
if (nrow(xwdi) == 0) {
  return(rep(0,length(x)));
}
if (length(x) == 0) { return(numeric(0));}
#
# summing first
xx <- unique(xwdi[,1]);
if (nrow(xwdi) > length(xx)) {
  # some cumulation has to be done
  xxw <- cbind(xx,0);
  for (ix in bf(xx)) {
    xxw[ix,2] <- sum(xwdi[xwdi[,1]==xx[ix],2]);
  }
  xwdi <- xxw;
}
# computing second
res <- rep(0,length(x));
for (ix in bc(nrow(xwdi))) {
  res[x==xwdi[ix,1]] <- xwdi[ix,2];
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
continuous2cumulative <- function(xyco,x,norma=FALSE)
#TITLE  cumulative of the continuous part
#DESCRIPTION
#   This function returns the values of the cumulative
# of a continous part of an /empirical/ for the
# values of \code{x}.
#DETAILS
# Values are not normalized, to obtain them
# just use \code{normalize8empirical} before
# extracting the continuous part.
#KEYWORDS 
#INPUTS
#{xyco} <<The \code{empirical@xy} to consider.>>
#{x} << Values for which the values must be computed.>>
#[INPUTS]
#{norma} <<Indicates if a normalization to one must be done.>>
#VALUE
# The resulting values.
#EXAMPLE
# rbsb3k("RESET"); # for R checking compliance (useless)
# xx <- seq(0,6,0.3);
# xy <- matrix(c(1:5,0,1,3,3,0),5);
# cbind(xx,continuous2cumulative(xy,xx));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_14
#REVISED 11_01_21
#--------------------------------------------
{
# some checking
if (rbsb.mck) {
  veri <- new("empirical",repa=0:1,xy=xyco,sup=range(xyco[,1]));
  che <- valid8empirical(veri);
  if (!identical(che,TRUE)) {
      erreur(che,"This 'xyco' is not valid");
  }
}
#
# degenerate cases
if (nrow(xyco) == 0) {
  return(rep(0,length(x)));
}
if (length(x) == 0) { return(numeric(0));}
## computing some value for each interval
# number of intevals
nbi <- nrow(xyco)-1;
# slope of each interval (corrected for null increases of x)
ra <- diff(xyco[,2]) / diff(xyco[,1]);
ra[is.infinite(ra)] <- 0; 
# cumulative at the beginning of each interval
cu <- cumsum(diff(xyco[,1])*(xyco[-(nbi+1),2]+xyco[-1,2])/2);
cu <- c(0,cu[-nbi]);
## adding an x-value for the right outside elements
x <- c(x,max(xyco[,1]));
## affectation to the intervals
ix <- findInterval(x,xyco[,1]);
# upper bounds must be reaffected
ix[ix==nbi+1] <- nbi
## initialization of the result
res <- rep(0,length(x));
## dealing for each interval in turn
for (ii in bc(nbi)) {
  jj <- which(ix==ii);
  res[jj] <- cu[ii] + (x[jj]-xyco[ii,1])*
                      (xyco[ii,2]+(x[jj]-xyco[ii,1])*ra[ii]/2);
}
## putting right outside elements to the maximum
res[x>=max(xyco[,1])] <- res[length(res)];
if (norma) {
  ## normalizing
  res <- res / res[length(res)];
}
# removing the added element
res <- res[-length(res)];
#
## returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
discrete2cumulative <- function(xwdi,x,norma=FALSE)
#TITLE  cumulative of the discrete part
#DESCRIPTION
#   This function returns the values of the cumulative
# of a discrete part of an /empirical/ for the
# values of \code{x}.
#DETAILS
# Values are not normalized, to obtain them
# just use \code{normalize8empirical} before
# extracting the continuous part.
#KEYWORDS 
#INPUTS
#{xwdi} <<The \code{empirical@xw} to consider.>>
#{x} << Values for which the values must be computed.>>
#[INPUTS]
#{norma} <<Indicates if a normalization to one must be done.>>
#VALUE
# The resulting values. If the \code{xwdi}
# was not normalized, possible multiple defined
# weight are cumulated.
#EXAMPLE
# rbsb3k("RESET"); # for R checking compliance (useless)
# xx <- c(4,6,3);
# xw <- matrix(c(1:3,3:4,0,1,3,3,0),5);
# cbind(xx,ww=discrete2cumulative(xw,xx));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_14
#REVISED 11_01_14
#--------------------------------------------
{
# some checking
if (rbsb.mck) {
  veri <- new("empirical",repa=1:0,xw=xwdi,sup=range(xwdi));
  che <- valid8empirical(veri);
  if (!identical(che,TRUE)) {
      erreur(che,"This 'xwdi' is not valid");
  }
}
#
# degenerate cases
if (nrow(xwdi) == 0) {
  return(rep(0,length(x)));
}
if (length(x) == 0) { return(numeric(0));}
#
# summing first
xx <- unique(xwdi[,1]);
if (nrow(xwdi) > length(xx)) {
  # some cumulation has to be done
  xxw <- cbind(xx,0);
  for (ix in bf(xx)) {
    xxw[ix,2] <- sum(xwdi[xwdi[,1]==xx[ix],2]);
  }
  xwdi <- xxw;
}
# sorting second
oo <- order(xwdi[,1]);
xwdi <- xwdi[oo,,drop=FALSE];
# computing third
nbi <- nrow(xwdi)-1;
# adding an element for the right outside elements
x <- c(x,xwdi[nbi+1,1]);
# finding the affectations
ix <- findInterval(x,xwdi[,1]);
# initializing
res <- rep(0,length(x));
for (ii in bc(nbi+1)) {
  jj <- (ix==ii);
  res[jj] <- sum(xwdi[bc(ii),2]);
}
## putting right outside elements to the maximum
res[x>=max(xwdi[,1])] <- res[length(res)];
if (norma) {
  ## normalizing 
  res <- res[-length(res)] / res[length(res)];
}
# removing the added element
res <- res[-length(x)];
#
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
density2cumulative <- function(empi,N=1000)
#TITLE  returns the cumulative of an /empirical/
#DESCRIPTION
#   This function returns the cumulative probability
# distribution function of an /empirical/ object.
# That is returns the coordinates of points
# defining this function. Be aware that the calculus is
# made numerically, which is not the case for \code{pempirical},
# so better use it.
#DETAILS
# \code{N} must be understood
# as a simple way to give the level of precision the user
# wants to see apply for the returned curve.
#KEYWORDS
#INPUTS
#{empi} <<The \code{empirical} object to consider.>>
#[INPUTS]
#{N} <<The number of steps for the curve all
#      around the range of the random variable.>>
#VALUE
# A matrix with two columns \code{x,y}.
#EXAMPLE
# rbsb3k("RESET"); # for R checking compliance (useless)
# density2cumulative(new("empirical"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_10
#REVISED 11_01_13
#--------------------------------------------
{
# some checking
if (rbsb.mck) {
  che <- valid8empirical(empi);
  if (!identical(che,TRUE)) {
    erreur(che,"/empirical/ is not valid");
  }
}
# degenerate case
if (sum(empi@repa)==0) { return();}
# computing the range
ra <- numeric(0);
if (empi@repa[1] > 0) {ra <- range(ra,empi@xw);}
if (empi@repa[2] > 0) {ra <- range(ra,empi@xy);}
# computing the points
N <- max(N,10);
xx <- seq(ra[1],ra[2],length=N);
if (empi@repa[1]>0) {
  xx <- sort(unique(c(xx,empi@xw[,1])));
}
# easying by the normalisation
empi <- normalize8empirical(empi);
# computing the values due to the continuous part
if (empi@repa[2] > 0) {
  yy <- cumsum(continuous2density(empi@xy,xx));
  yy <- yy / max(yy) * empi@repa[2];
} else {
  yy <- rep(0,length(xx));
}
# adding the jump for the discrete part
if (empi@repa[1]>0) {for (ii in bc(nrow(empi@xw))) {
  ou <- (xx >= empi@xw[ii,1]);
  yy[ou] <- yy[ou] + empi@xw[ii,2];
}}
# returning
matrix(cbind(xx,yy),ncol=2,dimnames=list(NULL,c("X","Y")));
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
solve2degree <- function(y,ky,kx2,kx,kk,dx=NULL,x0=NULL)
#TITLE  solves a 'generalized' degree two polynomial
#DESCRIPTION
#   This function returns the root (or two roots) of 
# the equation \code{ky*y + kx2*x^2 + kx*x + kk = 0}.
# When \code{dx} is not null, it is supposed to give
# the interval where the root lies, in that case only
# one root is returned.\cr
# The first parameter can be a vector of any
# length and all computations are vectorized.\cr
# Only real roots are considered.
#DETAILS
# When \code{dx} is defined  only one root is returned,
# belonging to the interval; if it is not possible (root(s)
# exist(s) and do(es) not comply, then a fatal error
# is issued.\cr
# When every real number complies with the equation, according
# to available arguments, the returning value is \code{x0},
# \code{mean(dx)} or \code{0}.
# When \code{is.null(dx)} either one or two roots is 
# returned with \code{NA} when the solution is complex.
#KEYWORDS
#INPUTS
#{y} <<Vector of values for which the equation must be satisfied.>>
#{ky} <<Coefficient for \code{y}.>>
#{kx2} <<Coefficient for \code{x^2}.>>
#{kx} <<Coefficient for \code{x}.>>
#{kk} <<Constant coefficient.>>
#[INPUTS]
#{dx} <<\code{NULL} or the interval (\code{numeric(2)}) for the roots.>>
#{x0} <<\code{NULL} or a proposal in case of indetermination.>>
#VALUE
# A matrix having one or two columns according to the values of
# \code{ky,kx2,kx,kk,dx}.
#EXAMPLE
# rbsb3k("RESET"); # for R checking compliance (useless)
# solve2degree(1:10, 1,1,0,-20);
# solve2degree(   3,-1,1,1,  1);
# solve2degree(   3,-1,1,1,  1,c(0.5,1.5));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_18
#REVISED 11_01_21
#--------------------------------------------
{
# some checking
if (rbsb.mck) {
  check4tyle(y,"numeric",-1,message="solve2degree: non accepted 'y'");
  check4tyle(ky,"numeric",1,message="solve2degree: non accepted 'ky'");
  check4tyle(kx2,"numeric",1,message="solve2degree: non accepted 'kx2'");
  check4tyle(kx,"numeric",1,message="solve2degree: non accepted 'kx'");
  check4tyle(kk,"numeric",1,message="solve2degree: non accepted 'kk'");
  if (!is.null(dx)) {
    check4tyle(dx,"numeric",2,message="solve2degree: non accepted 'dx'");
  }
  if (!is.null(x0)) {
    check4tyle(x0,"numeric",1,message="solve2degree: non accepted 'x0'");
  }
}
# number of equations
ne <- length(y);
# degenerate case
if (ne==0) { return(numeric(0));}
# modifying the constant
kk <- ky*y + kk;
#
# exploring the case
if (kx2==0) {
  # 1rst degree at most
  if (kx==0) {
    # 0 degree
    if (all(kk==0)) {
      # any real is root
      if (!is.null(x0)) {
        res <- matrix(x0,ne,1);
      } else {
        if (!is.null(dx)) {
          res <- matrix(mean(dx),ne,1);
        } else {
          res <- matrix(0,ne,1);
        }
      }
    } else {
      # no root
      res <- matrix(NA,ne,1);
      erreur(list(ky,kx2,kx,kk),"solve2degree: no solution for the proposed equation");
    }
  } else {
    # 1rst degree
    res <- matrix(-kk/kx,ne,1);
  }
} else {
  # 2d degree
  disc <- kx^2 - 4*kx2*kk;
  rro <- (disc >= 0);
  res <- matrix(NA,ne,2);
  res[rro,] <- (-kx + outer(sqrt(disc[rro]),c(-1,1),"*")) / (2*kx2);
  if (!is.null(dx)) {
    ou1 <- (res[rro,1]-dx[1])*(res[rro,1]-dx[2]);
    ou2 <- (res[rro,2]-dx[2])*(res[rro,2]-dx[1]);
    ou <- (ou1 <= ou2);
    res[rro[!ou],1] <- res[rro[!ou],2];
    res <- res[,1,drop=FALSE];
  }
}
#
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
dempirical <- function(x,empi)
#TITLE  returns the density of an /empirical/
#DESCRIPTION
#   This function returns the probability
# density of an /empirical/ object as a list
# with two components: \code{w} for the discrete part
# and \code{y} for the continuous part associated
# to different values of the random variable.
#DETAILS
# the proposed \code{empi} is normalized before
# the computation. So if you don't what that, use
# by yourself the two functions \code{continuous2density}
# and \code{discrete2density}.
#KEYWORDS
#INPUTS
#{x} <<Vector of values for which the density is desired.>>
#{empi} <<The \code{empirical} object to consider.>>
#[INPUTS]
#VALUE
# A list with two components \code{$w,$y} having each the
# same length as \code{x};
#EXAMPLE
# rbsb3k("RESET"); # for R checking compliance (useless)
# dempirical(seq(0,4,0.5),new("empirical"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_10
#REVISED 11_01_13
#--------------------------------------------
{
if (rbsb.mck) {
  # some checking
  che <- valid8empirical(empi);
  if (!identical(che,TRUE)) {
      erreur(che,"/empirical/ is not valid");
  }
}
# degenerate case
if (length(x)==0) { return(list(w=numeric(0),y=numeric(0)));}
# normalizing
empi <- normalize8empirical(empi);
# the continuous part
if (empi@repa[2]>0) {
  yy <- continuous2density(empi@xy,x);
} else {
  yy <- rep(0,length(x));
}
# the discrete part
if (empi@repa[1]>0) {
  ww <- discrete2density(empi@xw,x);
} else {
  ww <- rep(0,length(x));
}
#
# returning
list(w=ww,y=yy);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
pempirical <- function(x,empi)
#TITLE  returns the distribution function of an /empirical/
#DESCRIPTION
#   This function returns the cumulative distribution
# function of an /empirical/ object for different values of
# the random variable.
#DETAILS 
#KEYWORDS
#INPUTS
#{x} <<Vector of values for which the distribution is wanted.>>
#{empi} <<The \code{empirical} object to consider.>>
#[INPUTS]
#VALUE
# A vector with same length as \code{x};
#EXAMPLE
# rbsb3k("RESET"); # for R checking compliance (useless)
# pempirical(seq(0,4,0.5),new("empirical"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_10
#REVISED 11_01_18
#--------------------------------------------
{
if (rbsb.mck) {
  # some checking
  che <- valid8empirical(empi);
  if (!identical(che,TRUE)) {
      erreur(che,"/empirical/ is not valid");
  }
}
# degenerate case
if (length(x)==0) { return(numeric(0));}
# normalizing
empi <- normalize8empirical(empi);
#
# the continuous part
if (empi@repa[2]>0) {
  res <- continuous2cumulative(empi@xy,x,FALSE);
} else {
  res <- rep(0,length(x));
}
# the discrete part
if (empi@repa[1]>0) {
  res <- res + discrete2cumulative(empi@xw,x,FALSE);
}
#
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
qempirical <- function(p,empi)
#TITLE  returns the quantiles of an /empirical/
#DESCRIPTION
#   This function returns the quantiles of
# an /empirical/ object for different probabilities.
#DETAILS 
#KEYWORDS
#INPUTS
#{p} <<Vector of probabilities for which the quantile are wanted.>>
#{empi} <<The \code{empirical} object to consider.>>
#[INPUTS]
#VALUE
# A vector with same length as \code{p} containing the quantiles.
#EXAMPLE
# rbsb3k("RESET"); # for R checking compliance (useless)
# pempirical(seq(0,1,0.2),new("empirical"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_19
#REVISED 11_01_24
#--------------------------------------------
{
if (rbsb.mck) {
  # some checking
  check4tyle(p,"numeric",-1,c(0,1),"qempirical: some 'p' are not probabilities");
  che <- valid8empirical(empi);
  if (!identical(che,TRUE)) {
    erreur(che,"/empirical/ is not valid");
  }
}
# degenerate case
if (length(p)==0) { return(numeric(0));}
# normalization
empi <- normalize8empirical(empi);
#
# describing the distribution function with pieces of parabolae
#
# getting the abscissae
xx <- sort(unique(c(empi@xw[,1],empi@xy[,1])));
#
# getting the ordinates
if (empi@repa[2] == 0) {
  # only the discrete part
  yy <- discrete2cumulative(empi@xw,xx);
} else {
  # for the continuous part
  yy <- continuous2cumulative(empi@xy,xx);
  if (empi@repa[1] > 0) {
    # also must be added the discrete part
    for (ii in bc(nrow(empi@xw))) {
      # looking for the place into xx where to insert
      # the corresponding probability
      xi <- empi@xw[ii,1];
      jj <- which(xx==xi)[1];
      # adding it on both xx and yy
      xx <- c(xx[1:jj],xi,xx[bd(jj+1,length(xx))]);
      yy <- c(yy[1:jj],yy[bd(jj,length(yy))]+empi@xw[ii,2]);
    }
  }
}
#
# adding a starting point when the discrete part
# has got the lowest value
if (yy[1] > 0) {
  xx <- c(xx[1],xx);
  yy <- c(    0,yy);
}
dx <- diff(xx);
dy <- diff(yy);
#
# Classifying the different types of intervals
nbi <- length(xx) - 1;
# horizontal cases
# (= no proba at all)
cah <- which(dy==0);
# vertical cases
# (= discrete proba)
cav <- which(dx==0);
# increasing case cases
if (empi@repa[2]>0) {
  # (= continuous proba)
  cas <- setdiff(1:nbi,union(cah,cav));
  d2x <- cbind(xx[cas],xx[cas])+t(outer((1:2)/3,dx[cas],"*"));
  vco <- matrix(continuous2density(empi@xy,d2x),ncol=2);
  d2y <- vco[,2]-vco[,1];
  d2x <- d2x[,2]-d2x[,1];
  d2x[d2x==0] <- 1; 
  cde <- cas[d2y <0]; # decreasing ones
  cco <- cas[d2y==0]; #  constant ones
  cin <- cas[d2y >0]; #  inreasing ones
  # introducing the derivative in the same way
  ax <- rep(0,nbi);
  ax[cas] <- d2y/d2x;
  d2y <- ax;
} else {
  cde <- cco <- cin <- numeric(0);
  # and discrete weight must be duplicated
  xx <- rep(xx,each=2);
  yy <- rep(yy,each=2);
  xx <- xx[-c(1,3)];
  yy <- yy[-c(1,length(yy))];
  # updating in consequence the classification
  dx <- diff(xx);
  dy <- diff(yy);
  cah <- which(dy==0);
  cav <- which(dx==0);
}
# coding the type of curve of each different interval
# from starting points 'cbind(xx,yy,c(indica,10));'
indica <- rep(0,nbi);
indica[cin] <- indica[cin ] + 1;
indica[cco] <- indica[cco ] + 2;
indica[cde] <- indica[cde ] + 3;
indica[cav] <- indica[cav ] + 4;
## getting the corresponding coefficient for the curve pieces
# initializing with the horizontal case
nbir <- length(yy)-1;
ky <- rep(-1,nbir);
kx2 <- kx <- rep(0,nbir);
kk <- - yy[1:nbir];
if (empi@repa[2]>0) {
  # straight case
  kx[cco] <-   dy[cco] / dx[cco];
  kk[cco] <-   yy[cco] - dy[cco] / dx[cco] * xx[cco];
  # increasing and decreasing case
  ca2 <- c(cde,cin);
  kx2[ca2] <- d2y[ca2]/2;
  kx[ca2] <- dy[ca2]/dx[ca2] - kx2[ca2]*(xx[ca2]+xx[ca2+1]);
  kk[ca2] <- (xx[ca2]*yy[ca2+1]-xx[ca2+1]*yy[ca2]) / (xx[ca2]-xx[ca2+1]) +
             kx2[ca2]*xx[ca2]*xx[ca2+1];
}
# vertical case
ky[cav] <-  0;
kx[cav] <- -1;
kk[cav] <- xx[cav];
#print(cbind(xx[-(nbir+1)],yy[-(nbir+1)],indica,d2y,dy,dx,ky,kx2,kx,kk));
#
# starting the inversion
#
# getting the partition over the nbir intervals
ou <- findInterval(p,yy);
ou[ou>nbir] <- nbir;
ou[ou==0] <- 1;
#
# looping on the nbir interval
res <- rep(NA,length(p));
for (ii in bc(nbir)) { if (ii %in% ou) {
  oui <- which(ou==ii);
  res[oui] <-  solve2degree(p[oui],
                              ky[ii],kx2[ii],kx[ii],kk[ii],
                              xx[ii+(0:1)],xx[ii]
                             );
}}
#
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rempirical <- function(n,empi)
#TITLE  returns pseudo-random draws from an /empirical/
#DESCRIPTION
#   This function returns \code{n} independent pseudo-random
# draw from the /empirical/ object \code{empi}. The user is advised
# to manage the pseudo-random seed before calling this function.
# The object is normalized before being used (see \code{normalize8empirical}).
#DETAILS
# Just a call to \code{qempirical} with draw from the basic
# \code{runif}. But the bad convention of using \code{length(n)}
# when it is greater than 1 is not followed in case of \code{rbsb.mck}.
#KEYWORDS
#INPUTS
#{n} <<Wanted number of draws.>>
#{empi} <<The \code{empirical} object to consider.>>
#[INPUTS]
#VALUE
# A vector with length \code{n} containing the drawn values.
#EXAMPLE
# rbsb3k("RESET"); # for R checking compliance (useless)
# dempirical(100,new("empirical"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_23
#REVISED 11_01_23
#--------------------------------------------
{
if (rbsb.mck) {
  # some checking
  check4tyle(n,"numeric",1,c(0,Inf),message="qempirical: some 'p' are not probabilities");
  che <- valid8empirical(empi);
  if (!identical(che,TRUE)) {
    erreur(che,"/empirical/ is not valid");
  }
}
# degenerate case
if (n==0) { return(numeric(0));}
# normalization
empi <- normalize8empirical(empi);
#
# drawing uniform values
yy <- runif(n);
#
# deducing the values
res <- qempirical(yy,empi);
#
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
triangular2empirical <- function(a=-1,b=0,c=1,des=NULL)
#TITLE  returns the /empirical/ associated to a triangular distribution
#DESCRIPTION
#   This function returns  the /empirical/ object corresponding
# to the triangular distribution where \code{a} is the minimum value,
# \code{b} is the mode value and \code{c} is the maximum value.
#DETAILS
# The resulting object is normalized.
#KEYWORDS
#INPUTS
#[INPUTS]
#{a} <<Minimal value>>
#{b} <<Most probable value>>
#{c} <<Maximal value>>
#{des} << /des/ to give to the object, when \code{NULL}
# a standard description will be provided.>>
#VALUE
# An /empirical/ object.
#EXAMPLE
# rbsb3k("RESET"); # for R checking compliance (useless)
# plot(triangular2empirical(1,5,6),what="d");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_24
#REVISED 11_01_24
#--------------------------------------------
{
if (rbsb.mck) {
  # some checking
  check4tyle(a,"numeric",1,message="triangular2empirical: bad 'a'");
  check4tyle(b,"numeric",1,message="triangular2empirical: bad 'b'");
  check4tyle(c,"numeric",1,message="triangular2empirical: bad 'c'");
  if(!all(diff(c(a,b,c))>=0)) {
    erreur(c(a,b,c),"The three arguments must be non decreasing");
  }
}
# creation
if (is.null(des)) {
  des <- char2des("Triangular");
}
res <- new("empirical",des=des,
                       sup=c(a,c),
                       repa=c(0,1),
                       xw=matrix(NA,0,2),
                       xy=matrix(c(a,b,c,0,1,0),3,2)
          );
# normalization
res <- normalize8empirical(res);
#
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
trapezoid2empirical <- function(a=-1,b=-0.5,c=0.5,d=1,des=NULL)
#TITLE  returns the /empirical/ associated to a trapezoid distribution
#DESCRIPTION
#   This function returns  the /empirical/ object corresponding
# to the trapezoid distribution where \code{a} is the minimum value,
# \code{b} and \code{c} are the mode values and \code{d} is the maximum value.
#DETAILS
# The resulting object is normalized.
#KEYWORDS
#INPUTS
#[INPUTS]
#{a} <<Minimal value>>
#{b} <<Beginning of the mode interval>>
#{c} <<Ending of the mode interval>>
#{d} <<Maximal value>>
#{des} << /des/ to give to the object, when \code{NULL}
# a standard description will be provided.>>
#VALUE
# An /empirical/ object.
#EXAMPLE
# rbsb3k("RESET"); # for R checking compliance (useless)
# plot(trapezoid2empirical(1,5,6,6),what="d");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_24
#REVISED 11_01_24
#--------------------------------------------
{
if (rbsb.mck) {
  # some checking
  check4tyle(a,"numeric",1,message="trapezoid2empirical: bad 'a'");
  check4tyle(b,"numeric",1,message="trapezoid2empirical: bad 'b'");
  check4tyle(c,"numeric",1,message="trapezoid2empirical: bad 'c'");
  check4tyle(d,"numeric",1,message="trapezoid2empirical: bad 'd'");
  if(!all(diff(c(a,b,c,d))>=0)) {
    erreur(c(a,b,c),"The four arguments must be non decreasing");
  }
}
# creation
if (is.null(des)) {
  des <- char2des("Trapezoid");
}
res <- new("empirical",des=des,
                       sup=c(a,d),
                       repa=c(0,1),
                       xw=matrix(NA,0,2),
                       xy=matrix(c(a,b,c,d,0,1,1,0),4,2)
          );
# normalization
res <- normalize8empirical(res);
#
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

