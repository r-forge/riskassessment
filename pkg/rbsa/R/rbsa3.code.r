
####### rbsa3.code.r ########################


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
intersect8interv <- function(int1,int2,monitor=rbsa0$monitor$v)
#TITLE  computes the interval intersection of two intervals
#DESCRIPTION
# computes and returns the interval (vector of length 2 or 0)
# which is the intersection of two given intervals.\cr
# Null intervals are indicated by \samp{numeric(0)}.
#DETAILS
#KEYWORDS misc
#INPUTS
#{int1} <<The first interval (numeric(2) or numeric(0)).>>
#{int2} <<The second interval (numeric(2) or numeric(0)).>>
#[INPUTS]
#{monitor} <<List providing the monitoring constants, see \code{rbsa0$monitor$v}
#            to know the contents.>>
#VALUE
# A numeric(2) or numeric(0) providing the intersection of the
# two intervals.
#EXAMPLE
# intersect8interv(numeric(0),1:2);
# intersect8interv(c(1,10),c(-3,5));
# intersect8interv(c(1,10),c(10,12));
# intersect8interv(c(1,10),c(11,12));
# intersect8interv(c(1,10),c(pi,10*pi))
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_11_17
#REVISED 10_12_13
#--------------------------------------------
{
    # checking
    l1 <- length(int1); l2 <- length(int2);
    if (monitor$chk$v) {
        if (!(l1 %in% c(0,2))) { erreur(int1,"This is not an interval: numeric(0) or numeric(2) expected"); }
        if (!(l2 %in% c(0,2))) { erreur(int2,"This is not an interval: numeric(0) or numeric(2) expected"); }
        if (l1 == 2) {
          if (diff(int1) < 0) { erreur(int1,"This is not an interval: lower > upper"); }
          if (is.nan(diff(int1))) {erreur(int1,"This is not an accepted interval");}
        }
        if (l2 == 2) {
          if (diff(int2) < 0) { erreur(int1,"This is not an interval: lower > upper"); }
          if (is.nan(diff(int2))) {erreur(int2,"This is not an accepted interval");}
        }
    }
    # degenerate case
    if (l1*l2 == 0) { return(numeric(0));}
    # null cases
    if ((int1[2] < int2[1])|(int2[2] < int1[1])) { return(numeric(0));}
    # returning
    res <- c(max(int1[1],int2[1]),min(int1[2],int2[2]))
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
interv7belonging <- function(x,int,monitor=rbsa0$monitor$v)
#TITLE  checks if a series of values belong to a series of intervals
#DESCRIPTION
# computes and returns the indicator vector of the positions of
# values with respect to intervals.
#DETAILS
# This function is compatible with real infinite values
#KEYWORDS misc
#INPUTS
#{x} <<Vector of value(s) to be scrutinized.>>
#{int} <<Series of interval(s) to be considered.
# Either a \samp{numeric(2)} or a matrix with two columns.
# Empty intervals (\samp{numeric(0)} are not admitted.>>
#[INPUTS]
#{monitor} <<List providing the monitoring constants, see \code{rbsa0$monitor$v}
#            to know the contents.>>
#VALUE
# A matrix with rows associated to the \code{x} values and
# columns associated to the \code{int} intervals giving
# \code{-2,-1,0,1,2} according to whether \code{x} is less than,
# equal to the lower bound, inside, equal to the upper bound or
# greater than the interval.
#EXAMPLE
# interv7belonging(1:5,1:2);
# interv7belonging(1:5,matrix(1:10,ncol=2));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_11_17
#REVISED 10_12_13
#--------------------------------------------
{
    # checking
    if (monitor$chk$v) {
        object9(  x,"numeric",-1,mensaje="  'x' must be numeric");
        object9(int,"numeric",-1,mensaje="'int' must be numeric");
        if (!is.matrix(int)) {
            if (length(int) != 2) {
                erreur(int,"When 'int' is not a matrix, it must be a numeric(2)");
            }
            if (is.nan(diff(int))) {erreur(int,"This is not an accepted interval");}
            if (diff(int)<0) { erreur(int,"'int' does not define an interval!");}
        } else {
            if (ncol(int)!=2) {
                erreur(int,"When 'int' is a matrix, it must comprise 2 columnes");
            }
            ru <- int[,2] - int[,1];
            if (any(is.nan(ru))) { erreur(int,"Some rows are not accepted as intervals");}
            if (any((ru<0))) {
                erreur(int,"Not all rows of 'int' define an interval");
            }
        }
    }
    # getting a uniform presentation
    if (!is.matrix(int)) { int <- matrix(int,ncol=2);}
    # preparing the result
    nbx <- length(x); nbint <- nrow(int);
    res <- matrix(NA,nbx,nbint);
    dimnames(res) <- list(names(x),dimnames(int)[[1]]);
    # degenerate case
    if (length(res)==0) { return(res);}
    # ancillary functions
    be0 <- function(x,int0) {
      if (is.finite(int0)) {
        ss <- sign(x-int0);
      } else {
        ss <- rep(-sign(int0),length(x));
        ss[x==int0] <- 0;
      }
      ss;
    }
    bel <- function(x,int) {
        be0(x,int[1]) + be0(x,int[2]);
    }
    # computation
    for (ii in bc(nrow(int))) {
        res[,ii] <- bel(x,int[ii,]);
    }
    # returning
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
solve8quadratic <- function(y,ky,kx2,kx,kk,dx=NULL,x0=NULL,monitor=rbsa0$monitor$v)
#TITLE  solves a degree two polynomial
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
# exist(s) and do(es) not comply) a fatal error
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
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \code{rbsa0$monitor$v} provided object as an example.>>
#VALUE
# A matrix having one or two columns according to the values of
# \code{ky,kx2,kx,kk,dx}.
#EXAMPLE
# solve8quadratic(1:10, 1,1,0,-20);
# solve8quadratic(   3,-1,1,1,  1);
# solve8quadratic(   3,-1,1,1,  1,c(0.5,1.5));
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
if (monitor$chk$v) {
  object9(y,"numeric",-1,mensaje="solve8quadratic: non accepted 'y'");
  object9(ky,"numeric",1,mensaje="solve8quadratic: non accepted 'ky'");
  object9(kx2,"numeric",1,mensaje="solve8quadratic: non accepted 'kx2'");
  object9(kx,"numeric",1,mensaje="solve8quadratic: non accepted 'kx'");
  object9(kk,"numeric",1,mensaje="solve8quadratic: non accepted 'kk'");
  if (!is.null(dx)) {
    object9(dx,"numeric",2,mensaje="solve8quadratic: non accepted 'dx'");
  }
  if (!is.null(x0)) {
    object9(x0,"numeric",1,mensaje="solve8quadratic: non accepted 'x0'");
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
      erreur(list(ky,kx2,kx,kk),"solve8quadratic: no solution for the proposed equation");
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
