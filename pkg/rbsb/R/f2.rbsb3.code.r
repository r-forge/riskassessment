
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
intersect4interval <- function(int1,int2)
#TITLE  computes the interval intersection of two intervals
#DESCRIPTION
# computes and returns the interval (vector of length 2 or 0)
# which is the intersection of two given intervals.\cr
# Null intervals are indicated by \code{rbsb.num0}.
#DETAILS
#KEYWORDS misc
#INPUTS
#{int1} <<The first interval (numeric(2) or numeric(0)).>>
#{int2} <<The second interval (numeric(2) or numeric(0)).>>
#[INPUTS]
#VALUE
# A numeric(2) or numeric(0) providing the intersection of the
# two input intervals.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# intersect4interval(numeric(0),1:2);
# intersect4interval(c(1,10),c(-3,5));
# intersect4interval(c(1,10),c(10,12));
# intersect4interval(c(1,10),c(11,12));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_11_17
#REVISED 10_11_17
#--------------------------------------------
{
    # checking
    l1 <- length(int1); l2 <- length(int2);
    if (rbsb.mck) {
        if (!(l1 %in% c(0,2))) { erreur(int1,"This is not an interval: numeric(0) or numeric(2) expected"); }
        if (!(l2 %in% c(0,2))) { erreur(int2,"This is not an interval: numeric(0) or numeric(2) expected"); }
        if (l1 == 2) { if (diff(int1) < 0) { erreur(int1,"This is not an interval: lower > upper"); }}
        if (l2 == 2) { if (diff(int2) < 0) { erreur(int1,"This is not an interval: lower > upper"); }}
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
belong2interval <- function(x,int)
#TITLE  checks if a series of values belong to a series of intervals
#DESCRIPTION
# computes and returns the indicator vector of the positions of
# values with respect to intervals.
#DETAILS
#KEYWORDS misc
#INPUTS
#{x} <<vector of value(s) to be scrutinized.>>
#{int} <<series of interval(s) to be considered.
# Either a numeric(2) or a matrix with two columns.\cr
# Empty intervals (\code{numeric(0)}= are not admitted.>>
#[INPUTS]
#VALUE
# A matrix with rows associated to the \code{x} values and
# columns associated to the \code{int} intervals giving
# \code{-2,-1,0,1,2} according to whether \code{x} is less than,
# equal to the lower bound, inside, equal to the upper bound or
# greater than the interval.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# belong2interval(1:5,1:2);
# belong2interval(1:5,matrix(1:10,ncol=2));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_11_17
#REVISED 10_11_17
#--------------------------------------------
{
    # checking
    if (rbsb.mck) {
        check4tyle(  x,rbsb.numer,-1,message="  'x' must be numeric");
        check4tyle(int,rbsb.numer,-1,message="'int' must be numeric");
        if (!is.matrix(int)) {
            if (length(int) != 2) {
                erreur(int,"When 'int' is not a matrix, it must be a numeric(2)");
            }
            if (diff(int)<0) { erreur(int,"'int' does not define an interval!");}
        } else {
            if (ncol(int)!=2) {
                erreur(int,"When 'int' is a matrix, it must comprise 2 columnes");
            }
            if (!all((int %*% c(-1,1))>=0)) {
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
    # ancillary function
    bel <- function(x,int) {
        sign(x-int[1]) + sign(x-int[2]);
    }
    # computation
    for (ii in bc(nrow(int))) {
        res[,ii] <- bel(x,int[ii,]);
    }
    # returning
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
