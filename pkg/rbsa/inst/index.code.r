
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
ijk2n <- function(ii,di,monitor=rbsa0$monitor$v) 
#TITLE transforms a series of indices into a unique index,
#      and conversely
#DESCRIPTION
# Probably, it is worth running the examples before
# trying to understand the following explanation!\cr
# \samp{ii[k]} (or \samp{ii[i,k]} is supposed to be a value in \samp{1:di[k]},
# this function returns the global index obtained by
# running the nested loops \samp{ii[k]} (or \samp{ii[i,k]}).\cr
# The reverse computation is done by \samp{n2ijk}
#DETAILS
# When \samp{ii} is a matrix, this is equivalent to call the function
# repeatedly for each row.\cr
# \samp{length(ii)} (or \samp{ncol(ii)}) is supposed to be equal to \samp{length(di)}.
#KEYWORDS
#INPUTS
#{ii} <<vector of integers or a matrix of them.>>
#{di} <<vector of integers giving the dimensions of each sub-index.>>
#[INPUTS]
#{monitor} <<List providing the monitoring constants, see \samp{rbsa0$monitor$v}
#            to know the contents.>>
#ALIAS indexing
#VALUE
# a positive integer (or a vector of length \samp{nrow(ii)}.
#EXAMPLE
# for (i1 in 1:4) { for (i2 in 1:2) { for (i3 in 1:3) {
#   print(c(i1,i2,i3,ijk2n(c(i1,i2,i3),c(4,2,3))));
# }}}
# glo <- cbind(rep(1:4,each=6),rep(rep(1:3,each=2),4),rep(1:2,12));
# print(cbind(glo,ijk2n(glo,c(4,3,2))));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_03_23
#REVISED 13_10_25
#--------------------------------------------
{
  #
  # some checking
  #
  if (monitor$chk$v) {
    object9(ii,"numeric",c(0,Inf),c(1,Inf),"'ii' not accepted");
    object9(di,"numeric",c(0,Inf),c(1,Inf),"'di' not accepted");
    if (is.matrix(ii)) {
      if (ncol(ii) != length(di)) {
        erreur(list(ii,di),"'ii' and 'di' have not compatible dimensions");
      }
      if (!all(ii<=rep(di,each=nrow(ii)))) {
        erreur(list(ii,di),"Some values in 'ii' are greater than 'di'");
      }
    } else {
      if (length(ii) != length(di)) {
        erreur(list(ii,di),"'ii' and 'di' are supposed with equal lengths");
      }
      if (!all(ii<=di)) {
        erreur(list(ii,di),"'ii' cannot be greater than 'di'");
      }
    }
  }
  # normalizing
  if (!is.matrix(ii)) {
    ii <- matrix(ii,nrow=1);
  }
  # computing
  KK <- length(di);
  if (KK > 0) {
    res <- ii[,KK];
    for (kk in bc(KK-1)) {
      res <- res + (ii[,kk]-1) * prod(di[(kk+1):KK]);
    }
  } else {
    res <- 0;
  }
  #
  # returning
  res
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
n2ijk <- function(n,di,monitor=rbsa0$monitor$v) 
#TITLE transforms a unique index into a series of indices
#DESCRIPTION
# From a global index obtained by
# running the nested loops according to \samp{di} returns the matrix
# \samp{ii} each row of which is the corresponding value in \samp{1:di[k]}.
# \cr The reverse computation is done by \samp{ijk2n}
#DETAILS
#KEYWORDS
#INPUTS
#{n} <<an integer vector with values in between \samp{1} and \samp{prod(di)}.>>
#{di} <<vector of non negative integers giving the number of indices and
#       their maximal values.>>
#[INPUTS]
#{monitor} <<List providing the monitoring constants, see \samp{rbsa0$monitor$v}
#            to know the contents.>>
#VALUE
# a matrix of dimension \samp{c(length(n),length(di))}.
#EXAMPLE
# for (n in 1:24) {
#   print(c(n,n2ijk(n,c(4,2,3))));
# }
# print(cbind(1:24,n2ijk(1:24,c(4,3,2))));
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_03_23
#REVISED 13_10_25
#--------------------------------------------
{
#
# some checking
#
if (monitor$chk$v) {
  object9(n,"numeric",-1,c(1,Inf),"'n' not accepted");
  object9(di,"numeric",c(0,Inf),c(1,Inf),"'di' not accepted");
  if (any(n > prod(di))) {
    erreur(list(n,di),"A value of 'n' is too great with respect to 'di'");
  }
}
#
# computing
KK <- length(di);
if (KK > 0) {
  res <- matrix(NA,length(n),KK);
  for (kk in bc(KK-1)) {
    res[,kk] <- 1 + (n-1) %/% prod(di[(kk+1):KK]);
    n <- n - (res[,kk]-1)*prod(di[(kk+1):KK]);
  }
  res[,KK] <- n;
} else {
  res <- numeric(0);
}
#
# returning
res
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
