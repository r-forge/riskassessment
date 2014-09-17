
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
create8star <- function(rho=rep(0.5,2),
                        lambda=rep(0.4,4),
                        sigi=rep(1,length(rho)),
                        sigc=sqrt(1-sum(rho^2)),
                        sigo=sqrt(1-lambda^2),
                        quoi="gema",check=rbmn0$check$v)
#TITLE returns a star /rbmn/
#DESCRIPTION 
# A star /rbmn/ is a /rbmn/ comprising a centre node to which enter 
# \samp{ni=length(rho)} independent input nodes and from which
# come out \samp{no=length(lambda)} output nodes. This
# gives roughly the configuration of a star, the centre node being the center
# of the star and the other nodes some kind of radiuses around it.
#DETAILS
#KEYWORDS 
#INPUTS
#[INPUTS]
#{rho}<< Gives the regression coefficients of each input nodes,
# which are the parents of the center node.>>
#{lambda}<< Gives the regression coefficients of the centre node for each output node.>>
#{sigi} << Vector of the standard deviation of the input nodes.>>
#{sigc} << Scalar giving the conditional standard deviation of the center node.
#  The default value ensures a marginal variance of 1.>>
#{sigo} << Vector giving the conditional standard deviation of the output nodes.
#  The default value ensures a marginal variance of 1.>>
#{quoi}<< Indicates the desired type of output. The different possibilities 
# are \samp{gema} for a generating matrix, \samp{vari} for the variance
# matrix.>>
#{check} << Must a check of the arguments be performed?>>
#VALUE
# The resulting structure with standard names.
#EXAMPLE
# create8star(check=TRUE);
# create8star(rho=rep(0.2,10),check=TRUE);
# create8star(rho=rep(0,10),quoi="vari",check=TRUE);
# create8star(rho=rep(1,4),sigc=0,quoi="vari",check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 13_11_28
#REVISED 13_12_04
#--------------------------------------------
{
  # checking
  if (check) {
    sssobject9(rho,"numeric",c(0,Inf),c(0,Inf),
               mensaje="'rho' must be a non negative numeric vector");
    sssobject9(lambda,"numeric",c(0,Inf),c(0,Inf),
               mensaje="'lambda' must be a non negative numeric vector");
    sssobject9(sigi,"numeric",length(rho),c(0,Inf),
               mensaje="'sigi' must be a non negative numeric vector of length identical to 'rho'");
    sssobject9(sigo,"numeric",length(lambda),c(0,Inf),
               mensaje="'sigo' must be a non negative numeric vector of length identical to 'lambda'");
    sssobject9(sigc,"numeric",1,c(0,Inf),
               mensaje="'sigc' must be a non negative numeric scalar");
    sssobject9(quoi,"character",1,c("gema","vari"),
               mensaje="'quoi' must be an accepted keyword");
  }
  # some constants
  ni <- length(rho); no <- length(lambda);
  nava <- sssform3names(ni,nume=-9);
  nava <- c(nava,"CE");
  nava <- c(nava,sssform3names(no,nume=-15));
  if (quoi=="gema") {
    nage <- sssform3names(ni+1+no,nume=-5);
    res <- matrix(0,ni+1+no,ni+1+no,
                  dimnames=list(nava,nage));
    res[sssbc(ni),sssbc(ni)] <- diag(sigi,ni,ni);
    res[ni+1,sssbc(ni)] <- rho*sigi;
    res[ni+1,ni+1] <- sigc;
    res[ni+1+sssbc(no),sssbc(ni)] <- outer(lambda,rho*sigi,"*");
    res[ni+1+sssbc(no),ni+1] <- lambda*sigc;
    res[ni+1+sssbc(no),ni+1+sssbc(no)] <- diag(sigo,no,no);
  }
  if (quoi=="vari") {
    res <- matrix(0,ni+1+no,ni+1+no,
                  dimnames=list(nava,nava));
    res[sssbc(ni),sssbc(ni)] <- diag(sigi^2,ni,ni);
    res[sssbc(ni),ni+1] <- sigi^2*rho;
    res[ni+1,sssbc(ni)] <- sigi^2*rho;
    res[sssbc(ni),ni+1+sssbc(no)] <- outer(sigi^2*rho,lambda,"*");
    res[ni+1+sssbc(no),sssbc(ni)] <- outer(lambda,sigi^2*rho,"*");
    res[ni+sssbc(1+no),ni+sssbc(1+no)] <- (sum(sigi^2*rho^2)+sigc^2) *
                                    outer(c(1,lambda),c(1,lambda),"*");
    res[ni+1+sssbc(no),ni+1+sssbc(no)] <- res[ni+1+sssbc(no),ni+1+sssbc(no)] +
                                    diag(sigo^2,no,no);
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
