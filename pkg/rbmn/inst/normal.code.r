
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
check8mn <- function(mn)
#TITLE checks a /mn/ object
#DESCRIPTION checks the consistency of \samp{mn} as a /mn/ object,
# returns some clues if inconsistent.
#DETAILS
# Looking a the code of this function provides a way to know which
# are the requirements of a /mn/ object. But rouhghly speaking, it is a list 
# with two (or three) names components. \samp{$mu} for the expectation (when 
# possible covariables are nought); \samp{gamma} for the (conditional)
# variance matrix (and \samp{rho} for the regression coefficients of
# the covariables). All this structure must be consistently named or dimnamed.
#KEYWORDS 
#INPUTS
#{mn} << The \samp{mn} object to check.>>  
#[INPUTS]
#VALUE
# \samp{character(0)} or a \samp{character} containing some clue
# about the discovered inconsistency.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);  
# check8mn(rbmn0$mn1$v);
# check8mn(rbmn0$adja1$v);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 14_08_18
#REVISED 14_08_19
#--------------------------------------------
{
  res <- character(0);
  # initial checking
  che <- sssobject9(mn,"list",c(2,3),fatal=FALSE);
  if (length(che) > 0) {
    return(c(che,"A /mn/ must be a list of length 2 or 3."));
  }
  lino <- names(mn);
  if (is.null(lino)) { return("A /mn/ must be a named list.");}
  if (!("mu" %in% lino)) { return("A /mn/ must have a component '$mu'");}
  if (!("gamma" %in% lino)) { return("A /mn/ must have a component '$gamma'");}
  che <- sssobject9(mn$mu,"numeric",-1,fatal=FALSE);
  # checking $mu
  if (length(che)>0) {
    return(c(che,"'mn$mu' must be a numerical vector"));
  }
  nbno <- length(mn$mu);
  nano <- names(mn$mu);
  if (is.null(nano)) {
    return("'mn$mu' must be a named numerical vector");
  }
  if (length(unique(nano)) < length(nano)) {
    return("Nodes must have different names");
  }
  # checking $gamma
  che <- sssobject9(mn$gamma,"matrix",speci=matrix(rep(nbno,4),2),fatal=FALSE);
  if (length(che)>0) {
    return(c(che,"'mn$gamma' must be a square matrix over the nodes"));
  }
  for (dd in 1:2) {
    if (is.null(dimnames(mn$gamma)[[dd]])) {
      return("'mn$gamma' must have got dimnames");
    }
    if (!all(dimnames(mn$gamma)[[dd]]==nano)) {
      return("Dimnames of 'mn$gamma' must be identical to 'names(mn$mu)'");
    }
  }
  if (!all(svd(mn$gamma)$d >= 0)) {
    return("'mn$gamma' seems not a non-negative matrix");
  }
  # checking $rho
  if (length(mn)==3) {
    if (!("rho" %in% lino)) { return("The possible third component of a /mn/ must be '$rho'");}
    che <- sssobject9(mn$rho,"matrix",speci=matrix(c(rep(nbno,3),NA),2),fatal=FALSE);
    if (length(che)>0) {
      return(c(che,"'mn$rho' must be a matrix with rows associated to the nodes"));
    }
    for (dd in 1:2) {
      if (is.null(dimnames(mn$rho)[[dd]])) {
        return("'mn$rho' must have got dimnames");
      }
      if (dd==1) { if (!all(dimnames(mn$rho)[[dd]]==nano)) {
        return("Row dimnames of 'mn$rho' must be identical to 'names(mn$mu)'");
      }}
      if (dd==2) { if (length(intersect(dimnames(mn$rho)[[dd]],nano))>0) {
        return("Colmuns dimnames of 'mn$rho' must not include element of 'names(mn$mu)'");
      }}
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
cor4var <- function(ma,check=rbmn0$check$v)
#TITLE returns the correlation matrix from the variance
#DESCRIPTION returns the correlation matrix from the variance
# preserving possible variable names
#DETAILS
# Zero variances are detected and accepted (all associated correlation
# coefficients are forced to be zero.>>
#KEYWORDS 
#INPUTS
#{ma}<< The variance matrix.>>
#[INPUTS]
#{check} << Must a check of the arguments be performed?>>
#VALUE
# The correlation matrix
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# cor4var(rbmn0$mn4$v$gamma,check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_12_05
#REVISED 12_07_31
#--------------------------------------------
{
  # checking
  if (check) {
    sssobject9(ma,"matrix",mensaje="'ma' must be a matrix");
    if (nrow(ma)!=ncol(ma)) {
      ssserreur(dim(ma),"'ma' must be a square matrix");
    }
  }
  # computing
  dd <- 1/sqrt(diag(ma));
  dd[diag(ma)<=0] <- 0;
  dd <- diag(dd,nrow=length(dd));
  res <- dd %*% ma %*% dd;
  dimnames(res) <- dimnames(ma);
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
var2pre <- function(ma,check=rbmn0$check$v)
#TITLE returns the precision matrix from the variance
#DESCRIPTION returns the precision matrix from the variance
# preserving possible variable names
#DETAILS
# Non full rank matrices are accepted, a generalized inverse
# is returned and a warning is issued.
#KEYWORDS 
#INPUTS
#{ma}<< The variance matrix.>>
#[INPUTS]
#{check} << Must a check of the arguments be performed?>>
#VALUE
# The precision matrix
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# var2pre(rbmn0$mn4$v$gamma,check=FALSE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_12_17
#REVISED 11_12_17
#--------------------------------------------
{
  # checking
  if (check) {
    sssobject9(ma,"matrix",mensaje="'ma' must be a matrix");
    if (nrow(ma)!=ncol(ma)) {
      ssserreur(dim(ma),"'ma' must be a square matrix");
    }
  }
  # constant
  eps <- 10^-10;
  # a look to the rank
  sivadi <- eigen(ma,symmetric=TRUE);
  nbv <- sum(sivadi$values > eps);
  if (nbv < nrow(ma)) {
    # issuing the warning
    ssserreur(NULL,message=
           paste("The matrix was found singular (rank = ",
                 nbv," < ",nrow(ma),") then a generalized inverse was provided",
                 sep=""));
    # computing the generalized inverse
    res <- sivadi$vectors[,sssbc(nbv)] %*%
           diag(1/sivadi$values[sssbc(nbv)],nrow=nbv,ncol=nbv) %*%
           t(sivadi$vectors[,sssbc(nbv)]);
  } else {
    res <- solve(ma);
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
condi4joint <- function(mn,par,pour,x2=rep(0,length(pour)),
                        check=rbmn0$check$v) 
#TITLE computes some conditional distribution of a multinormal vector
#DESCRIPTION returns the expectation and
# variance of a sub-vector conditionned
# with another (non overlapping) sub-vector
# from an initial random vector described 
# by \samp{mn}.
#DETAILS
#KEYWORDS 
#INPUTS
#{mn}<< list defining the distribution of
#       the initial vector with \samp{$mu},
#       its expectation, and \samp{$gamma},
#       its variance matrix.>>
#{par}<< names (or indices) of the sub-vector
#        to give the distribution.>>
#{pour}<< names (or indices) of the conditionning
#         sub-vector (can be \samp{NULL} when
#         for non conditionning.>>
#[INPUTS]
#{x2} <<values to consider for the conditioning
#       sub-vector. When \samp{NULL} the general
#       form is supplied, not a /mn/ object.  >>
#{check} << Must a check of the arguments be performed?>>
#VALUE
# A list:\cr
# when \samp{x2} provides the values taken by the
#      conditioning part, it is a /mn/ object with its
#      two components: \samp{$mu} for the expectation vector
#      and \samp{$gamma} for the variance matrix.\cr
# when \samp{x2} is \samp{NULL} the list has got three
#      components: \samp{$mu} for the fixed part of the
#      expectation vector, \samp{$rho} for the regression
#      coefficients to be associated to the non precised
#      \samp{x2} values, varying part of the expectation
#      and \samp{$gamma} for the variance matrix.\cr
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# print8mn(condi4joint(rbmn0$mn4$v,c("1.1","2.2","1.2","2.1"),NULL,check=FALSE),check=FALSE);
# print8mn(condi4joint(rbmn0$mn4$v,c("1.1","2.2","1.2","2.1"),"C",0,check=FALSE),check=FALSE);
# print(condi4joint(rbmn0$mn4$v,c("1.1","2.2","1.2","2.1"),"C",NULL,check=FALSE),check=FALSE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_12_05
#REVISED 12_07_31
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8mn(mn);
    if (length(che)>0) {
      ssserreur(che,"Bad 'mn'");
    }
    nona <- names(mn$mu);
    sssobject9(par,c("integer","character"),mensaje=c("Bad 'par'"));
    sssobject9(pour,c("null","integer","character"),mensaje=c("Bad 'pour'"));
    sssobject9(x2,c("null","numeric"),mensaje=c("Bad 'x2'"));
  }
  # lengths of the involved vectors
  n <- length(mn$mu); n1 <- length(par); n2 <- length(pour);
  if (n1+n2>n) { stop("mu.parpour");}
  # naming if necessary
  if (is.null(names(mn$mu))) {
    va <- paste("V",as.character(1:n),sep="");
    par <- va[par];
    pour <- va[pour];
    names(mn$mu) <- va;
    dimnames(mn$gamma) <- list(va,va);
  }
  if (length(intersect(par,pour)) > 0) { stop("parpour");}
  #  
  if (n1==0) {
    if (is.null(x2)) {
      res <- list(mu=numeric(n1),
                  rho=matrix(NA,n1,n2,dimnames=list(par,pour)),
                  gamma=matrix(NA,n1,n1)
                 );
    } else {
      res <- list(mu=numeric(n1),
                  gamma=matrix(NA,n1,n1)
                 );
    }
  } else {
    if (n2==0) {
      if (is.null(x2)) {
        res <- list(mu=mn$mu[par],
                    rho=matrix(NA,n1,n2,dimnames=list(par,pour)),
                    gamma=mn$gamma[par,par]);
      } else {
        res <- list(mu=mn$mu[par],gamma=mn$gamma[par,par]);
      }
    } else {
      mu1 <- mn$mu[par];
      mu2 <- mn$mu[pour];
      s11 <- mn$gamma[par,par,drop=FALSE];
      s12 <- mn$gamma[par,pour,drop=FALSE];
      s22 <- mn$gamma[pour,pour,drop=FALSE];
      ss12 <- s12 %*% solve(s22);
      si <- s11 - ss12 %*% t(s12);
      if (is.null(x2)) {
        ac <- mu1 - ss12 %*% matrix(mu2,n2,1);
        ac <- as.vector(ac);
        names(ac) <- par;
        res <- list(mu=ac,
                    rho=ss12,
                    gamma=si);
      } else {
        mu <- mu1 + ss12 %*% matrix(as.numeric(x2-mu2),length(pour),1);
        mun <- as.vector(mu);
        names(mun) <- par;
        res <- list(mu=mun,gamma=si);
      }
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
mn4joint1condi <- function(lmar,lcon,check=rbmn0$check$v) 
#TITLE computes a joint distribution from a marginal and a conditional one for multinormal distributions
#DESCRIPTION returns the expectation and
# variance of the multinormal normal distribution
# defined through a marginal subcomponent (\samp{lmar}) and
# a conditional distribution (\samp{lcon}).
#DETAILS
#KEYWORDS 
#INPUTS
#{lmar}<< /mn/ object defining the distribution of
#       the marginal part with \samp{$mu},
#       its expectation, and \samp{$gamma},
#       its variance matrix (in fact a /mn/ object).>>
#{lcon}<< /mn/ object defining the distribution of
#       the conditional part.>>
#[INPUTS]
#{check} << Must a check of the arguments be performed?>>
#VALUE
# A list with two components: \samp{$mu} of the expectation vector and
# \samp{$gamma} of the joint variance matrix; that is a /mn/ object.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# lcon <- list(mu=c(D=2,E=4),
#              rho=matrix(1:6,2,dimnames=list(LETTERS[4:5],
#                                           LETTERS[1:3])),
#              gamma=matrix(c(1,1,1,2),2,dimnames=list(LETTERS[4:5],LETTERS[4:5])));
#
# print8mn(mn4joint1condi(rbmn0$mn1$v,lcon,check=FALSE),check=FALSE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE (after the paper publication: change the components names of
# \samp{lcon} to be consistent with the output of function
# \samp{condi4joint} when regression coefficients are outputted.
#AUTHOR J.-B. Denis
#CREATED 11_12_14
#REVISED 11_12_14
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8mn(lmar);
    if (length(che)>0) {
      print(che);
      stop("The provided 'lmar' is not valid!");
    }
    che <- check8mn(lcon);
    if (length(che)>0) {
      print(che);
      stop("The provided 'lcon' is not valid!");
    }
  }
  # getting constants
  nv <- length(lmar$mu);
  nu <- length(lcon$mu);
  # degenerate cases
  if (nu == 0) {
    return(lmar);
  }
  if (nv == 0) {
    return(list(mu=lcon$mu,gamma=lcon$gamma));
  }
  # non matrix cases
  if (nv == 1) {
    if (!is.matrix(lmar$gamma)) {
      lmar$gamma <- matrix(lmar$gamma,nv,nv);
    }
    if (!is.matrix(lcon$rho)) {
      lcon$rho <- matrix(lcon$rho,nu,nv);
    }
  }
  if (nu == 1) {
    if (!is.matrix(lcon$gamma)) {
      lcon$gamma <- matrix(lcon$gamma,nu,nu);
    }
    if (!is.matrix(lcon$rho)) {
      lcon$rho <- matrix(lcon$rho,nu,nv);
    }
  }
  # general case
  if (is.null(names(lmar$mu))) {
    vam <- paste("M",as.character(1:nv),sep="");
    names(lmar$mu) <- vam;
    dimnames(lmar$gamma) <- list(vam,vam);
  } else {
    vam <- names(lmar$mu);
  }
  if (is.null(names(lcon$mu))) {
    vac <- paste("C",as.character(1:nu),sep="");
    names(lcon$mu) <- vac;
    dimnames(lcon$rho) <- list(vac,vam);
    dimnames(lcon$gamma) <- list(vac,vac);
  } else {
    vac <- names(lcon$mu);
  }
  # a limited check on the variable/covariable consistency
  if (length(intersect(vac,vam)) > 0) {
    stop("Overlap between variables and covariables!");
  }
  cova1 <- names(lmar$mu); cova2 <- dimnames(lcon$rho)[[2]];
  if (length(cova1)!=length(cova2)) {
    ssserreur(list(cova1,cova2),"Covariable number are different into 'lmar' and 'lcon'");
  }
  if (any(cova1!=cova2)) {
    ssserreur(list(cova1,cova2),"Covariable names are different into 'lmar' and 'lcon'");
  }
  # the computation
  n <- nv+nu;
  mu <- lcon$mu + lcon$rho %*% lmar$mu;
  mu <- c(lmar$mu,mu);
  s22 <- lcon$gamma + lcon$rho %*% lmar$gamma %*% t(lcon$rho);
  s12 <- lcon$rho %*% lmar$gamma;
  gamma <- rbind(t(s12),s22);
  gamma <- cbind(rbind(lmar$gamma,s12),
                 gamma);
  names(mu) <- c(vam,vac);
  dimnames(gamma) <- list(c(vam,vac),c(vam,vac));
  #
  res <- list(mu=mu,gamma=gamma);
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
simulate8mn <- function(mn,nbs,tol=1e-7,check=rbmn0$check$v) 
#TITLE simulates a multinormal vector
#DESCRIPTION returns a matrix of simulated
# values with the variable in columns and the 
# simulations in rows.
#DETAILS
# Just a call to the basic function \samp{mvrnorm}.
# Names of the variables are taken from those of
# \samp{mn$mu}, when these does not exist, standard
# ones are provided.
#KEYWORDS 
#INPUTS
#{mn}<< list defining the distribution of
#       the initial vector with \samp{$mu},
#       its expectation, and \samp{$gamma},
#       its variance matrix.>>
#{nbs}<< number of simulations to return.>>
#[INPUTS]
#{tol} << tolerance value to be transmitted
#         to \samp{mvrnorm}.>>
#{check} << Must a check of the arguments be performed?>>
#VALUE
# A matrix/data frame of size : \samp{nbs x length(mn$mu)}
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# print(simulate8mn(rbmn0$mn1$v,12,check=FALSE));
#REFERENCE
#SEE ALSO simulate8gmn
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 12_04_27
#REVISED 13_02_06
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8mn(mn);
    if (length(che)>0) {
      print(che);
      stop("The provided 'mn' is not valid!");
    }
    sssobject9(nbs,"integer",1,c(1,Inf),mensaje="Bad 'nbs'");
  }
  # number of variables and their names
  nbv <- length(mn$mu);
  # 
  if (is.null(names(mn$mu))) {
    va <- paste("V",as.character(sssbc(nbv)),sep="");
  } else {
    va <- names(mn$mu);
  }
  # number of simulations
  nbs <- round(max(0,nbs));
  # simulating
  if (nbv*nbs > 1) {
    res <- mvrnorm(nbs,mn$mu,mn$gamma,tol=tol);
    if (nbs == 1) {
      res <- matrix(res,nbs,nbv);
    }
  } else {
    res <- matrix(NA,nbs,nbv);
  }
  # adding the variable names
  dimnames(res) <- list(NULL,va);
  # returning
  as.data.frame(res);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
simulate8gmn <- function(loi,cova,nbs,tol=1e-7,check=rbmn0$check$v) 
#TITLE simulates a multinormal vector with varying expectation
#DESCRIPTION returns a matrix of simulated
# values with the variable in columns and the 
# simulations in rows.
#DETAILS
# Just a call to the function \samp{simulate8mn},
# adding the terms to the expectation due to the regression...
#KEYWORDS 
#INPUTS
#{loi}<< list defining the distribution of
#       the initial vector with \samp{$mu},
#       its expectation, \samp{$gamma},
#       its variance matrix and \samp{$rho} a
#       matrix of regression coefficients for
#       the covariables modifying the expectation.>>
#{cova}<< Values to give to the covariables.
#   Must be a matrix with \samp{nbs} rows and \samp{ncol(loi$rho)}
#   columns or a vector with \samp{ncol(loi$rho)} values to be
#   used for all simulations (i.e to replace a matrix with
#   identical rows..>>
#{nbs}<< number of simulations to return.>>
#[INPUTS]
#{tol} << tolerance value to be transmitted
#         to \samp{mvrnorm}.>>
#{check} << Must a check of the arguments be performed?>>
#VALUE
# A matrix of size : \samp{nbs x length(loi$mu)}
#EXAMPLE
# loi <- list(mu=c(D=2,E=4),
#             rho=matrix(1:6,2,dimnames=list(LETTERS[4:5],
#                                           LETTERS[1:3])),
#             gamma=matrix(c(1,1,1,2),2,dimnames=list(LETTERS[4:5],LETTERS[4:5])));
# cova <- matrix(runif(36),12,dimnames=list(NULL,LETTERS[1:3]));
# print(simulate8gmn(loi,cova,12,check=FALSE));
#REFERENCE
#SEE ALSO simulate8mn
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 13_02_06
#REVISED 13_02_18
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8mn(loi);
    if (length(che)>0) {
      print(c(che,"Bad 'loi'"));
      stop();
    }
    if (length(cova) > 0) {
      if (length(loi)==2) {
        ssserreur(NULL,"'loi' and 'cova' are not consistent (1).");
      }
      che <- sssobject9(cova,"matrix",
                     speci=matrix(c(nbs,ncol(loi$rho),nbs,ncol(loi$rho)),2));
      if (length(che)>0) {
        print(c(che,"'loi' and 'cova' are not consistent (1)."));
        stop();
      }
      if (is.null(dimnames(cova)[[2]])) {
        stop("'cova' must have column dimnames");
      }
      if (!all(dimnames(cova)[[2]]==dimnames(loi$rho)[[2]])) {
        stop("'cova and 'loi$rho' must coincide");
      }
    }
  }
  # simulating the constant part
  res <- simulate8mn(loi,nbs,tol);
  # adding the regression part
  if (!is.null(loi$rho)) { if (ncol(loi$rho) > 0) {
    nco <- dimnames(loi$rho)[[2]];
    if (!is.null(nco)) {
      nco <- sssbc(ncol(loi$rho));
    }
    if (is.matrix(cova)) {
      if (ncol(cova) != length(nco)) {
        stop("'cova' and 'loi$rho' column lengths are not consistent");
      }
      nco2 <- dimnames(loi$rho)[[2]];
      if (!is.null(nco2)) {
        nco2 <- sssbc(ncol(loi$rho));
      }
      if (!all(nco==nco2)) {
        stop("'cova' and 'loi$rho' column names are not consistent");
      }
      if (nrow(cova) != nbs) {
        stop("'cova' row number is not 'nbs'");
      }
    } else {
      if (length(cova) != length(nco)) {
        stop("'cova' and 'loi$rho' lengths are not consistent");
      }
      cova <- matrix(rep(as.numeric(cova),each=nbs),nbs);
    }
    res <- res + cova %*% t(loi$rho);
  }}
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8mn <- function(mn,what="mrsC",ordering=NULL,
                     digits=3,printed=TRUE,
                     check=rbmn0$check$v)
#TITLE standard print function for a /mn/ object.
#DESCRIPTION prints a /mn/ object completely
# or a part of it.
#DETAILS
#KEYWORDS 
#INPUTS
#{mn} <<\samp{mn} object to be printed.>>
#[INPUTS]
#{what} <<a \samp{character(1)}; when comprising "m" the 
#         expectations are printed, "r" the regression coefficient
#         if any, "s" the standard deviations
#         are printed, "C" the correlation matrix is printed,
#         "S" the variance matrix is printed,
#         "P" the precision matrix is printed,
#         "p" the normalized precision matrix is printed.>>
#{ordering} << Nodes are given following the indices of "ordering" if \samp{numeric}
#         or the names if it is \samp{character}. \samp{NULL} means the
#         identity permutation. Repetitions or missing nodes are accepted.>>
#{digits} << when not null, the number of digits for rounding the parameter values.>>
#{printed} << \samp{TRUE} to issue a printing, if not the prepared matrix
#           is returned.>>
#{check} << Must a check of the arguments be performed?>>
#VALUE
# The \samp{mn} is printed or a matrix having \samp{nn x ?} is returned
#  binding which elements precised in the argument \samp{what}.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# print8mn(rbmn0$mn1$v,check=FALSE);
# loi <- list(mu=c(D=2,E=4),
#             rho=matrix(1:6,2,dimnames=list(LETTERS[4:5],
#                                           LETTERS[1:3])),
#             gamma=matrix(c(1,1,1,2),2,dimnames=list(LETTERS[4:5],LETTERS[4:5])));
# print8mn(loi,check=FALSE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 12_01_19
#REVISED 14_08_19
#--------------------------------------------
{
  if (check) {
    che <- check8mn(mn);
    if (length(che)>0) {
      print(che);
      stop("The provided 'mn' is not valid!");
    }
    if (!is.null(ordering)) {
      if (is.character(ordering)) {
        if (length(setdiff(ordering,names(mn$mu)))>0) {
          ssserreur(ordering,"Bad argument 'ordering' as character");
        }
      } else {
        sssobject9(ordering,"numeric",-1,sssbf(mn$mu),
                mensaje="Bad argument 'ordering' as numeric");
      }
    }
    sssobject9(printed,"logical",1,mensaje="Bad argument 'printed'");
  }
  # number of nodes
  nn <- length(mn$mu);
  nam <- names(mn$mu);
  if (nn!=length(nam)) {
    ssserreur(mn$mu,message="'mn$mu' must be a named vector");
  }
  # getting the ordering for the nodes
  ordering <- aux3(nam,ordering);
  nnr <- length(ordering);
  namr <- nam[ordering];
  # initializing
  cnam <- character(0);
  res <- matrix(NA,nnr,0);
  # printing each asked option
  if (sssbelong9("m",what)) {
    cnam <- c(cnam,"mu");
    res <- cbind(res,mn$mu[ordering]);
  }
  if (sssbelong9("r",what)) { if (!is.null(mn$rho)) {
    cnam <- c(cnam,paste0("R.",dimnames(mn$rho)[[2]]));
    res <- cbind(res,mn$rho[ordering,,drop=FALSE]);
  }}
  if (sssbelong9("s",what)) {
    cnam <- c(cnam,"s.d.");
    res <- cbind(res,sqrt(diag(mn$gamma[ordering,ordering,drop=FALSE])));
  }
  if (sssbelong9("C",what)) { if (nnr > 1) {
    cnam <- c(cnam,paste("C.",namr,sep=""));
    res <- cbind(res,cor4var(mn$gamma[ordering,ordering,drop=FALSE],check=FALSE));
  }}
  if (sssbelong9("S",what)) {
    cnam <- c(cnam,paste("V.",namr,sep=""));
    res <- cbind(res,mn$gamma[ordering,ordering,drop=FALSE]);
  }
  if (sssbelong9("P",what)) {
    cnam <- c(cnam,paste("P.",namr,sep=""));
    res <- cbind(res,var2pre(mn$gamma[ordering,ordering,drop=FALSE]));
  }
  if (sssbelong9("p",what)) {
    cnam <- c(cnam,paste("PC.",namr,sep=""));
    res <- cbind(res,cor4var(var2pre(mn$gamma[ordering,ordering,drop=FALSE]),check=FALSE));
  }
  # dimnaming
  dimnames(res)[[2]] <- cnam;
  # rounding
  if (!is.null(digits)) {
    res <- round(res,digits);
  }
  # returning
  if (printed) {
    print(res);
    invisible();
  } else {
    return(res);
  }
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
dev4mn <- function(Y,EY,VY)
#TITLE Computes the deviance for a sample of multinormal vector
#DESCRIPTION 
# Returns the deviance from the \samp{n} observed values of a vector of size \samp{p} (Y),
# their expectations (EY) and the
# variance matrix (VY) supposed identical for all vectors;
# i.e. \samp{-2*log(p(Y))}.
#DETAILS
# When \samp{EY} is a vector with length \samp{ncol(Y)} this
# supposes that all observations have the same expectation.
#KEYWORDS 
#INPUTS
#{Y} <<Matrix \samp{nxp} of the \samp{n} observed values of length \samp{p}.>>
#{EY}<<Expectation of \samp{Y} (matrix \samp{nxp} or vector \samp{p}).>>
#{VY}<<Matrix of the variance of each row of \samp{Y} (matrix \samp{pxp}).>>
#[INPUTS]
#VALUE
# A scalar 
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# dev4mn(matrix(runif(3),1),t(rbmn0$mn1$v$mu),rbmn0$mn1$v$gamma);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT to be made consistent with a /mn/ object!
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 13_06_19
#REVISED 13_07_30
#--------------------------------------------
{
  # checking 1
  if (!(is.matrix(Y) | is.data.frame(Y))) {
    stop("'Y' must be a data.frame or a matrix");
  }
  if (length(EY) == ncol(Y)) {
    EY <- matrix(rep(EY,each=nrow(Y)),nrow(Y));
  }
  if (!is.matrix(EY)) { stop("'EY' must be a matrix");}
  if (!is.matrix(VY)) { stop("'VY' must be a matrix");}
  # size of the vector
  pp <- ncol(Y);
  # checking 2
  if (!all(dim(Y)==dim(EY))) {
    stop("'Y' and 'EY' don't have the same dimensions");
  }
  if (!all(dim(VY)==rep(pp,2))) {
    stop("'VY' doesn't have the consistent dimensions");
  }
  # computing the precision matrix
  S1 <- solve(VY);
  # residuals
  del <- Y-EY;
  # computing the quadratic part
  qua <- 0;
  for (ii in sssbc(nrow(Y))) {
    deli <- matrix(as.numeric(del[ii,,drop=FALSE]),nrow=1);
    qua <- qua + deli %*% S1 %*% t(deli);
  }
  # computing the "constant" part
  kon <- determinant(VY)$modulus + pp*log(2*pi);
  # returning
  nrow(Y)*kon + qua;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
