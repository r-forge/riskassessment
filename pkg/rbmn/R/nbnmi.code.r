
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
topo9 <- function(nbn)
#TITLE checks a topological order
#DESCRIPTION checks that the supposed \samp{nbn} has got
# a topological order. If not return some indications about
# the inconsistencies.
#DETAILS
# I am not completely certain about the validity of the
# proposed algorithm...
#KEYWORDS 
#INPUTS
#{nbn} << The \samp{nbn} object to check.>>  
#[INPUTS]
#VALUE
# \samp{character(0)} if true or a \samp{character} containing 
# the nodes involved in some cycle or, descendants of nodes involved
# in some cycle.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# topo9(rbmn0$nbn1$v);
# topo9(rbmn0$adja1$v);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 14_08_13
#REVISED 14_08_13
#--------------------------------------------
{
  # initial checking
  if (!is.list(nbn)) {
    return("A /nbn/ must be a list");
  }
  # getting the node numbers
  nbno <- length(nbn);
  # corner case
  if (nbno==0) { return(character(0));}
  # checking the existence of node names
  nano <- names(nbn);
  if (length(unique(nano)) < nbno) {
    return("A /nbn/ must be a named list with different names");
  }
  # initialization
  innod <- character(0);
  # getting the root nodes
  for (nn in nano) {
    if (length(nbn[[nn]]$parents)==0) {
      innod <- c(innod,nn);
    }
  }
  if (length(innod)==0) {
    return(nano);
  }
  # getting the children of the root nodes
  newnod <- 1;
  while (newnod > 0) {
    newnod <- 0;
    # trying every non included node
    for (nn in setdiff(nano,innod)) {
      # are all its parents already included?
      ppa <- nbn[[nn]]$parents;
      if (length(innod)==length(union(innod,ppa))) {
        # it can be included
        newnod <- 1;
        innod <- c(innod,nn);
      }
    }
  }
  # are all nodes included
  if (length(innod)==nbno) {
    res <- character(0);
  } else {
    res <- setdiff(nano,innod);
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
check8nbn <- function(nbn,topo=TRUE)
#TITLE checks a /nbn/ object
#DESCRIPTION checks the consistency of \samp{nbn} as a /nbn/ object,
# returns some clues if inconsistent.
#DETAILS
# Looking a the code of this function provides a way to know which
# are the requirements of a /nbn/ object.
#KEYWORDS 
#INPUTS
#{nbn} << The \samp{nbn} object to check.>>  
#[INPUTS]
#{topo} << Must the existence of a topological order be checked?>>
#VALUE
# \samp{character(0)} or a \samp{character} containing some clue
# about the discovered inconsistency.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# check8nbn(rbmn0$nbn1$v);
# check8nbn(rbmn0$adja1$v);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 13_07_26
#REVISED 14_08_12
#--------------------------------------------
{
  # initial checking
  if (!is.list(nbn)) {
    return("A /nbn/ must be a list");
  }
  # getting the node numbers
  nbno <- length(nbn);
  # checking the existence of node names
  nano <- names(nbn);
  if (length(unique(nano)) < nbno) {
    return("A /nbn/ must be a named list with different names");
  }
  # checking each node in turn
  res <- character(0);
  for (nn in sssbc(nbno)) {
    na <- names(nbn)[nn];
    if (is.null(nbn[[na]]$mu)) {
      res <- c(res,paste("node",na,"hasn't got a '$mu'"));
    } else {
      if (length(nbn[[nn]]$mu) != 1) {
        res <- c(res,paste("node",na,"has got a '$mu' with a length != to 1"));
      } else {
        if (!is.numeric(nbn[[nn]]$mu)) {
          res <- c(res,paste("node",na,"has got a '$mu' which is not numeric"));
        }
      }
    }
    if (is.null(nbn[[na]]$sigma)) {
      res <- c(res,paste("node",na,"hasn't got a '$sigma'"));
    } else {
      if (length(nbn[[nn]]$sigma) != 1) {
        res <- c(res,paste("node",na,"has got a '$sigma' with a length != to 1"));
      } else {
        if (!is.numeric(nbn[[nn]]$sigma)) {
          res <- c(res,paste("node",na,"has got a '$sigma' which is not numeric"));
        } else {
          if (nbn[[nn]]$sigma < 0) {
            res <- c(res,paste("node",na,"has got a '$sigma' which is negative"));
          }
        }
      }
    }
    if (length(nbn$parents) > 0) {
      papa <- nbn$parents;
      cpa <- TRUE;
      if (length(unique(papa))!=length(papa)) {
        res <- c(res,paste("node",na,"has got duplicated parents"));
        cpa <- FALSE;
      }
      if (length(union(papa,nano))!= length(nano)) {
        res <- c(res,paste("node",na,"has got parents which are not nodes"));
        cpa <- FALSE;
      }
      if (any(na==papa)) {
        res <- c(res,paste("node",na,"is itself its parent"));
        cpa <- FALSE;
      }
      if (cpa) {
        if (length(papa) != length(nbn$regcoef)) {
          res <- c(res,paste("node",na,"has got a '$parents' and '$regcoef' with different lengths"));
        }
        if (!is.numeric(nbn$regcoef)) {
          res <- c(res,paste("node",na,"has got a '$regcoef' which is not numeric"));
        }
      }
    }      
  }
  # existence of a topological order
  if (sssvoid9(res)) { if (topo) {
    res <- topo9(nbn)
  }}
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8nbn <- function(nbn,what="pr",digits=3,ordering=NULL,check=rbmn0$check$v)
#TITLE print function for a /nbn/ object.
#DESCRIPTION prints a /nbn/ object.
#DETAILS
#KEYWORDS 
#INPUTS
#{nbn} <<\samp{nbn} object to be printed.>>
#[INPUTS]
#{what} <<a \samp{character(1)}; when comprising "p" the name of each node
#         with its parents are given, when comprising "r the formula
#         regression of each node is given with the node, when comprising
#         "m" the model is given.>>
#{digits} << when not null, the number of digits for rounding.>>
#{ordering} << Nodes are given following the indices of "ordering" if \samp{numeric}
#         or the names if it is \samp{character}. \samp{NULL} means the
#         identity permutation. Repetitions or missing nodes are accepted.>>
#{check} << Must a check of the argument be performed?>>
#VALUE
# Nothing but but \samp{nbn} is printed.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# print8nbn(rbmn0$nbn1$v,check=TRUE);
# print8nbn(rbmn0$nbn3$v,"pm",order=1:2,check=TRUE)
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE add the option 'model'
#AUTHOR J.-B. Denis
#CREATED 12_01_14
#REVISED 13_07_04
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8nbn(nbn);
    if (length(che)>0) {
      print(che);
      stop("The provided 'nbn' is not valid!");
    }
  }
  # number of nodes
  nn <- length(nbn);
  # getting the ordering for the nodes
  ordering <- aux3(names(nbn),ordering);
  # printing each asked option
  if (sssbelong9("m",what)) {
    cat("\nModel:",string7dag4nbn(nbn),"\n\n")
  }
  pare <- sssbelong9("p",what);
  regr <- sssbelong9("r",what);
  if (pare | regr) {
    for (i in sssbf(ordering)) {
      ii <- ordering[i];
      if (i==1) {
        cat(sssform3justify("Nodes",nbc=10,format=3,carac="="));
        cat("===");
        if (pare) {
          cat(sssform3justify("[parents]",nbc=12,format=1,carac=" "));
        }
        if (regr) {
          cat(sssform3justify("= Exp. (sd.dev)",nbc=22,format=1,carac=" "));
        }
        cat("\n");
        cat(rep("-",10+3+12+22),collapse="",sep="");
        cat("\n");
      }
      nbpa <- length(nbn[[ii]]$parents);
      cat(sssform3justify(names(nbn)[ii],nbc=10,format=3,carac="-"));
      cat("---");
      if (pare) {
        if (nbpa == 0) {
          papa = "[-]";
        } else {
          papa <- paste(nbn[[ii]]$parents,collapse=",");
          papa <- paste("[",papa,"]",sep="");
        }
        cat(papa,sep="");
      }
      if (regr) {
        rere <- "  =";
        if ((nbn[[ii]]$mu != 0)| (nbpa==0)) {
          rere <- paste(rere,round(nbn[[ii]]$mu,digits));
          if (nbpa>0) {
            rere <- paste(rere,"+");
          }
        }
        for (pp in sssbc(nbpa)) {
          rere <- paste(rere,
                       paste(round(nbn[[ii]]$regcoef[pp],digits),"*",
                             nbn[[ii]]$parents[pp],sep="")
                      );
          if (pp < nbpa) {
            rere <- paste(rere,"+");
          }
        }
        rere <- paste(rere,
                      paste(" (",round(nbn[[ii]]$sigma,digits),")",sep="")
                     );
        cat(rere);
      }
      cat("\n");
    }
  }
  # returning
  invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8gema <- function(gema,what="ml",ordering=NULL,
                       digits=3,printed=TRUE,check=rbmn0$check$v)
#TITLE standard print function for a /gema/ object.
#DESCRIPTION prints a /gema/ object completely
# or a part of it according to \samp{what} specification.
#DETAILS
#KEYWORDS 
#INPUTS
#{gema} <<\samp{gema} object to be printed.>>
#[INPUTS]
#{what} <<a \samp{character(1)}; when comprising "m" the 
#         expectations are printed, "l" the linear combinations
#         are printed.>>
#{ordering} << Nodes are given following the indices of "ordering" if \samp{numeric}
#         or the names if it is \samp{character}. \samp{NULL} means the
#         identity permutation. Repetitions or missing nodes are accepted.>>
#{digits} << when not null, the number of digits for rounding.>>
#{printed} << \samp{TRUE} to issue a printing, if not the prepared matrix
#           is returned.>>
#{check} << Must a check of the argument be performed?>>
#VALUE
# The \samp{gema} is printed or a matrix having a number of rows equal to 
# the number of nodes is returned
#  binding the elements which are precised in the argument \samp{what}.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# print8gema(rbmn0$gema1$v,check=TRUE);
# print8gema(rbmn0$gema2$v,"m",check=TRUE);
# print8gema(rbmn0$gema3$v,"l",digit=1,check=TRUE);
# print8gema(rbmn0$gema4$v,printed=FALSE,check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 12_01_19
#REVISED 12_02_16
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8gema(gema);
    if (!sssvoid9(che)) {
      print(che);
      stop("The provided 'gema' is not valid!");
    }
  }
  # number of nodes
  nn <- length(gema$mu);
  nam <- names(gema$mu);
  # getting the ordering for the nodes
  ordering <- aux3(nam,ordering);
  nnr <- length(ordering);
  # initializing
  cnam <- character(0);
  res <- matrix(NA,nn,0);
  # printing each asked option
  if (sssbelong9("m",what)) {
    cnam <- c(cnam,"mu");
    res <- cbind(res,gema$mu);
  }
  if (sssbelong9("l",what)) {
    cnam <- c(cnam,paste("E",sssbc(nnr),sep=""));
    if (length(union(ordering,sssbc(nn))==nn)) {
      uop <- ordering;
    } else {
      uop <- sssbc(nn);
    }
    res <- cbind(res,gema$li[,uop]);
  }
  # permuting
  res <- res[ordering,,drop=FALSE];
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
check8gema <- function(gema)
#TITLE checks a /gema/ object
#DESCRIPTION checks the consistency of \samp{gema} as a /gema/ object
# issues a fatal error with some clues if inconsistent.
#DETAILS
# Looking a the code of this function provides a way to know which
# are the requirements of a /chain/ object.
#KEYWORDS 
#INPUTS
#{gema} << The \samp{gema} object to check.>>  
#[INPUTS]
#VALUE
# \samp{character(0)} or a \samp{character} containing some clue
# about the discovered inconsistency.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# check8gema(rbmn0$gema1$v);
# res <- check8gema(rbmn0$adja1$v);
# if (is.na(as.logical(res))) { print(res);}
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 13_07_26
#REVISED 13_07_30
#--------------------------------------------
{
  # initial checking
  if (!is.list(gema)) {
    return("A /gema/ must be a list");
  }
  nage <- c("mu","li");
  if (!setequal(names(gema),nage)) {
    return(paste("Names of every /gema/ list must be:",nage,collapse=" "));
  }
  # getting the node numbers
  nbno <- length(gema$mu);
  # checking the existence of node names
  nano <- names(gema$mu);
  if (length(unique(nano)) < nbno) {
    return("Proposed names for the nodes doesn't fit");
  }
  # more checks
  res <- character(0);
  if (!is.numeric(gema$mu)) {
    res <- c(res,"$mu must be numeric");
  }
  if (!is.numeric(gema$li)) {
    res <- c(res,"$li must be numeric");
  }
  if (!is.matrix(gema$li)) {
    res <- c(res,"$li must be a matrix");
  } else {
    if (!setequal(nano,dimnames(gema$li)[[1]])) {
      res <- c(res,"$li must have node names in rows"); 
    }
    if (nrow(gema$li)!=ncol(gema$li)) {
      res <- c(res,"$li must be a squared matrix"); 
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
simulate8gema <- function(gema,nbs,check=rbmn0$check$v) 
#TITLE simulates from a /gema/ object
#DESCRIPTION returns a matrix of simulated
# values with the variable in columns and the 
# simulations in rows.
#DETAILS
# Just the application of the standard formula
# to a white noise. Variables names are taken
# from those of \samp{gema$mu}, when these
# does not exist, standard ones are provided.
#KEYWORDS 
#INPUTS
#{gema}<< The \samp{gema} object.>>
#{nbs}<< number of simulations to return.>>
#[INPUTS]
#{check} << Must a check of the argument be performed?>>
#VALUE
# A matrix of size : \samp{nbs x length(gema$mu)}
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# simulate8gema(rbmn0$gema1$v,10,check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE Include the checking of the topological order
#AUTHOR J.-B. Denis
#CREATED 12_04_27
#REVISED 12_12_03
#--------------------------------------------
{
  # checking
  if (check) {
    cheche <- check8gema(gema);
    if (!sssvoid9(cheche)) {
      ssserreur(cheche,"Not a valid /gema/");
    }
  }
  # number of variables and their names
  nbv <- length(gema$mu);
  # 
  if (is.null(names(gema$mu))) {
    va <- paste("V",as.character(sssbc(nbv)),sep="");
  } else {
    va <- names(gema$mu);
  }
  # number of simulations
  nbs <- round(max(0,nbs));
  # simulating
  if (nbv*nbs > 1) {
    res <- matrix(rnorm(nbs*nbv),nbv,nbs);
    res <- gema$mu + gema$li %*% res;
    res <- t(res);
  } else {
    res <- matrix(NA,nbs,nbv);
  }
  # adding the variable names
  dimnames(res) <- list(NULL,va);
  #
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
simulate8nbn <- function(nbn,nbs,cova=NULL,check=rbmn0$check$v) 
#TITLE simulates from a /nbn/ object
#DESCRIPTION returns a matrix of simulated
# values with the variable in columns and the 
# simulations in rows.
#DETAILS
# Just the sequential simulations of the nodes.
# The nodes must be in topological order.
#KEYWORDS 
#INPUTS
#{nbn}<< The \samp{nbn} object.>>
#{nbs}<< number of simulations to return.>>
#[INPUTS]
#{cova} << Possibly imposing some values to a root 
#          set of nodes (\samp{NULL} if not).
#  Let \samp{nc} be the number of such nodes. \samp{cova}
#  must be either a \samp{nbsxnc} matrix (with
#  column names associated to the node names) or a
#  named vector (the names being the node names) of the values
#  to give to the nodes. When a vector, all covariate nodes
#  will have identical values, when a matrix the values can
#  change from one simulation to another.>>
#{check} << Must a check of the argument be performed?>>
#VALUE
# A matrix of size : \samp{nbs x length(nbn)}
# So if root nodes are imposed they are in the output.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# simulate8nbn(rbmn0$nbn1$v,10,check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE Check and impose the topological order
#AUTHOR J.-B. Denis
#CREATED 12_04_27
#REVISED 14_04_03
#--------------------------------------------
{
  # checking
  if (check) {
    cheche <- check8nbn(nbn);
    if (!sssvoid9(cheche)) {
      ssserreur(cheche,"Not a valid /nbn/");
    }
  }
  # number of variables and their names
  nbv <- length(nbn);
  va <- names(nbn);
  # number of simulations
  nbs <- round(max(0,nbs));
  # initializing
  res <- matrix(NA,nbs,nbv);
  dimnames(res) <- list(NULL,va);
  # preparing the covariate step
  if (!is.null(cova)) {
    if (is.matrix(cova)) {
      if (nrow(cova)!=nbs) {
        stop("'nrow(cova)'",nrow(cova),"and 'nbs'",nbs,"are different");
      }
      ncova <- dimnames(cova)[[2]];
      for (cc in ncova) {
        if (cc %in% va) {
          res[,cc] <- cova[,cc];
        } else {
          stop(paste(cc,"from 'cova' not in 'names(nbn)':",
                     paste(va,collapse=" ")));
        }
      }
    } else {
      ncova <- names(cova);
      for (cc in ncova) {
        if (cc %in% va) {
          res[,cc] <- cova[cc];
        } else {
          stop(paste(cc,"from 'cova' not in 'names(nbn)':",
                     paste(va,collapse=" ")));
        }
      }
    }
  } else {
    ncova <- character(0);
  }
  # simulating
  if (nbv*nbs > 1) {
    for (vv in names(nbn)) { if (!(vv %in% ncova)) {
      res[,vv] <- rnorm(nbs,nbn[[vv]]$mu,nbn[[vv]]$sigma);
      for (pp in sssbf(nbn[[vv]]$parents)) {
        pn <- nbn[[vv]]$parents[pp];
        res[,vv] <- res[,vv] + nbn[[vv]]$regcoef[pp] * res[,pn];
      }
    }}
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
string7dag4nbn <- function(nbn,sep=";",check=rbmn0$check$v) 
#TITLE provides so-called string model of a /nbn/
#DESCRIPTION returns a \samp{character(1)}
# describing the dag of the nbn under
# the string form.
#DETAILS
#KEYWORDS 
#INPUTS
#{nbn}<< The nbn.>>
#[INPUTS]
#{sep}<<Separation sign between parents after the
#       conditioning sign (\samp{|}).>>
#{check} << Must a check of the argument be performed?>>
#VALUE
# A \samp{character(1)}.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# string7dag4nbn(rbmn0$nbn1$v,check=TRUE);
# string7dag4nbn(rbmn0$nbn4$v,sep=",",check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE Do it through two new functions 'on-work': \samp{adja2string7dag} and \samp{string7dag2adja}
#AUTHOR J.-B. Denis
#CREATED 12_11_29
#REVISED 12_11_29
#--------------------------------------------
{
  # checking
  if (check) {
    cheche <- check8nbn(nbn);
    if (!sssvoid9(cheche)) {
      ssserreur(cheche,"Not a valid /nbn/");
    }
  }
  # initializing
  res <- character(0);
  # adding the nodes in turn
  for (nn in sssbf(nbn)) {
    res <- paste(res,"[",names(nbn)[nn],sep="");
    pp <- nbn[[nn]]$parents;
    if (length(pp) >= 1) {
      res <- paste(res,"|",pp[1],sep="");
      for (ip in sssbd(2,length(pp))) {
        res <- paste(res,sep,pp[ip],sep="");
      }
    }
    res <- paste(res,"]",sep="");
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
diff8nbn <- function(nbn1,nbn2,type=1,scalar=TRUE,check=rbmn0$check$v)
#TITLE returns a score of the difference between two /nbn/s
#DESCRIPTION 
# Returns a positive scalar value measuring, in some way, the difference
# existing within two /nbn/s sharing the same structure.\cr
#DETAILS
# For \samp{type==1} it is the canonical euclidian difference between
# all parameters, including the \samp{sigma}.
# The score to use to measure the differences between two successive
# estimations is not well established (see the code).
#KEYWORDS 
#INPUTS
#{nbn1} <<First \samp{nbn} object.>>
#{nbn2} <<Second \samp{nbn} object.>>
#[INPUTS]
#{type}<<When 1, the score includes the difference between the sigmas.
#        When -1, sigmas are not taken into account.>>
#{scalar}<<When \samp{TRUE} the squared norm is returned, if not the vector of difference.>>
#{check} << Must a check of the argument be performed?>>
#VALUE
# Either a scalar or a named vector (according to \samp{scalar}).
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# diff8nbn(rbmn0$nbn1$v,rbmn0$nbn1$v,check=TRUE);
# diff8nbn(rbmn0$nbn1$v,rbmn0$nbn1$v,scalar=FALSE,check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 13_04_25
#REVISED 13_07_30
#--------------------------------------------
{
  # checking
  if (check) {
    cheche <- check8nbn(nbn1);
    if (!sssvoid9(cheche)) {
      ssserreur(cheche,"/nbn1/: not a valid /nbn/");
    }
    cheche <- check8nbn(nbn2);
    if (!sssvoid9(cheche)) {
      ssserreur(cheche,"/nbn2/: not a valid /nbn/");
    }
    adja1 <- adja4nbn(nbn1,check=FALSE);
    adja2 <- adja4nbn(nbn2,check=FALSE);
    if (sum((adja1-adja2)^2) > 0.00001) {
      ssserreur(list(adja1,adja2),"'nbn1' and 'nbn2' don't have identical stuctures.");
    }
  }
  nona <- names(nbn1);
  # constituting the vector of differences.
  didi <- numeric(0);
  for (nn in sssbf(nona)) {
    suff <- nona[nn];
    n0 <- length(didi);
    didi <- c(didi,nbn2[[nn]]$mu-nbn1[[nn]]$mu);
    nana <- paste(suff,"mu",sep=".");
    if (type > 0) {
      didi <- c(didi,nbn2[[nn]]$sigma-nbn1[[nn]]$sigma);
      nana <- c(nana,paste(suff,"sigma",sep="."));
    }
    if (length(nbn2[[nn]]$parents)>0) {
      didi <- c(didi,nbn2[[nn]]$regcoef-nbn1[[nn]]$regcoef);
      nana <- c(nana,paste(suff,nbn2[[nn]]$parents,sep="."));
    }
    names(didi)[sssbd(n0+1,length(didi))] <- nana;
  }
  # returning
  if (scalar) {
    return(sum(didi^2));
  } else {
    return(didi);
  }
  invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
