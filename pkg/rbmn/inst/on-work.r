#
# In this file are new functions under elaboration
#                  or existing function to be improved.
#

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
topo4gema <- function(gema,ord=NULL,check=rbmn0$check$v)
#TITLE topological order of a /gema/
#DESCRIPTION returns one of the orders of the nodes
# such as the parents of any node are less ranked than
# it when \samp{is.null(ord)}. If not check that the proposed order
# is either a right permutation (\samp{is.numeric(ord)}) or
# a vector of node names providing a topological order
# (\samp{is.character(ord)}).
#DETAILS
# When \samp{!is.null(ord)} the order must be an order, if
# not an error is issued.
#KEYWORDS 
#INPUTS
#{gema} <<\samp{gema} object for which the order must be computed.>>
#[INPUTS]
#{ord} <<\samp{NULL} or an order to test as a permutation or
#        a vector of names.>>
#{check} << Must a check of the argument be performed?>>
#VALUE
# a permutation vector of the nodes of the /gema/
#        or a named list with the nodes not having
#        their parents before them. That is a topological order.
#EXAMPLE
#  names(rbmn0$gema4$v$mu)[topo4gema(rbmn0$gema4$v)];
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE 'ord' option is bad
#AUTHOR J.-B. Denis
#CREATED 12_01_14
#REVISED 12_01_17
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8gema(gema);
    if (!void9(che)) {
      print(che);
      stop("The provided 'gema' is not valid!");
    }
    if (!is.null(ord)) {
      object9(ord,c("numeric","character"),length(gema$mu));
      if (is.numeric(ord)) {
        if (length(unique(c(bc(length(gema$mu)),ord)))!=length(gema$mu)) {
          erreur(ord,"Not a permutation");
        }
        if (length(unique(c(names(gema$mu),ord)))!=length(gema$mu)) {
          str(gema);
          erreur(ord,"Not a permutation of the names of the gema");
        }
      }
    }
  }
  eps <- 10^-10;
  li <- gema$li;
  # number of nodes
  nn <- nrow(li);
  nam <- dimnames(li)[[1]];
  if (is.null(ord)) {
    # getting the order
    res <- numeric(0);
    while (length(res) < nn) {
      for (ii in bc(nn)) {
        if (!(ii %in% res)) {
          nbpar <- sum(abs(li[ii,])>eps) - 1;
          if (nbpar==0) {
            res <- c(res,ii);
            li[,ii] <- 0;
          }
        }
      }
    }
  } else {
    return("Sorry, the 'ord' option is wrong and must be corrected!");
    # checking the proposed permutation
    if (is.numeric(ord)) {
      if (length(union(ord,bc(nn)))!=nn) {
        erreur(ord,message="'ord' is not a permutation.")
      }
      ord <- nam[ord];
    } else {
      if (length(union(ord,nam))!=nn) {
        erreur(list(ord,nam),
               message="'ord' is not a permutation of 'nam'");
      }
    }
    # checking the order
    res <- vector("list",0); kk <- 0; nm <- character(0);
    parents <- character(0);
    for (ii in bc(nn)) {
      parents <- c(parents,nam[ii]);
      wpar <- which(abs(gema$li[ii,])>eps);
      pare <- nam[wpar];
      if (length(union(pare,parents))>length(parents)) {
        kk <- kk + 1;
        res[[kk]] <- pare;
        nm <- c(nm,nam[ii]);
        names(res) <- nm;
      }
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rm8nd4nbn <- function(nbn,nodes,check=rbmn0$check$v)
#TITLE removes some nodes from a /nbn/
#DESCRIPTION returns a /nbn/ object deduced from
# an original /nbn/ by integrating on a given
# subset of nodes.
#DETAILS
# The transformation is made through the associated
# joint distributions for the probabilities and with
# the help of the function \samp{rm8nd4adja} 
# for the relationships.
#KEYWORDS
#INPUTS
#{nbn}<< The \samp{nbn} object to reduce.>>
#{nodes} <<\samp{character} or \samp{numeric} vector
#           giving the subset of nodes to remove.>>
#[INPUTS]
#{check} << Must a check of the argument be performed?>>
#VALUE
# The resulting \samp{nbn}.
#EXAMPLE
# rm8nd4nbn(rbmn0$nbn4$v,"1.1"); 
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE to be done properly!
#AUTHOR J.-B. Denis
#CREATED 12_05_16
#REVISED 12_07_27
#--------------------------------------------
{
  # checking
  return("the presently proposed algorithm is false, see 'margina.r' to check it");
  if (check) {
    cheche <- check8nbn(nbn);
    if (!void9(cheche)) {
      erreur(cheche,"Not a valid /nbn/");
    }
  }
  # getting the ordering subset as a numeric
  if (is.numeric(nodes)) {
    ss <- sort(nodes);
    if ((max(ss) > length(nbn)) | (min(ss) < 1)) {
      erreur(list(length(nbn),nodes),"Not a subset (1)");
    }
  } else {
    ss <- sort(match(nodes,names(nbn)));
    if (length(ss)!=length(nodes)) {
      erreur(list(names(nbn),nodes),"Not a subset (2)");
    }
  }
  nbno <- length(nbn);
  # getting the structure
  pam <- adja4nbn(nbn);
  pam <- rm8nd4adja(pam,ss);
  # getting the parameters
  rr <- sort(setdiff(bc(nbno),ss));
  para <- gema2mn(nbn2gema(nbn));
  para <- list(mu=para$mu[rr],
               gamma=para$gamma[rr,rr,drop=FALSE]
              );
  # reconsistuting the nbn from pam and para
  res <- vector("list",length(rr));
  names(res) <- names(nbn)[rr];
  for (nn in bf(rr)) {
    # the parents
    rrpa <- which(pam[,nn]==1);
    res[[nn]]$parents <- names(res)[rrpa];
    # the parameters values
    if (length(rrpa)==0) {
      # no parents
      mumu <- para$mu[nn];
      gaga <- para$gamma[nn,nn];
      coco <- numeric(0);
    } else {
      # parents
      gvv <- solve(para$gamma[rrpa,rrpa,drop=FALSE]);
      guv <- para$gamma[nn,rrpa,drop=FALSE];
      mup <- matrix(para$mu[rrpa],length(rrpa));
      mumu <- para$mu[nn] - guv %*% gvv %*% mup;
      gaga <- guv %*% gvv %*% t(guv);
      coco <- guv %*% gvv;
    }
    res[[nn]]$mu <- as.vector(mumu);
    res[[nn]]$sigma <- as.vector(sqrt(gaga));
    res[[nn]]$regcoef <- as.vector(coco);
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nbn2pam <- function(nbn) 
#TITLE gets the /pam/ associate to a /nbn/
#DESCRIPTION returns a \samp{pam} object of /g4n/ 
# package with the parentship of \samp{nbn}.
#DETAILS
#KEYWORDS 
#INPUTS
#{nbn}<< The initial \samp{nbn} object.>>
#[INPUTS]
#VALUE
# A \samp{pam} object
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 12_05_16
#REVISED 12_05_16
#--------------------------------------------
{
  # checking
  # To be done
  # getting the parentship matrix
  nbno <- length(nbn);
  nbna <- names(nbn);
  rlt <- matrix(0,nbno,nbno);
  for (nn in bf(nbn)) {
    if (length(nbn[[nn]]$parents) > 0) {
      rlt[match(nbn[[nn]]$parents,nbna),nn] <- 1;
    }
  }
  # creating the pam object
  res <- new("pam",rlt=rlt);
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
chain4chain <- function(chain,nodes,
                        condi=numeric(0),
                        value=rep(0,length(condi)),check=rbmn0$check$v)
#TITLE extracts a chain from a chain
#DESCRIPTION returns the chain obtained
# from \samp{chain} retaining only nodes indicated
# by \samp{nodes} and conditioned with nodes
# indicated in \samp{condi}.
#DETAILS
# Integration is done for nodes not belonging to
# the extracted chain nor being in the conditioning
# subset. Then the distribution of the retained nodes
# is left identical to this in the initial chain (when 
# there is no conditioning).
#KEYWORDS 
#INPUTS
#{chain}<< The chain object to consider.>>
#[INPUTS]
#{nodes} << \samp{numeric} (or \samp{character}) vector
#         giving the numbers (or names) of the nodes
#         to be retained in the extracted chain.>>
#{condi} << \samp{numeric} (or \samp{character}) vector
#         giving the numbers (or names) of the
#         conditioning nodes for the extracted chain.>>
#{value} << Numerical values associated to \samp{condi}.>>
#{check} << Must a check of the arguments be performed?>>
#VALUE
# The resulting chain
#EXAMPLE
# chain4chain(rbmn0$chain2$v,c("a","d"),c("b"),12);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE To be rewritten
#AUTHOR J.-B. Denis
#CREATED 11_02_08
#REVISED 11_02_08
#--------------------------------------------
{
  return("This function is under construction!");
  # checking
  # <to be done>
  # constants
  nn <- length(chain$names);
  # each case in turn
  fait <- FALSE;
  if (is.null(head) & is.null(tail)) {
    # no condition
    res <- chain;
    fait <- TRUE;
  }
  #
  if (!is.null(head) & !is.null(tail)) {
    # both conditioning
    if (nn < 3) {
      res <- NULL;
    } else {
      res <- "'a faire";
    fait <- TRUE;
    }
  }
  #
  if (!fait) {
    if (nn < 2) {
      res <- NULL;
    } else {
      if (is.null(tail)) { chain <- rev8chain(chain);}
      # condioning has to be done for the last node
      # looking for the last root
      lro <- max(which(state4chain(chain)=="r"));
      ncor <- aux8(chain$corre[bd(lro,nn)]);
      ncor <- diag(ncor[-1,-(nn-lro),drop=FALSE]);
      nmu <- chain$mu[-nn];
      nsi <- chain$sigma[-nn];
      res <- list(names=chain$names[-nn],
                  roots=chain$roots,
                  colliders=chain$colliders,
                  mu=nmu,
                  sigma=nsi,
                  corre=ncor
                 );
      fait <- TRUE;
      if (is.null(tail)) { res <- rev8chain(res);}
    }
  }      
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
topo4chain <- function(chain,ord=NULL,check=rbmn0$check$v)
#TITLE returns a topological order of a /chain/ or checks a proposed order.
#DESCRIPTION From a \samp{chain} object
# returns one of the possible topological orders,
# through a permutation when \samp{is.null(ord)}.
# If not \samp{ord} must be a proposed order to be
# checked given as a permutation if \samp{is.numeric(ord)}
# or a vector of ordered names if \samp{is.character(ord)}.
#DETAILS
# For the moment the \samp{ord} option is
# bad and an error message is returned when used.
#KEYWORDS 
#INPUTS
#{chain}<< the \samp{chain} object to be considered.>>
#[INPUTS]
#{ord} << Indicates what must be done. \samp{NULL} to get a topological
# order associated to the chain otherwise a permutation to be checked as
# one of the possible topological orders of the chain.>>
#{check} << Must a check of the arguments be performed?>>
#VALUE
# a permutation vector of the nodes of the /nbn/
#        or a named character with the nodes not having
#        their parents before them; when it is of
#        length zero this means that the check 
#        was successful.
#EXAMPLE
# topo4chain(rbmn0$chain2$v);
# topo4chain(rbmn0$chain2$v,topo4chain(rbmn0$chain2$v));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE Correct the 'ord' option!
#AUTHOR J.-B. Denis
#CREATED 12_01_31
#REVISED 12_01_31
#--------------------------------------------
{
  C'est ord qui ne fonctionne pas et est à faire'
  # checking
  if (check) {
    che <- check8chain(chain);
    if (length(che)>0) {
      print(che);
      stop("The provided 'chain' is not valid!");
    }
    if (!is.null(order)) {
      if (!all(sort(order)==bf(order))) {
        erreur(order,"'order' is not a permutation");
      }
    }
  }
  # getting the permutation
  ooo <- aux2(chain,TRUE);
  if (is.null(ord)) {
    res <- ooo;
  } else {
    return("Sorry, the 'ord' option is wrong and must be corrected!");
    nam <- chain$names;
    nn <- length(nam);
    if (is.numeric(ord)) {
      # a numeric permutation is expected
      if (length(union(ord,bc(nn)))!=nn) {
        erreur(ord,message="'ord' is not a permutation.")
      }
    } else {
      # a list of names is expected
      if (length(union(ord,nam))!=nn) {
        erreur(list(ord,nam),
               message="'ord' is not a permutation of 'nam'");
      }
    ord <- posi9(ord,nam);
    }
    # checking and storing inconsistencies
    res <- numeric(0); nm <- character(0);
    names(res) <- nm;
    etat <- state4chain(chain);
    for (uu in ord) {
      if (etat[uu] == "r") {
        if (uu > 1) {
	    if (etat[uu-1] == "t") { etat[uu-1] <- "r";}
	    if (etat[uu-1] == "c") { etat[uu-1] <- "t";}
        }
        if (uu < nn) {
	    if (etat[uu+1] == "t") { etat[uu+1] <- "r";}
	    if (etat[uu+1] == "c") { etat[uu+1] <- "t";}
        }
      } else {
	  res <- c(res,uu);
          nm <- c(nm,nam[uu]);
          names(res) <- nm;
      }
      etat[uu] <- "f";
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
read8nbn <- function(file,check=rbmn0$check$v)
#TITLE reads from a text file a /nbn/ object.
#DESCRIPTION reads from the text file \samp{file}
# following a precised format an object describing
# in the natural way a normal Bayesian network.\cr
# For a /nbn/ with \samp{n} nodes, the file comprises 
# \samp{n} main fields each one associated to a different
# node. The order is supposed to be the choosen order;
# it must be a topological one, if not a topological
# order will be found and imposed. Names of the nodes are
# the field names.\cr
# Each node field comprises four possible subfields. \samp{mu}, 
# the conditional expectation of the node when all its parents
# has got a zero value; default value \samp{0}.
#  \samp{sigma}, the conditional standard deviation of the node;
# default value \samp{1}. \samp{parents}, the list of possible parents
# (indicated by their names); default value \samp{NULL}. \samp{regcoef},
# values of the regression associated to the parents in the same order;
# default values: \samp{1} for each.
#DETAILS
# The function \samp{read8list} is called to read the file so standard
# tags \samp{<<...>>} and \samp{[[...]]} can be replaced according the
# user preferences.\cr
# Comments and more things can be introduced in the file, see
# the \samp{read8list} documentation for full details.\cr
# For the moment, if the implicit graph proposed in the file
# comprises loops, this is not detected and provokes an
# infinite looping of the function!
#KEYWORDS 
#INPUTS
#{file} <<file to be read.>>
#[INPUTS]
#{check} << Must a check of the arguments be performed?>>
#VALUE
# a /nbn/ object, for the moment a mere list.
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE Checks cycles to avoid infinite loops
#AUTHOR J.-B. Denis
#CREATED 12_01_14
#REVISED 12_01_16
#--------------------------------------------
{
  # checking
  if (check) {
    if (fidi9(file)!="f") {
      erreur(file,"It is not an valid file!");
    }
  }
  # getting the list
  uu <- read8list(file);
  # node number
  nn <- length(uu);
  # completing each node with the default values
  for (ii in bc(nn)) {
    if (void9(uu[[ii]])) {
      uu[[ii]] <- vector("list",0);
    }
    if (void9(uu[[ii]]$parents)) {
      uu[[ii]]$parents <- character(0);
      uu[[ii]]$regcoef <- numeric(0);
    } else {
      if (void9(uu[[ii]]$regcoef)) {
        uu[[ii]]$regcoef <- rep(1,length(uu[[ii]]$parents));
      } else {
        uu[[ii]]$regcoef <- as.numeric(uu[[ii]]$regcoef);
      }
    }
    if (void9(uu[[ii]]$mu)) {
      uu[[ii]]$mu <- 0;
    } else {
      uu[[ii]]$mu <- as.numeric(uu[[ii]]$mu);
    } 
    if (void9(uu[[ii]]$sigma)) {
      uu[[ii]]$sigma <- 1;
    } else {
      uu[[ii]]$sigma <- as.numeric(uu[[ii]]$sigma);
    }
  }
  # checking the proposed order
  voir <- topo4nbn(uu,bc(nn));
  if (length(voir)>0) {
    # modifying the proposed order
    erreur(names(uu),"Not a topological order, was changed",w=TRUE);
    nord <- topo4nbn(uu);
    vv <- uu;
    for (ii in bc(nn)) {
      uu[ii] <- vv[nord[ii]];
      names(uu) <- names(vv)[nord];
    }
  }
  # returning
  uu;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
string7dag2adja <- function(adja,sep=";",check=rbmn0$check$v) 
#TITLE TO BE DONE AND INCLUDED IN 'string7dag4nbn'
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
# string7dag4nbn(rbmn0$nbn1$v);
# string7dag4nbn(rbmn0$nbn4$v,sep=",");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE Do it through two new functions 'on-work': \samp{adja2string} and \samp{string2adja}
#AUTHOR J.-B. Denis
#CREATED 14_08_24
#REVISED 14_08_24
#--------------------------------------------
{
  # checking
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
adja2string7dag <- function(adja,sep=";",check=rbmn0$check$v) 
#TITLE TO BE DONE AND INCLUDED IN 'string7dag4nbn'
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
# string7dag4nbn(rbmn0$nbn1$v);
# string7dag4nbn(rbmn0$nbn4$v,sep=",");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE Do it through two new functions 'on-work': \samp{adja2string} and \samp{string2adja}
#AUTHOR J.-B. Denis
#CREATED 14_08_24
#REVISED 14_08_24
#--------------------------------------------
{
  # checking
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
