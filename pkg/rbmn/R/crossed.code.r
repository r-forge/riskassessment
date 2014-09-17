
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
crossed4nbn1nbn <- function(nbn1,nbn2,
                        we1=rep(1,length(nbn1)),
                        we2=rep(1,length(nbn2)),
                        nona=as.vector(outer(names(nbn1),
                                             names(nbn2),paste,sep="_")),
                            check=rbmn0$check$v)
#TITLE creates a crossed-nbn from two /nbn/s
#DESCRIPTION
# A crossed /nbn/ is a /nbn/ obtained when replacing
# each node of the first /nbn/ with the second /nbn/ and
# vice-versa.\cr
# Let \samp{nn1/nn2} and \samp{na1/na2} be the node and arc
# numbers of the two \samp{nbn}s, the node number of the
# crossed \samp{nbn} is \samp{nn1*nn2} and its arc number
# is \samp{nn1*na2+nn2*na1}.\cr
# The regression coefficients attributed to the crossed \samp{nbn}
# are the products of the weights (\samp{we1/we2}) and the regression
# coefficients of the initial \samp{nbn}s.
#DETAILS
# The \samp{mu} coefficient is the sum of the two corresponding \samp{mu}s
# of the generating \samp{nbn}.\cr
# The \samp{sigma} coefficient is the product of the two corresponding \samp{sigma}s
# of the generating \samp{nbn}.\cr
# The regression coefficient are directed inherited from the \samp{nbn}
# which is duplicated with this arc.
#KEYWORDS 
#INPUTS
#{nbn1}<< The first generating /nbn/.>>
#{nbn2}<< The second generating /nbn/.>>
#[INPUTS]
#{we1}<< The weight to apply to the nodes of the first generating /nbn/.>>
#{we2}<< The weight to apply to the nodes of the second generating /nbn/.>>
#{nona}<< The node names to give to the crossed /nbn/, the nodes of the
# \samp{nbn1} varying first.>>
#{check} << Must a check of the arguments be performed?>>
#VALUE
# The resulting crossed \samp{nbn} object.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# print8nbn(crossed4nbn1nbn(rbmn0$nbn1$v,rbmn0$nbn4$v,check=TRUE),check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 13_04_22
#REVISED 13_04_24
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8nbn(nbn1);
    che <- c(che,check8nbn(nbn2));
    if (length(che)>0) {
      print(che);
      stop("The provided 'nbn1' and/or 'nbn2' is/are not valid!");
    }
  }
  # constants
  nn1 <- length(nbn1); nn2 <- length(nbn2);
  if (length(nona) != nn1*nn2) {
    ssserreur(list(nn1,nn2,length(nona)),
           message="Bad correspondence with the proposed names"
          );
  }
  nuij <- function(ii,jj,nlig) {ii + nlig*(jj-1);}
  # initializing
  res <- vector("list",nn1*nn2);
  names(res) <- nona;
  # making each crossed node in turn
  for (ii in sssbc(nn1)) {
    ip1 <- match(nbn1[[ii]]$parents,names(nbn1));
    for (jj in sssbc(nn2)) {
      kk <- nuij(ii,jj,nn1);
      #cat(names(nbn1)[ii],names(nbn2)[jj],":",nona[kk],"\n");
      res[[kk]]$mu    <- nbn1[[ii]]$mu    + nbn2[[jj]]$mu;
      res[[kk]]$sigma <- nbn1[[ii]]$sigma * nbn2[[jj]]$sigma;
      ip2 <- match(nbn2[[jj]]$parents,names(nbn2));
      res[[kk]]$parents <- character(0);
      res[[kk]]$regcoef <- numeric(0);
      for (ij in sssbf(ip1)) {
        nupa <- nona[nuij(ip1[ij],jj,nn1)];
        res[[kk]]$parents <- c(res[[kk]]$parents,nupa);
        res[[kk]]$regcoef <- c(res[[kk]]$regcoef,we2[jj]*nbn1[[ii]]$regcoef[ij]);
      }
      for (ij in sssbf(ip2)) {
        nupa <- nona[nuij(ii,ip2[ij],nn1)];
        #cat(names(nbn1)[ii],names(nbn2)[ij],":",nupa,"\n");
        res[[kk]]$parents <- c(res[[kk]]$parents,nupa);
        res[[kk]]$regcoef <- c(res[[kk]]$regcoef,we1[ii]*nbn2[[jj]]$regcoef[ij]);
      }
      #pause(paste("<<",nona[kk],">>"));
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
arcs4nbn1nbn <- function(nbn1,nbn2,type="a1",
                        nona=as.vector(outer(names(nbn1),
                                             names(nbn2),paste,sep="_")),
                         check=rbmn0$check$v)
#TITLE returns the list of 'parallel' arcs of a crossed-nbn
#DESCRIPTION
# Returns a list of matrices with two columns (as needed by \samp{estimate8constrained7nbn})
# indicating corresponding arcs for each arcs/nodes of \samp{nbn1} (or \samp{nbn2}) of the 
# crossed /nbn/ obtained when crossing /nbn1/ and /nbn2/ with node names given by \samp{nona}.
#DETAILS
#KEYWORDS 
#INPUTS
#{nbn1}<< The first generating /nbn/.>>
#{nbn2}<< The second generating /nbn/.>>
#[INPUTS]
#{type}<< \samp{"a1"} to indicate that the parallelism must be done for each arc of \samp{nbn1};
#         \samp{"a2"} for each arc of \samp{nbn2};
#         \samp{"n1"} for each node of \samp{nbn1};
#         \samp{"n2"} for each node of \samp{nbn2}.
#>>
#{nona}<< The node names to give to the crossed /nbn/, the nodes of the
# \samp{nbn1} varying first.>>
#{check} <<Must the arguments be checked?>>
#VALUE
# The resulting named (after node names) list of matrices.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# print(arcs4nbn1nbn(rbmn0$nbn1$v,rbmn0$nbn4$v,"a1",check=TRUE));
# print(arcs4nbn1nbn(rbmn0$nbn1$v,rbmn0$nbn4$v,"n1",check=TRUE));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 13_04_26
#REVISED 13_04_26
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8nbn(nbn1);
    che <- c(che,check8nbn(nbn2));
    if (length(che)>0) {
      print(che);
      stop("The provided 'nbn1' and/or 'nbn2' is/are not valid!");
    }
  }
  # constants
  nn1 <- length(nbn1); nn2 <- length(nbn2);
  if (length(nona) != nn1*nn2) {
    ssserreur(list(nn1,nn2,length(nona)),
           message="Bad correspondence with the proposed names"
          );
  }
  nuij <- function(ii,jj,nlig) {ii + nlig*(jj-1);}
  fait <- FALSE;
  # 
  if (type=="n1") {
    # initializing
    res <- vector("list",nn1);
    names(res) <- names(nbn1);
    # making each crossed node in turn
    for (ii in sssbc(nn1)) {
      res[[ii]] <- matrix(NA,0,2);
      for (jj in sssbc(nn2)) {
        head <- nona[nuij(ii,jj,nn1)];
        ip2 <- match(nbn2[[jj]]$parents,names(nbn2));
        for (ij in sssbf(ip2)) {
          tail <- nona[nuij(ii,ip2[ij],nn1)];
          res[[ii]] <- rbind(res[[ii]],c(tail,head));
        }
      }
    }
    fait <- TRUE;
  }
  #
  if (type=="n2") {
    # initializing
    res <- vector("list",nn2);
    names(res) <- names(nbn2);
    # making each crossed node in turn
    for (ii in sssbc(nn2)) {
      res[[ii]] <- matrix(NA,0,2);
      for (jj in sssbc(nn1)) {
        head <- nona[nuij(jj,ii,nn1)];
        ip1 <- match(nbn1[[jj]]$parents,names(nbn1));
        for (ij in sssbf(ip1)) {
          tail <- nona[nuij(ip1[ij],ii,nn1)];
          res[[ii]] <- rbind(res[[ii]],c(tail,head));
        }
      }
    }
    fait <- TRUE;
  }
  #
  if (type=="a1") {
    # getting the arcs of nbn1
    arcs1 <- adja2arcs(adja4nbn(nbn1,check=check),check=check);
    # initializing
    res <- vector("list",nrow(arcs1));
    names(res) <- paste(arcs1[,1],arcs1[,2],sep="->");
    # making each crossed node in turn
    for (ii in sssbf(res)) {
      res[[ii]] <- matrix(NA,0,2);
      ii1 <- match(arcs1[ii,1],names(nbn1));
      ii2 <- match(arcs1[ii,2],names(nbn1));
      for (jj in sssbc(nn2)) {
        headn <- nona[nuij(ii1,jj,nn1)];
        tailn <- nona[nuij(ii2,jj,nn1)];
        res[[ii]] <- rbind(res[[ii]],c(headn,tailn));
      }
    }
    fait <- TRUE;
  }
  # 
  if (type=="a2") {
    # getting the arcs of nbn2
    arcs2 <- adja2arcs(adja4nbn(nbn2));
    # initializing
    res <- vector("list",nrow(arcs2));
    names(res) <- paste(arcs2[,1],arcs2[,2],sep="->");
    # making each crossed node in turn
    for (jj in sssbf(res)) {
      res[[jj]] <- matrix(NA,0,2);
      jj1 <- match(arcs2[jj,1],names(nbn2));
      jj2 <- match(arcs2[jj,2],names(nbn2));
      for (ii in sssbc(nn1)) {
        headn <- nona[nuij(ii,jj1,nn1)];
        tailn <- nona[nuij(ii,jj2,nn1)];
        res[[jj]] <- rbind(res[[jj]],c(headn,tailn));
      }
    }
    fait <- TRUE;
  }
  if (!fait) {
    stop(paste("'type' =",type,"not recognized by 'arcs4nbn1nbn'"));
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
adja2crossed <- function(adj1,adj2,
                        nona=as.vector(outer(dimnames(adj1)[[1]],
                                             dimnames(adj2)[[1]],
                                             paste,sep="_")),
                         check=rbmn0$check$v)
#TITLE creates a crossed-adjacency matrix from two ones
#DESCRIPTION
# Like crossed4nbn1nbn but at the level of adjacency matrices. Must 
# be much efficient when regression coefficients are not needed.
#DETAILS
# Just two Kronecker products of matrices.
#KEYWORDS 
#INPUTS
#{adj1}<< The first adjacency matrix.>>
#{adj2}<< The second adjacency matrix.>>
#[INPUTS]
#{nona}<< The node names to give to the crossed /nbn/, the nodes of the
# \samp{nbn1} varying first.>>
#{check} << Must a check of the arguments be performed?>>
#VALUE
# The resulting crossed adjacency matrix.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# print(adja2crossed(rbmn0$adja1$v,rbmn0$adja1$v,check=TRUE));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 13_04_30
#REVISED 13_04_30
#--------------------------------------------
{
  # checking
  if (check) {
    sssobject9(adj1,"matrix",mensaje=" 'adj1' is not a matrix");
    sssobject9(adj2,"matrix",mensaje=" 'adj1' is not a matrix");
    if (nrow(adj1)!=ncol(adj1)) {
      ssserreur(dim(adj1)," 'adj1' must be a square matrix");
    }
    if (nrow(adj2)!=ncol(adj2)) {
      ssserreur(dim(adj2)," 'adj2' must be a square matrix");
    }
  }
  # constants
  nn1 <- nrow(adj1); nn2 <- nrow(adj2);
  if (length(nona) != nn1*nn2) {
    ssserreur(list(nn1,nn2,length(nona)),
           message="Bad correspondence with the proposed names"
          );
  }
  # computation
  res <- diag(nrow=nn2) %x% adj1 + adj2 %x% diag(nrow=nn1);
  dimnames(res) <- list(nona,nona);
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
generate8grid7crossed <- function(
                             nrow=3,ncol=3,xrow=1,xcol=1,
                             rhorow=matrix(0.5,nrow-1,ncol),
                             rhocol=matrix(0.5,ncol-1,nrow),
                             xrhorow=matrix(0.5,nrow,xrow),
                             xrhocol=matrix(0.5,ncol,xcol),
                             sigmas=matrix(1,nrow,ncol)
                                 )
#TITLE creates a grid crossed nbn with covariates
#DESCRIPTION
# Creates crossed \samp{nbn} following a grid of \samp{nrow} rows and \samp{ncol}
# columns; plus \samp{xrow} covariables associated to the first column nodes
# and \samp{xcol} covariables associated to the first row nodes.
#DETAILS
# Let \samp{Y(i,j)} the node of the i-th row and j-th column of the grid;
# \samp{i} varies from \samp{1} to \samp{nrow} and \samp{j} varies from \samp{1}
# to \samp{ncol}. The children of \samp{Y(i,j)} are \samp{Y(i+1,j)} and \samp{Y(i,j+1)}
# when these child nodes exist.\cr
# So called covariate nodes are \samp{XR(h)} (\samp{h} varying from \samp{1} to \samp{xrow}
# and \samp{XC(k)} (\samp{k} varying from \samp{1} to \samp{xcol}. The possible children of each
# \samp{XR(h)} are the \samp{nrow} nodes \samp{Y(i,1)} and those of each \samp{XC(k)} are
# the \samp{ncol} nodes \samp{Y(1,j)}.\cr
# No more children are possible: \samp{(nrow-1)*(ncol-1)} within the \samp{Y}s,
# \samp{xrow*nrow} from the \samp{XR}s and \samp{xcol*ncol} from the \samp{XC}s.\cr
# All marginal expectations are null. Marginal variances of the covariates are one.
#KEYWORDS 
#INPUTS
#[INPUTS]
#{nrow}<< Number of rows for the grid (>1).>>
#{ncol}<< Number of columns for the grid (>1).>>
#{xrow}<< Number of covariabes for the rows (>=0).>>
#{xcol}<< Number of covariates for the columns (>=0).>>
#{rhorow} << Regression coefficients between \samp{Y(i,j)} and \samp{Y(i+1,j)}.>>
#{rhocol} << Regression coefficients between \samp{Y(i,j)} and \samp{Y(i,j+1)}.>>
#{xrhocol} << Regression coefficients between \samp{XR(h)} and \samp{Y(i,1)}.>>
#{xrhorow} << Regression coefficients between \samp{XC(k)} and \samp{Y(1,j)}.>>
#{sigmas} << Conditional standard deviations (not variances!) of the \samp{Y} nodes.>>
#VALUE
# A list comprising the following components. (i) \samp{$nbn}: the resulting \samp{nbn} object;
# (ii) \samp{vari}: names of the variables (in a matrix);
# (iii) \samp{rcova}: names of the row covariates;
# (iv) \samp{ccova}: names of the column covariates;
# (v) \samp{$xy}: a two column matrix proposing convenient positions for the nodes
# to draw the associated DAG;
# (vi) \samp{$cova2vari}: the matrix of arcs going from a covariate to a variable;
# (vii) \samp{$vari2cova}: the matrix of arcs going from a variable to a covariate;
# (viii) \samp{$row2row}: the matrix of arcs going from a variable to another variable
#       having a greater row number;
# (ix) \samp{$col2col}: the matrix of arcs going from a variable to another variable
#       having a greater column number.
#EXAMPLE
#  print8nbn(generate8grid7crossed()$nbn,check=TRUE);
#  print8nbn(generate8grid7crossed(rhorow=matrix(1:2,2,3))$nbn,check=TRUE);
#  rr <- matrix(1:2,2,3);
#  rc <- matrix(10*(1:2),2,3);
#  xrr <- matrix(1:3,3);
#  xrc <- matrix(100*(1:3),3);
#  uu <- generate8grid7crossed(rhorow=rr,rhocol=rc,xrhorow=xrr,xrhocol=xrc);
#  print8nbn(uu$nbn,check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 14_04_02
#REVISED 14_08_12
#--------------------------------------------
{
  # constants
  ope <- "."; sep <- "."; clo <- "";
  ope <- ""; sep <- "-"; clo <- "";
  # checking and constituting
  matrice <- function(mat,nr,nc,kk) {
    if (is.null(mat)) { mat <- matrix(kk,nr,nc);}
    if (length(mat)==nr) { mat <- matrix(mat,nr,nc);}
    mat;
  }
  sssobject9(nrow,"integer",1,c(2,Inf));
  sssobject9(ncol,"integer",1,c(2,Inf));
  sssobject9(xrow,"integer",1,c(0,Inf));
  sssobject9(xcol,"integer",1,c(0,Inf));
  rhorow <- matrice(rhorow,nrow-1,ncol,0.5);
  sssobject9(rhorow,"matrix");
  rhocol <- matrice(rhocol,ncol-1,nrow,0.5);
  sssobject9(rhocol,"matrix");
  xrhorow <- matrice(xrhorow,nrow,xrow,0.5);
  sssobject9(xrhorow,"matrix");
  xrhocol <- matrice(xrhocol,ncol,xcol,0.5);
  sssobject9(xrhocol,"matrix");
  sigmas <- matrice(sigmas,nrow,ncol,1);
  sssobject9(sigmas,"matrix");
  if ((nrow(rhorow)!=nrow-1)|(ncol(rhorow)!=ncol)) {
    print(dim(rhorow));
    stop("Bad dimension for matrix 'rhorow'");
  }
  if ((nrow(rhocol)!=ncol-1)|(ncol(rhocol)!=nrow)) {
    print(dim(rhocol));
    stop("Bad dimension for matrix 'rhocol'");
  }
  if ((nrow(xrhorow)!=nrow)|(ncol(xrhorow)!=xrow)) {
    print(dim(xrhorow));
    stop("Bad dimension for matrix 'xrhorow'");
  }
  if ((nrow(xrhocol)!=ncol)|(ncol(xrhocol)!=xcol)) {
    print(dim(xrhocol));
    stop("Bad dimension for matrix 'xrhocol'");
  }
  if ((nrow(sigmas)!=nrow)|(ncol(sigmas)!=ncol)) {
    print(dim(sigmas));
    stop("Bad dimension for matrix 'sigmas'");
  }
  # initializing
  rnbn <- vector("list",xrow+xcol+nrow*ncol);
  posi <- matrix(NA,xrow+xcol+nrow*ncol,2);
  vari <- matrix("",nrow,ncol);
  rcova <- rep("",xrow);
  ccova <- rep("",xcol);
  nn <- 0;
  # the covariate nodes
  for (hh in sssbc(xrow)) {
    nn <- nn+1;
    posi[nn,] <- c(-(ncol-1)/4,nrow-(nrow-1)/(xrow+1)*hh);
    names(rnbn)[nn] <- paste0("XR",ope,hh,clo);
    rcova[hh] <- names(rnbn)[nn]; 
    rnbn[[nn]]$mu <- 0;
    rnbn[[nn]]$sigma <- 1;
    rnbn[[nn]]$parents <- character(0);
    rnbn[[nn]]$regcoef <- numeric(0);
  }    
  for (kk in sssbc(xcol)) {
    nn <- nn+1;
    posi[nn,] <- c((ncol-1)/(xcol+1)*kk,nrow+1+(nrow-1)/4);
    names(rnbn)[nn] <- paste0("XC",ope,kk,clo);
    ccova[kk] <- names(rnbn)[nn];
    rnbn[[nn]]$mu <- 0;
    rnbn[[nn]]$sigma <- 1;
    rnbn[[nn]]$parents <- character(0);
    rnbn[[nn]]$regcoef <- numeric(0);
  }    
  # the nodes for the grid
  for (ii in sssbc(nrow)) { for (jj in sssbc(ncol)) {
    nn <- nn+1;
    posi[nn,] <- c(jj,nrow+1-ii);
    names(rnbn)[nn] <- paste0("Y",ope,ii,sep,jj,clo);
    vari[ii,jj] <- names(rnbn)[nn];
    rnbn[[nn]]$mu <- 0;
    rnbn[[nn]]$sigma <- sigmas[ii,jj];
    rnbn[[nn]]$parents <- character(0);
    rnbn[[nn]]$regcoef <- numeric(0);
    if (ii>1) {
      rnbn[[nn]]$parents <- c(rnbn[[nn]]$parents,paste0("Y",ope,ii-1,sep,jj,clo));
      rnbn[[nn]]$regcoef <- c(rnbn[[nn]]$regcoef,rhorow[ii-1,jj]);
    } else {
      for (k in sssbc(xcol)) {
        coef <- xrhocol[jj,k];
        if (coef != 0) {
          rnbn[[nn]]$parents <- c(rnbn[[nn]]$parents,paste0("XC",ope,k,clo));
          rnbn[[nn]]$regcoef <- c(rnbn[[nn]]$regcoef,coef);
        }
      }
    }
    if (jj>1) {
      rnbn[[nn]]$parents <- c(rnbn[[nn]]$parents,paste0("Y",ope,ii,sep,jj-1,clo));
      rnbn[[nn]]$regcoef <- c(rnbn[[nn]]$regcoef,rhocol[jj-1,ii]);
    } else {
      for (h in sssbc(xrow)) {
        coef <- xrhorow[ii,h];
        if (coef != 0) {
          rnbn[[nn]]$parents <- c(rnbn[[nn]]$parents,paste0("XR",ope,h,clo));
          rnbn[[nn]]$regcoef <- c(rnbn[[nn]]$regcoef,coef);
        }
      }
    }
  }}
  # finalization
  nona <- names(rnbn);
  dimnames(posi) <- list(nodes=nona,coor=c("x","y"));
  nonacr <- nona[sssbc(xrow)]; nonacc <- nona[xrow+sssbc(xcol)];
  nonava <- t(matrix(nona[-sssbc(xrow+xcol)],ncol,nrow));
  #
  # arc matrices
  cova2vari <- matrix(NA,xrow*nrow+xcol*ncol,2,
                      dimnames=list(NULL,c("from","to")));
  nn <- 0;
  for (hh in sssbc(xrow)) { for (ii in sssbc(nrow)) {
    nn <- nn+1;
    cova2vari[nn,] <- c(nonacr[hh],nonava[ii,1]);
  }}
  for (kk in sssbc(xcol)) { for (jj in sssbc(ncol)) {
    nn <- nn+1;
    cova2vari[nn,] <- c(nonacc[kk],nonava[1,jj]);
  }}
  #
  vari2cova <- matrix(NA,nrow*ncol*(xrow+xcol),2,
                      dimnames=list(NULL,c("from","to")));
  nn <- 0;
  for (ii in sssbc(nrow)) { for (jj in sssbc(ncol)) {
    for (hh in sssbc(xrow)) {
      nn <- nn+1;
      vari2cova[nn,] <- c(nonava[ii,jj],nonacr[hh]);
    }
    for (kk in sssbc(xcol)) {
      nn <- nn+1;
      vari2cova[nn,] <- c(nonava[ii,jj],nonacc[kk]);
    }
  }}
  #
  nn <- 0;
  row2row <- matrix(NA,nrow*nrow*ncol*(ncol-1)/2,2,
                       dimnames=list(NULL,c("from","to")));
  for (j1 in sssbc(ncol-1)) { for (j2 in (j1+sssbc(ncol-j1))) {
    for (i1 in sssbc(nrow)) { for (i2 in sssbc(nrow)) { 
      nn <- nn + 1;
      row2row[nn,] <- c(nonava[i1,j1],nonava[i2,j2]);
    }}
  }}
  #
  nn <- 0;
  col2col <- matrix(NA,ncol*ncol*nrow*(nrow-1)/2,2,
                       dimnames=list(NULL,c("from","to")));
  for (i1 in sssbc(nrow-1)) { for (i2 in (i1+sssbc(nrow-i1))) {
    for (j1 in sssbc(ncol)) { for (j2 in sssbc(ncol)) { 
      nn <- nn + 1;
      col2col[nn,] <- c(nonava[i1,j1],nonava[i2,j2]);
    }}
  }}
  # returning
  list(nbn=rnbn,posi=posi,
       vari=vari,rcova=rcova,ccova=ccova,
       cova2vari=cova2vari,
       vari2cova=vari2cova,
       row2row=row2row,
       col2col=col2col);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
