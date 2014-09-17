
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
# The function \samp{sssread8list} is called to read the file so standard
# tags \samp{<<...>>} and \samp{[[...]]} can be replaced according the
# user preferences.\cr
# Comments and more things can be introduced in the file, see
# the \samp{sssread8list} documentation for full details.\cr
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
# rbmn0 <- complete8rbmn0(rbmn0);
# print8nbn(rbmn0$nbn1$v);
# write8nbn(rbmn0$nbn1$v,file="toto.txt");
# print8nbn(read8nbn("toto.txt"));
# unlink("toto.txt");
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
    if (sssfidi9(file)!="f") {
      ssserreur(file,"It is not an valid file!");
    }
  }
  # getting the list
  uu <- sssread8list(file);
  # node number
  nn <- length(uu);
  # completing each node with the default values
  for (ii in sssbc(nn)) {
    if (sssvoid9(uu[[ii]])) {
      uu[[ii]] <- vector("list",0);
    }
    if (sssvoid9(uu[[ii]]$parents)) {
      uu[[ii]]$parents <- character(0);
      uu[[ii]]$regcoef <- numeric(0);
    } else {
      if (sssvoid9(uu[[ii]]$regcoef)) {
        uu[[ii]]$regcoef <- rep(1,length(uu[[ii]]$parents));
      } else {
        uu[[ii]]$regcoef <- as.numeric(uu[[ii]]$regcoef);
      }
    }
    if (sssvoid9(uu[[ii]]$mu)) {
      uu[[ii]]$mu <- 0;
    } else {
      uu[[ii]]$mu <- as.numeric(uu[[ii]]$mu);
    } 
    if (sssvoid9(uu[[ii]]$sigma)) {
      uu[[ii]]$sigma <- 1;
    } else {
      uu[[ii]]$sigma <- as.numeric(uu[[ii]]$sigma);
    }
  }
  # checking the proposed order
  voir <- topo4nbn(uu,sssbc(nn));
  if (length(voir)>0) {
    # modifying the proposed order
    ssserreur(names(uu),"Not a topological order, was changed",w=TRUE);
    nord <- topo4nbn(uu);
    vv <- uu;
    for (ii in sssbc(nn)) {
      uu[ii] <- vv[nord[ii]];
      names(uu) <- names(vv)[nord];
    }
  }
  # returning
  uu;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
write8nbn <- function(nbn,file,tag1=sssrbsa0$tag1$v,check=rbmn0$check$v)
#TITLE writes a /nbn/ object into a text file
#DESCRIPTION writes to the text file \samp{file}
# a /nbn/ object which can be back read with \samp{read8nbn}.
#DETAILS
# See \samp{read8nbn} documentation.
#KEYWORDS 
#INPUTS
#{nbn} << The concerned \samp{nbn} object.>>  
#{file} <<file to be created. When NULL the result is sent
#         to the standard output.>>
#[INPUTS]
#{tag1} <<A matrix of three tags (by rows): first column
#         to open and second column to close.>>
#{check} << Must a check of the arguments be performed?>>
#VALUE
# nothing but a file is created.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# write8nbn(rbmn0$nbn1$v,file="toto.txt");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 12_01_16
#REVISED 12_02_16
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
  # opening the file
  if (!is.null(file)) { sink(file);}
  # node number
  nn <- length(nbn);
  cat("#\n");
  cat("# created by 'write8nbn' on",sssnow(),"\n");
  cat("#\n");
  # writing each node in turn
  for (ii in sssbc(nn)) {
    cat("#----------------\n");
    cat(tag1[1,1],names(nbn)[ii],tag1[1,2],"\n",sep="");
    cat(tag1[2,1],"mu",tag1[2,2]," ",nbn[[ii]]$mu,"\n",sep="");
    cat(tag1[2,1],"sigma",tag1[2,2]," ",nbn[[ii]]$sigma,"\n",sep="");
    if (length(nbn[[ii]]$parents) > 0) {
      cat(tag1[2,1],"parents",tag1[2,2]," ",sep="");
      cat(nbn[[ii]]$parents,collapse=" ");
      cat("\n");
      cat(tag1[2,1],"regcoef",tag1[2,2]," ",sep="");
      cat(nbn[[ii]]$regcoef,collapse=" ");
      cat("\n");
    }
    cat("#----------------\n");
  }
  # closing the file
  if (!is.null(file)) { sink();}
  # returning
  invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
read8chain <- function(file,check=rbmn0$check$v)
#TITLE reads from a text file a /chain/ object.
#DESCRIPTION reads from the text file \samp{file}
# following a precised format an object describing
# in the natural way a /chain/ that is a normal Bayesian
# network where all nodes are in simple string.\cr
# The /chain/ is described with several fields:
#  (i) the sequence of node names in their order,
#  (ii) the names of the roots (at least one).
#       Default first.
#  (iii) the names of the colliders (always in
#        between two roots). Default none.
#  (iv) the conditional expectations of
#       each node (if only one value is given, it is
#       interpreted as common for all nodes).
#       Default 0.
#  (v) the conditional standard deviation
#      of each node (if only one value is given, it is
#      interpreted as common for all nodes).
#       Default 1.
#  (vi) the correlations between neighbour nodes
#       (if only one value is given, it is
#       interpreted as common for all relatonships).
#       Default 0.5.\cr
# But when a field \samp{marginal} is added to the list, then
# the expectation and standard deviations of each node are
# interpreted as being marginal and not conditional. Nevertherless
# the resulting output is given in conditional status.
#DETAILS
# The function \samp{sssread8list} is called to read the file so standard
# tags \samp{<<...>>} and \samp{[[...]]} can be replaced according the
# user preferences.
# Comments and more things can be introduced in the file, see
# the \samp{sssread8list} documentation for full details.
#KEYWORDS 
#INPUTS
#{file} <<file to be read.>>
#[INPUTS]
#{check} << Must a check of the arguments be performed?>>
#VALUE
# a /chain/ object, for the moment a mere list.
#EXAMPLE
# print8chain(rbmn0$chain1$v);
# write8chain(rbmn0$chain1$v,file="toto.txt");
# print8chain(read8chain("toto.txt"));
# unlink("toto.txt");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 12_01_17
#REVISED 12_01_30
#--------------------------------------------
{
  # checking
  if (check) {
    if (sssfidi9(file)!="f") {
      ssserreur(file,"It is not an valid file!");
    }
  }
  # getting the list
  uu <- sssread8list(file);
  if (is.null(uu$names)) {
    ssserreur(uu,message=("'$names' is compulsory"));
  }
  # node number
  nn <- length(uu$names);
  # default values fields
  if (is.null(uu$roots)) { uu$roots <- uu$names[1]; }
  if (is.null(uu$mu)) { uu$mu <- 0; }
  if (is.null(uu$sigma)) { uu$sigma <- 1; }
  if (nn > 1) {
    if (is.null(uu$corre)) {
      uu$corre <- 0.5;
    }
  }
  # finding the root positions
  aaa <- sort(sssposi9(uu$roots,uu$names));
  # checking the consistency of roots and colliders
  # and sorting them
  if (!all(uu$roots %in% uu$names)) {
    ssserreur(uu,messsage="'roots' not consistent with 'names'");
  }
  if (length(uu$colliders) > 0) {
    if (!all(uu$colliders %in% uu$names)) {
      ssserreur(uu,messsage="'colliders' not consistent with 'names'");
    }
    if (length(uu$roots)!=length(uu$colliders)+1) {
      ssserreur(uu,message="Non consistent numbers of roots and colliders");
    }
    ccc <- sort(sssposi9(uu$colliders,uu$names));
    if (aaa[1] >= ccc[1]) {
      ssserreur(uu,message="A collider before an root");
    }
    if (!all((aaa[-length(aaa)]-ccc)<0)) {
      ssserreur(uu,message="Positions of roots and colliders not consistent [1]");
    }
    if (!all((aaa[-1]-ccc)>0)) {
      ssserreur(uu,message="Positions of roots and colliders not consistent [2]");
    }
    uu$roots <- uu$names[aaa];
    uu$colliders <- uu$names[ccc];
  } else {
    if (length(uu$roots) != 1) {
      ssserreur(uu,message="When there is no collider, exactly one root is expected");
    }
  }
  # translating into numeric
  uu$mu <- as.numeric(uu$mu);
  uu$sigma <- as.numeric(uu$sigma);
  uu$corre <- as.numeric(uu$corre);
  # checking the var-covariance values
  if (!all(uu$sigma>=0)) {
    ssserreur(uu$sigma,message="negative standard deviations");
  }
  if (!all(abs(uu$corre)<=1)) {
    ssserreur(uu$corre,message="exagerated correlation");
  }
  # completing with the default values
  if (length(uu$mu) != nn) {
    uu$mu <- rep(uu$mu[1],nn);
  }
  if (length(uu$sigma) != nn) {
    uu$sigma <- rep(uu$sigma[1],nn);
  }
  if (nn > 2) {
    if (length(uu$corre) != (nn-1)) {
      uu$corre <- rep(uu$corre[1],nn-1);
    }
  }
  # must expectation and standard deviation
  # being considered as marginal?
  if (!is.null(uu$marginal)) {
    uu <- aux4(uu);
  }
  # returning
  uu;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
write8chain <- function(chain,file=NULL,check=rbmn0$check$v)
#TITLE writes a /chain/ object into a text file
#DESCRIPTION writes to the text file \samp{file}
# a /chain/ object which can be back read with \samp{read8chain}.
#DETAILS
# See \samp{read8chain} documentation.
#KEYWORDS 
#INPUTS
#{chain} << The concerned \samp{chain} object.>>  
#{file} <<file to be created. When NULL the result is sent
#         to the standard output.>>
#[INPUTS]
#{check} << Must a check of the arguments be performed?>>
#VALUE
# nothing but a file is created.
#EXAMPLE
# write8chain(rbmn0$chain1$v,file="toto.txt");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 12_01_17
#REVISED 12_01_17
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8chain(chain);
    if (length(che)>0) {
      print(che);
      stop("The provided 'chain' is not valid!");
    }
  }
  # opening the file
  if (!is.null(file)) { sink(file);}
  # node number
  nn <- length(chain$names);
  cat("#\n");
  cat("# created by 'write8nbn' on",sssnow(),"\n");
  cat("#\n");
  cat("#----------------\n");
  # writing each fields in turn
  cat(sssrbsa0$tag1$v[1,1],"names",sssrbsa0$tag1$v[1,2]," ",
      paste(chain$names,collapse=" "),sep="");
  cat("\n");
  cat(sssrbsa0$tag1$v[1,1],"roots",sssrbsa0$tag1$v[1,2]," ",
      paste(chain$roots,collapse=" "),sep="");
  cat("\n");
  cat(sssrbsa0$tag1$v[1,1],"colliders",sssrbsa0$tag1$v[1,2]," ",
      paste(chain$colliders,collapse=" "),sep="");
  cat("\n");
  cat(sssrbsa0$tag1$v[1,1],"mu",sssrbsa0$tag1$v[1,2]," ",
      paste(chain$mu,collapse=" "),sep="");
  cat("\n");
  cat(sssrbsa0$tag1$v[1,1],"sigma",sssrbsa0$tag1$v[1,2]," ",
      paste(chain$sigma,collapse=" "),sep="");
  cat("\n");
  if (length(chain$names) > 1) {
    cat(sssrbsa0$tag1$v[1,1],"corre",sssrbsa0$tag1$v[1,2]," ",
        paste(chain$corre,collapse=" "),sep="");
    cat("\n");
  }
  cat("#----------------\n");
  # closing the file
  if (!is.null(file)) { sink();}
  # returning
  invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
