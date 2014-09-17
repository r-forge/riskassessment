#
# defining constants for the /rbmn/ package
#
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rbmn0 <- 
#TITLE list of the /rbmn/ constants
#DESCRIPTION
# Just a list of constants defined with a name, a definition
# and a value. They can be modified by the user. Values
# definitions and names can be obtained with the function
# \samp{sssdisplay8k} of /rbsa/ package.
#DETAILS
# It is a named list, one component for each constant.
# A sublist is associated to each constant with two components: \samp{$d}
# for the definition and \samp{$v} for the value. Be aware that the value
# can be any object (vector, list, matrix, function,...)
#KEYWORDS misc
#INPUTS
#[INPUTS]
#VALUE
# A list see the description and details to be able to know its contents.
# (it is a self-documented object.)
#EXAMPLE
# names(rbmn0);
# rbmn0$check;
# rbmn0$chain1;
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 13_07_03
#REVISED 14_09_16
#--------------------------------------------
{
  # initialization
  set.seed(1234);
  #
  #  <0> general monitoring
  #
  l0 <- list(
             check=list(v=TRUE,
                        d="Must systematic checking be done in the functions"
                       )
            );
  #
  #  <1> DEFINING SOME INITIAL OBJECTS
  #
  l1 <- list(
         chain1=list(v=list(names = c("A", "B", "C"),
                            mu    = c(  0,   0,   0),
                            sigma = c(  1,   2,   3),
                            corre = c(0.5, 0.5), 
                            roots = c("A"), colliders = NULL
                           ),
                     d="chain1 as /chain/"),
         chain2=list(v=list(names = letters[1:5],
                            mu    = 1:5,
                            sigma = 5:1,
                            corre = rep(1/3,4), 
                            roots = c("a","d"), colliders = c("c")
                           ),
                     d="chain2 as /chain/"),
         chain3=list(v=list(names=LETTERS[1:10],
                            mu=rep(0,10),
                            sigma=runif(10),
                            corre=runif(9,-1,1),
                            roots=c("A","E","G","I"),
                            colliders=c("C","F","H")
                           ),
                     d="chain3 as /chain/"),
         adja4=list(v=matrix(c(rep(0,5),1,rep(0,5),1,rep(0,4),1,
                               rep(0,3),1,0,1,1,0),
                             5,dimnames=
                                list(c("C","1.1","1.2","2.1","2.2"),
                                     c("C","1.1","1.2","2.1","2.2"))),
                    d="chain4 as /adja/")
            );
  #
  #  <2> Some text to be transformed into /nbn/
  #
  l2 <- list(
             dat1=list(
                      v=c("<<A>>",
                        "<<B>>",
                        "<<C>>",
                        "[[parents]] A B",
                        "<<D>>",
                        "[[parents]] C",
                        "<<E>>",
                        "[[parents]] C",
                        "<<F>>",
                        "[[parents]] D E"
                          ),
                      d="Data set number 1"
                      ),
             dat2=list(
                      v=c(
                          "<<A>>",
                          "<<B>>",
                          "[[parents]] A",
                          "[[regcoef]] 3",
                          "[[sigma]] 4",
                          "[[mu]] 10",
                          "<<C>>",
                          "[[parents]] B",
                          "[[regcoef]] 0.2",
                          "[[sigma]] 1.7320508076",
                          "[[mu]] 100"
                         ),
                      d="Data set number 2"
                      ),
             dat3=list(
                      v=c(
                          "<<A>>",
                          "[[parents]] B",
                          "[[regcoef]] 0.12",
                          "[[sigma]] 0.8",
                          "<<B>>",
                          "[[sigma]] 5",
                          "[[mu]] 10",
                          "<<C>>",
                          "[[parents]] B",
                          "[[regcoef]] 0.2",
                          "[[sigma]] 1.7320508076",
                          "[[mu]] 100"
                         ),
                      d="Data set number 3"
                      ),
             dat4=list(
                      v=c(
                          "<<A>>",
                          "<<B>>",
                          "[[parents]] A",
                          "<<C>>",
                          "[[parents]] A",
                          "<<D>>",
                          "[[parents]] B C"
                         ),
                      d="Data set number 4"
                      )
            );
  # returning
  c(l0,l1,l2);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
complete8rbmn0 <- function(rbmn0)
#TITLE completes the general constant with further components
#DESCRIPTION 
# From the initial constant \samp{rbmn0}, returns it
# with more components, keeping the initial ones.
#DETAILS
# More \samp{nbn}, \samp{adja}, \samp{mn}, \samp{gema} and
#  \samp{arc} objects are generated using available functions of
# \pkg{rbmn} package. Preexisting ones will be silently replaced.
#KEYWORDS 
#INPUTS
#{rbmn0} << The initial constant to be completed.>>
#[INPUTS]
#VALUE
# The completed constant
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 14_09_16
#REVISED 14_09_16
#--------------------------------------------
{
  # checking
  if(is.null(rbmn0$chain1)) {
    ssserreur(rbmn0,"Component 'rbmn0$chain1' is not present");
  }
  if(is.null(rbmn0$chain2)) {
    ssserreur(rbmn0,"Component 'rbmn0$chain2' is not present");
  }
  if(is.null(rbmn0$chain3)) {
    ssserreur(rbmn0,"Component 'rbmn0$chain3' is not present");
  }
  if(is.null(rbmn0$adja4)) {
    ssserreur(rbmn0,"Component 'rbmn0$adja4' is not present");
  }
  # initialization
  res <- rbmn0;
  # creating the new components
  l1 <- list(
       nbn1=list(v=chain2nbn(rbmn0$chain1$v,check=FALSE),
                 d="chain1 as /nbn/"),
       nbn2=list(v=chain2nbn(rbmn0$chain2$v,check=FALSE),
                 d="chain2 as /nbn/"),
       nbn3=list(v=chain2nbn(rbmn0$chain3$v,check=FALSE),
                 d="chain3 as /nbn/"),
       nbn4=list(v= adja2nbn( rbmn0$adja4$v),
                 d="chain4 as /nbn/")
             );
  l2 <- list(
       #
       adja1=list(v=adja4nbn(l1$nbn1$v,check=FALSE),
                 d="chain1 as /adja/"),
       adja2=list(v=adja4nbn(l1$nbn2$v,check=FALSE),
                 d="chain2 as /adja/"),
       adja3=list(v=adja4nbn(l1$nbn3$v,check=FALSE),
                 d="chain3 as /adja/"),
       #
       mn1=list(v=nbn2mn(l1$nbn1$v),
                 d="chain1 as /mn/"),
       mn2=list(v=nbn2mn(l1$nbn2$v),
                 d="chain2 as /mn/"),
       mn3=list(v=nbn2mn(l1$nbn3$v),
                 d="chain3 as /mn/"),
       mn4=list(v=nbn2mn(l1$nbn4$v),
                 d="chain4 as /mn/"),
       #
       gema1=list(v=nbn2gema(l1$nbn1$v,check=FALSE),
                 d="chain1 as /gema/"),
       gema2=list(v=nbn2gema(l1$nbn2$v,check=FALSE),
                 d="chain2 as /gema/"),
       gema3=list(v=nbn2gema(l1$nbn3$v,check=FALSE),
                 d="chain3 as /gema/"),
       gema4=list(v=nbn2gema(l1$nbn4$v,check=FALSE),
                 d="chain4 as /gema/")
            );
 #
 #  PREPARING A CROSSED STRUCTURE
  uu <- vv <- l2$adja1$v;
  ss <- c("T","L","A"); cc <- c("L","F","B");
  dimnames(uu) <- list(ss,ss);
  dimnames(vv) <- list(cc,cc);
  uun <- adja2nbn(uu);
  vvn <- adja2nbn(vv);
  ww <- as.vector(outer(ss,cc,paste,sep=""));
  crarc.05 <- arcs4nbn1nbn(uun,vvn,nona=ww,check=FALSE);
  nbn.05 <- crossed4nbn1nbn(uun,vvn,nona=ww,check=FALSE);
  #
  l3 <- list(
      arc5 =list(v=crarc.05,
                 d="Crossed nbn as /nbn/"),
      nbn5 =list(v=nbn.05,
                 d="Crossed nbn as /nbn/"),
      adja5=list(v=adja4nbn(nbn.05,check=FALSE),
                 d="Crossed nbn as /adja/"),
      gema5=list(v=nbn2gema(nbn.05,check=FALSE),
                 d="Crossed nbn as /gema/"),
      mn5  =list(v=nbn2mn (nbn.05),
                 d="Crossed nbn as /mn/")
            );
  #
  # adding the different components
  lne <- c(l1,l2,l3);
  for (nn in names(lne)) {
    res[[nn]] <- lne[[nn]];
  }
  #
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
