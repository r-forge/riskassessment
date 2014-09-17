#
# 13_04_19 13_04_20 13_04_21 13_04_24 13_04_25
#
library(rbsb);
source("auxi.code.r");
source("proba.code.r");
source("nbn.code.r");
source("idea.code.r");
source("crossed.code.r");
#
set.seed(4321);
nbnc <- generate8nbn(3,nona=paste("c",1:3,sep=""));
nbnr <- generate8nbn(4,nona=paste("r",1:4,sep=""));
nbn1 <- nbn2crossed(nbnr,nbnc);
nn <- length(nbn1);
ny <- 100; don <- as.data.frame(matrix(round(100*runif(nn*ny)),ncol=nn,
                                       dimnames=list(NULL,names(nbn1))));
form3title("Initial /nbn/");
print8nbn(nbn1);
nite <- 20; eps <- 10^-3;
sarc <- vector("list",length(nbnr)+length(nbnc));
sarc[bf(nbnr)] <- nbn2crossed4arcs(nbnr,nbnc,TRUE);
sarc[length(nbnr)+bf(nbnc)] <- nbn2crossed4arcs(nbnr,nbnc,FALSE);
nbn2 <- estimate8constrainednbn(nbn1,sarc,don,
                                imp=TRUE,nite=nite,eps=eps);
print8nbn(nbn2);
pause("constrained estimates OK (integrated)?");
#
#
nbn1 <- read8nbn("test.nbn.code.dat4");
nn <- length(nbn1);
ny <- 10; don <- as.data.frame(matrix(round(100*runif(nn*ny)),ncol=nn,
                                       dimnames=list(NULL,names(nbn1))));
form3title("Initial /nbn/");
print8nbn(nbn1);
nite <- 10; nbne <- nbn1;
for (ite in bc(nite)) {
  aa1 <- matrix(c("A","C",
                  "B","D"),ncol=2);
  nbne <- estimate8arcs(nbne,aa1,don);
  aa2 <- matrix(c("A","B",
                  "C","D"),ncol=2);
  nbne <- estimate8arcs(nbne,aa2,don);
  form3title(paste("Estimated /nbn/ with",ite,"iteration"));
  print8nbn(nbne);
  pause(ite);
}
pause("constrained estimates OK (by hand)?");
#
print8nbn(generate8nbn());
pause("Ça va ?");
#
set.seed(12345);
for (iif in 2:4) {
  if (iif < 4) {
    fifi <- paste("test.nbn.code.dat",iif,sep="");
    nbn1 <- read8nbn(fifi);
  } else {
    nbn1 <- generate8nbn(rnn=c(9,9));
  }
  form3title("Initial /nbn/");
  print8nbn(nbn1);
  rr1 <- nbn2rr(nbn1);
  #print(rr1);
  mn11 <- nbn2mn1(nbn1);
  cat(rep("-",30),"\n",sep="");
  print8mn(mn11,ordering=names(nbn1));
  cat(rep("-",30),"\n",sep="");
  mn12 <- nbn2mn2(nbn1);
  cat(rep("-",30),"\n",sep="");
  print8mn(mn12,ordering=names(nbn1));
  cat(rep("-",30),"\n",sep="");
  mn13 <- nbn2mn3(nbn1);
  cat(rep("-",30),"\n",sep="");
  print8mn(mn13,ordering=names(nbn1));
  cat(rep("-",30),"\n",sep="");
}
#
pause("Content ?");
#

