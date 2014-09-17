#
# 13_04_19 13_04_20 13_04_21 13_04_24 13_04_25
# 14_08_15 14_08_16
#
debug <- FALSE;
# loading the package
if (debug) {
  source8file(Sys.glob("*.code.r"));
  voir <- 1;
} else {
  library(rbmn);
  voir <- NA;
}
rbmn0 <- complete8rbmn0(rbmn0);
#
library(bnlearn);
#
print8nbn(rbmn0$nbn5$v);
data(boco);
print8nbn(estimate8nbn(rbmn0$nbn5$v,boco));
ssspause("estimation",top=voir);
sarc5 <- rbmn0$arc5$v;
print8nbn(estimate8constrained7nbn(rbmn0$nbn5$v,sarc5,boco));
ssspause("constrained estimation",top=voir);
#
set.seed(4321);
nbnc <- generate8nbn(3,nona=paste("c",1:3,sep=""));
nbnr <- generate8nbn(4,nona=paste("r",1:4,sep=""));
nbn1 <- crossed4nbn1nbn(nbnr,nbnc);
nn <- length(nbn1);
ny <- 100; don <- as.data.frame(matrix(round(100*runif(nn*ny)),ncol=nn,
                                       dimnames=list(NULL,names(nbn1))));
sssform3title("Initial /nbn/");
print8nbn(nbn1);
nite <- 20; eps <- 10^-3;
sarc <- vector("list",length(nbnr)+length(nbnc));
sarc[1:length(nbnr)] <- arcs4nbn1nbn(nbnr,nbnc,"a2");
sarc[length(nbnr)+(1:length(nbnc))] <- arcs4nbn1nbn(nbnr,nbnc);
nbn2 <- estimate8constrained7nbn(nbn1,sarc,don,
                                imp=TRUE,nite=nite,eps=eps);
print8nbn(nbn2);
ssspause("constrained estimates OK (integrated)?",top=voir);
#
#
if (debug) {
  nbn1 <- read8nbn("perso/test.nbn.code.dat4");
  nn <- length(nbn1);
  ny <- 10; don <- as.data.frame(matrix(round(100*runif(nn*ny)),ncol=nn,
                                         dimnames=list(NULL,names(nbn1))));
  sssform3title("Initial /nbn/");
  print8nbn(nbn1);
  nite <- 10; nbne <- nbn1;
  for (ite in bc(nite)) {
    aa1 <- matrix(c("A","C",
                    "B","D"),ncol=2);
    nbne <- estimate8arcs(nbne,aa1,don);
    aa2 <- matrix(c("A","B",
                    "C","D"),ncol=2);
    nbne <- estimate8arcs(nbne,aa2,don);
    sssform3title(paste("Estimated /nbn/ with",ite,"iteration"));
    print8nbn(nbne);
    ssspause(ite,top=voir);
  }
  ssspause("constrained estimates OK (by hand)?",top=voir);
  #
  print8nbn(generate8nbn());
  ssspause("Ca va ?",top=voir);
  #
  set.seed(12345);
  for (iif in 2:4) {
    if (iif < 4) {
      fifi <- paste("perso/test.nbn.code.dat",iif,sep="");
      nbn1 <- read8nbn(fifi);
    } else {
      nbn1 <- generate8nbn(rnn=c(9,9));
    }
    sssform3title("Initial /nbn/");
    print8nbn(nbn1);
    rr1 <- nbn2rr(nbn1);
    print(rr1);
    mn11 <- nbn2mn(nbn1,1);
    cat(rep("-",30),"\n",sep="");
    print8mn(mn11,ordering=names(nbn1));
    cat(rep("-",30),"\n",sep="");
    mn12 <- nbn2mn(nbn1,2);
    cat(rep("-",30),"\n",sep="");
    print8mn(mn12,ordering=names(nbn1));
    cat(rep("-",30),"\n",sep="");
    mn13 <- nbn2mn(nbn1,3);
    cat(rep("-",30),"\n",sep="");
    print8mn(mn13,ordering=names(nbn1));
    cat(rep("-",30),"\n",sep="");
  }
}
#
sssform3title("'nbnst.test.r' finished!",8);
#

