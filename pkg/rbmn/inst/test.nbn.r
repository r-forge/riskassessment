#
# 12_07_27 13_01_28 13_04_24 13_04_26 14_08_21
#
library(rbmn);
#
#
## getting the data set
data(boco)
print(head(boco));
#
## estimating a simple model
mod <- rbmn0$nbn5$v;
est <- estimate8nbn(rbmn0$nbn5$v,boco);
print8nbn(mod);
print8nbn(est);
#
## getting the main forms of a Bayesian network
nbnmod <- mod;
print8nbn(nbnmod);
mnmod <- nbn2mn(mod);
print8mn(mnmod);
gemamod <- nbn2gema(mod);
print8gema(gemamod);
#
stop("fini");
#
source("auxi.code.r");
source("proba.code.r");
source("nbn.code.r");
source("interface.code.r");
source("rbsb0.code.r");
#
#
nbn1 <- read8nbn("test.nbn.code.dat1");
form3titre("Initial /nbn/");
print8nbn(nbn1);
mn1 <- gema2mn(nbn2gema(nbn1));
print8mn(mn1);
print(round(var2pre(mn1$gamma),3));
#
set.seed(4321);
nn <- 3; nbn1 <- generate8nbn(rnn=nn);
ny <- 100; don <- as.data.frame(matrix(round(100*runif(nn*ny)),ncol=nn,
                                       dimnames=list(NULL,names(nbn1))));
form3titre("Initial /nbn/");
print8nbn(nbn1);
nbn2 <- estimate8nbn(nbn1,don);
form3titre("Estimated /nbn/");
print8nbn(nbn2);
# testing with /bnlearn/
library(bnlearn);
dag <- nbn2bnfit(nbn1,TRUE);
bnf <- bn.fit(dag,data=don);
nbn3 <- bnfit2nbn(bnf);
form3titre("Estimated /nbn/ by /bnlearn/");
print8nbn(nbn3);
pause("estimates OK ?");
#
set.seed(4321);
nbn1 <- generate8nbn(rnn=5);
form3titre("Initial /nbn/");
print8nbn(nbn1);
form3titre("Its R-matrix");
rm1 <- nbn2rmatrix(nbn1);
print(rm1);
form3titre("coming back to the /nbn/");
nbn1b <- rmatrix2nbn(rm1);
print8nbn(nbn1b);
pause("R-matrix OK ?");
#
print8nbn(nbn2nbn(nbn1,6:1));
print8nbn(nbn2nbn(nbn1,c(2,6,4,5)));
pause("Content ?");
#
nbn1r <- rm8nd4nbn(nbn1,"C");
form3titre("1: Reduced /nbn/");
print8nbn(nbn1r);
mn1r <- gema2mn(nbn2gema(nbn1r));
print8mn(mn1r);
print(round(var2pre(mn1r$gamma),3));
#
gaga <- mn1$gamma;
grgr <- gaga[-3,-3];
print(round(var2pre(gaga),3));
print(round(var2pre(grgr),3));
stop()
nbn2r <- rm8nd4nbn(nbn1,c("C","F"));
form3titre("2: Reduced /nbn/");
print8nbn(nbn2r);
mn2r <- gema2mn(nbn2gema(nbn2r));
print8mn(mn2r);
#

