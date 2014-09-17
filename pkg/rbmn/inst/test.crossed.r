#
# 13_04_23 13_04_26 13_05_21 13_05_22
#
library(rbsb);
source("auxi.code.r");
source("crossed.code.r");
source("nbnma.code.r");
source("rbsb.code.r");
source("nbnmi.code.r");
source("misc.code.r");
#
set.seed(12345);
nbn1 <- generate8nbn(rnn=4,nona=letters);
print8nbn(nbn1);
nbn2 <- generate8nbn(rnn=5);
print8nbn(nbn2);
sarca1 <- nbn2crossed4arcs(nbn1,nbn2,"a1");
print(sarca1); pause("par arcs de nbn1");
sarca2 <- nbn2crossed4arcs(nbn1,nbn2,"a2");
print(sarca2); pause("par arcs de nbn2");
sarcn1 <- nbn2crossed4arcs(nbn1,nbn2,"n1");
print(sarcn1); pause("par noeuds de nbn1");
sarcn2 <- nbn2crossed4arcs(nbn1,nbn2,"n2");
print(sarcn2); pause("par noeuds de nbn2");
#
pause("Content de ces flèches ?");
#
uu <- paste("R",1:3,sep="");
vv <- paste("C",1:3,sep="");
muu <- matrix(c(0,1,1,rep(0,6)),3,3,dimnames=list(uu,uu));
mvv <- matrix(c(0,0,0,1,rep(0,5)),3,3,dimnames=list(vv,vv));
print(muu); print(mvv);
muv <- adja2crossed(muu,mvv);
print(muv);
pause("adjacencies...");
#
#
set.seed(12345);
nbn1 <- generate8nbn(rnn=c(3,3),nona=letters);
print8nbn(nbn1);
nbn2 <- generate8nbn(rnn=c(3,3));
print8nbn(nbn2);
dounbn <- nbn2crossed(nbn1,nbn2);
print8nbn(dounbn);
#
pause("Content de croisement ?");
#

