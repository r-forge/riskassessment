#
# 12_11_29
#
library(rbsb);
library(bnlearn);
source("auxi.code.r");
source("nbn.code.r");
source("interface.code.r");
#
nbn1 <- read8nbn("test.nbn.code.dat1");
form3titre("Initial /nbn/");
print8nbn(nbn1);
#
bnf1 <- nbn2bnfit(nbn1);
print(bnf1);
plot(bn.net(bnf1));
