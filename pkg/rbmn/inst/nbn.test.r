#
# 12_07_27 13_01_28 13_04_24 13_04_26 14_08_12
#
debug <- FALSE;
# loading the package
if (debug) {
  source8file(Sys.glob("*.code.r"));
  top <- 1;
} else {
  library(rbmn);
  top <- NA;
}
rbmn0 <- complete8rbmn0(rbmn0);
#
fifi <- "toto.txt";
sink(fifi);
cat(rbmn0$dat1$v,sep="\n");
sink();
nbn1 <- read8nbn(fifi);
unlink(fifi);
sssform3title("Initial /nbn/");
print8nbn(nbn1);
mn1 <- gema2mn(nbn2gema(nbn1));
print8mn(mn1);
print(round(var2pre(mn1$gamma),3));
ssspause("nbn1",top=top);
#
set.seed(4321);
nn <- 3; nbn1 <- generate8nbn(rnn=nn);
ny <- 100; don <- as.data.frame(matrix(round(100*runif(nn*ny)),ncol=nn,
                                       dimnames=list(NULL,names(nbn1))));
sssform3title("Initial /nbn/");
print8nbn(nbn1);
nbn2 <- estimate8nbn(nbn1,don);
sssform3title("Estimated /nbn/");
print8nbn(nbn2);
# testing with /bnlearn/
library(bnlearn);
dag <- nbn2bnfit(nbn1,TRUE);
bnf <- bn.fit(dag,data=don);
nbn3 <- bnfit2nbn(bnf);
sssform3title("Estimated /nbn/ by /bnlearn/");
print8nbn(nbn3);
ssspause("estimates OK ?",top=top);
#
set.seed(4321);
nbno <- 5;
nbn1 <- generate8nbn(rnn=nbno);
sssform3title("Initial /nbn/");
print8nbn(nbn1);
sssform3title("Its R-matrix");
rm1 <- rmatrix4nbn(nbn1);
print(rm1);
sssform3title("coming back to the /nbn/");
nbn1b <- nbn4rmatrix(rm1);
print8nbn(nbn1b);
ssspause("R-matrix OK ?",top=top);
#
print8nbn(nbn2nbn(nbn1,nbno:1));
print8nbn(nbn2nbn(nbn1,c(2,1,4,5)));
ssspause("Content ?",top=top);
#
gaga <- mn1$gamma;
grgr <- gaga[-3,-3];
print(round(var2pre(gaga),3));
print(round(var2pre(grgr),3));
#
sssform3title("'nbn.test' finished!",8);
