#
# 14_08_24
#
debug <- FALSE;
# loading the package
if (debug) {
  source8file(Sys.glob("*.code.r"));
  voir <- 1;
  pau <- TRUE;
} else {
  library(rbmn);
  voir <- NA;
  pau <- FALSE;
}
rbmn0 <- complete8rbmn0(rbmn0);
#
## testing read8chain
#
print8chain(rbmn0$chain1$v);
write8chain(rbmn0$chain1$v,file="toto.txt");
print8chain(read8chain("toto.txt"));
unlink("toto.txt");
#
## finished
sssform3title("chain.test.r finished at the end",8);
