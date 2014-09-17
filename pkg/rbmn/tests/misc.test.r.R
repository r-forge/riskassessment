#
# 14_04_07 14_08_11
#
# testing misc.code.r functions
#
debug <- FALSE;
# loading the package
if (debug) {
  source8file(Sys.glob("*.code.r"));
} else {
  library(rbmn);
}
rbmn0 <- complete8rbmn0(rbmn0);
#
aa1 <- rbmn0$adja2$v;
sssform3title("Adjacency Matrix");
print(aa1);
sssform3title("Its raw dissimilarity expression");
print(dissim4adja(aa1,nature="d"));
sssform3title("Its symetricized dissimilarity expression");
print(dissim4adja(aa1));

