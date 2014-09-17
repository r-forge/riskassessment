#
# 14_05_20 14_05_21 14_05_22 14_08_29 14_09_02
# 14_09_15
#
library(documair); library(rbsa);
#
resu <- build8pkg("rbmn",".","../resu",what="pzl");
#
if (void9(resu)) {
  form3title("'make-rbmn.r' finished its work!",8);
} else {
  print(resu);
  stop("ERREUR");
}

