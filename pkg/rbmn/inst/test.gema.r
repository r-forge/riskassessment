#
# 13_07_05
#
library(rbsb);
source("more.code.r");
#
nbn1 <- rbmn0nbn.04;
#nbn1 <- rbmn0nbn.02;
form3title("Initial /nbn/");
print8nbn(nbn1);
#
gema1 <- nbn2gema(nbn1);
form3title("Transformed into /gema/");
print8gema(gema1);
#
nbn2 <- gema2nbn(gema1);
form3title("Back into /nbn/");
print8nbn(nbn2);
