#
# 14_09_15
#
interactive <- FALSE;
if (interactive) {
  source("charfile.code.r");
  source("rbsa0.code.r");
} else {
  library(rbsa);
}
#
# file2text
sink("charfile.essai.txt")
cat("# comments can be included as well\n")
cat(" something\n");
cat("/* skipping from here\n");
cat("blablabla\n");
cat("  */ until here (this line is ALSO eliminated\n");
cat(" interesting:\n");
cat("un dos tres\n");
cat(")STOP(\n");
cat(" It is finished!\n");
cat(" Don't insist!\n");
sink();
cat("\n\n\n everything \n\n");
ww <- file2text("charfile.essai.txt",skip=matrix(NA,0,2),
                ended=" >< ");
print(ww);
cat("\n\n\n something \n\n");
vv <- file2text("charfile.essai.txt",skip=matrix(NA,0,2));
print(vv);
uu <- file2text("charfile.essai.txt");
cat("\n\n\n usual \n\n");
print(uu);
unlink("charfile.essai.txt");
#
#
cat("\n\n   'charfile.test.r' went to its end!\n\n");
