#
# 14_07_31 14_08_09 14_08_16 14_08_28
#
interactive <- FALSE;
if (interactive) {
  source("rbsa0.code.r");
} else {
  library(rbsa);
}


#
# void9
#
# The intent here is to find a way to also detect
#     non existing objects
#
# standard objects
fo <- vector("list",0);
fo[[1]] <- matrix(1:20,4);
fo[[2]] <- array(1:24,dim=2:4);
fo[[3]] <- list(A=fo[[1]],B=fo[[2]],C=NULL);
fo[[4]] <- data.frame(CV=sample(8),FV=rep(c("A","B"),4));
#
for (ii in 1:length(fo)) { for (vv in 0:1) {
  resu <- void9(fo[[ii]],vv);
  if (any(resu)) {
    form3display(vv);
    form3display(fo[[ii]]);
    form3display(void9(fo[[ii]],vv));
    stop("Unexpected Result!");
  }
}}
#
# void cases
eo <- vector("list",0);
eo[[1]] <- "";
eo[[2]] <- NULL;
eo[[3]] <- NA;
eo[[4]] <- vector("list",0);
#
for (ii in 1:length(eo)) { for (vv in 0:1) {
  resu <- void9(eo[[ii]],vv);
  if (!all(resu)) {
    form3display(vv);
    form3display(eo[[ii]]);
    form3display(void9(eo[[ii]],vv));
    stop("Unexpected Result!");
  }
}}
#
# direct cases
form3display(void9(""),interactive);
form3display(void9(NULL),interactive);
form3display(void9(c("","AA"),TRUE),interactive);
form3display(void9(numeric(0)),interactive);
form3display(void9(c(NA,10),TRUE),interactive);
form3display(void9(c(NA,NA),TRUE),interactive);
form3display(void9(void9),interactive);
form3display(void9(data.frame(A=letters,B=1:26)));
#
# non existing object
if (interactive) {
  form3display(void9(toto),interactive);
}
#
form3title("'void9.test.r' finished",8);
