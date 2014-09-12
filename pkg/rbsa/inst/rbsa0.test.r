#
# 14_07_31 14_08_09 14_08_16
#
interactive <- FALSE;
if (interactive) {
  source("rbsa0.code.r");
} else {
  library(rbsa);
}
#
# void9
aa <- matrix(1:20,4);
aaa <- array(1:24,dim=2:4);
form3display(object9(aa,"matrix"),pau=interactive);
form3display(object9(aa,"matrix",speci=matrix(c(1,10,10,NA),2)),pau=interactive);
form3display(object9(aa,"matrix",speci=matrix(c(1,10,10,23),2),fatal=FALSE),pau=interactive);
form3display(object9(aaa,"array",speci=matrix(c(1,1),ncol=2),fatal=FALSE),pau=interactive);
form3display(object9(aaa,"array",speci=matrix(c(1,5),ncol=2),fatal=FALSE),pau=interactive);
form3display(object9(aaa,"array",speci=matrix(c(1,1,1,1,3,2,3,3),ncol=2),fatal=FALSE),pau=interactive);
form3display(object9(aaa,"array",speci=matrix(c(1,1,1,NA,3,2,3,3),ncol=2),fatal=FALSE),pau=interactive);
#
form3display(void9(""),interactive);
form3display(void9(c("","AA"),TRUE),interactive);
form3display(void9(numeric(0)),interactive);
form3display(void9(c(NA,10),TRUE),interactive);
form3display(void9(c(NA,NA),TRUE),interactive);
form3display(void9(void9),interactive);
form3display(void9(data.frame(A=letters,B=1:26)));
#
  
#
if (interactive) {
  # pause
  pause("essai simple");
  pause("essai avec r'eponse",top="stop");
  pause("essai avec r'eponses",top=c("stop","arret"));
  pause("essai simple","et un peu de details en plus");
  pause("essai avec r'eponse","et un peu de details en plus",top="stop");
  pause("essai avec r'eponses","et un peu de details en plus",top=c("stop","arret"),ans=c("non"));
}
#
form3title("'rbsa0.test.r' finished",8);
