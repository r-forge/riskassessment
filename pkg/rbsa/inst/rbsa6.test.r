#
# 14_09_03
#
interactive <- FALSE;
if (interactive) {
  source("rbsa6.code.r");
} else {
  library(rbsa);
}
#
# text3replace
tt <- rbsa0$text2$v;
let <- matrix(c("a","e","i","o","u","y","A","E","I","O","U","Y"),ncol=2);
cache <- c("her","---");
expre <- c("her","Chaperon Rouge");
let2 <- rbind(expre,let);
alpha <- c(letters,LETTERS);
form3display(tt,pau=interactive);
form3display(text3replace(tt,cache),pau=interactive);
form3display(text3replace(tt,c("her","?")),pau=interactive);
form3display(text3replace(tt,expre),pau=interactive);
form3display(text3replace(tt,let),pau=interactive);
form3display(text3replace(tt,let,b.accepted=" "),pau=interactive);
form3display(text3replace(tt,let,b.accepted=" ",a.rejected=" "),pau=interactive);
form3display(text3replace(tt,let,a.accepted=c(" ","t")),pau=interactive);
form3display(text3replace(tt,let2,a.accepted=c(" ","t")),pau=interactive);
form3display(text3replace(tt,let2,a.rejected=alpha,b.rejected=alpha),pau=interactive);
#
# text3acceptance
tt <- "Juste pour voir";
print(tt);
form3display(text3acceptance(tt,c(1,1)),pau=interactive);
form3display(text3acceptance(tt,c(3,5)),pau=interactive);
form3display(text3acceptance(tt,c(3,5),b.accepted=c("Ju","st")),pau=interactive);
form3display(text3acceptance(tt,c(3,5),b.rejected=c("Ju","st")),pau=interactive);
#
form3title("'rbsa6.test.r' finished",8);
