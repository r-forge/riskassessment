#
# 13_11_28 13_11_29 13_12_01 13_12_02 13_12_04
# 13_12_05
#
library(rbsa);
library(rbmn);
source("stars.code.r");
xx1 <- create8star();
print(xx1);
vv1 <- create8star(quoi="vari");
print(vv1);
print(xx1%*%t(xx1));
pause("regarde");
#
for (ni in c(0,1,3,10,20,30)) {
  for (no in c(0,1,3,7,15,50,100)) {
    xx <- create8star(rep(1,ni),rep(1,no),
                      rep(1,ni),1,rep(1,no),
                      "gema");
    vv <- create8star(rep(1,ni),rep(1,no),
                      rep(1,ni),1,rep(1,no),
                      "vari");
    dd <- sum((xx%*%t(xx)-vv)^2);
    cat(ni,no,dd,"\n");
  }
}
#
form3display(vv1);
form3display(round(solve(vv1),3));
dia1 <- svd(vv1);
form3display(dia1$d);
form3display(round(dia1$u,4));
#
vv2 <- create8star(rho=rep(0.4,4),
                   lambda=rep(0.5,2),
                   quoi="vari");
form3display(vv2);
form3display(round(solve(vv2),3));
dia2 <- svd(vv2);
form3display(dia2$d);
form3display(round(dia2$u,4));
#
vv3 <- create8star(rho=rep(0.4,4),
                   lambda=c(0.2,0.4,0.6),
                   quoi="vari");
form3display(vv3);
form3display(round(solve(vv3),3));
dia3 <- svd(vv3);
form3display(dia3$d);
form3display(round(dia3$u,4));
#
vv4 <- create8star(rho=(1:4)/10,
                   lambda=c(0.2,0.4,0.6),
                   quoi="vari");
form3display(vv4);
form3display(round(solve(vv4),3));
dia4 <- svd(vv4);
form3display(dia4$d);
form3display(round(dia4$u,4));
#
for (rho in c(0.1,0.2,0.3)) {
  for (lambda in c(0.1,0.2,0.3)) {
    vv5 <- create8star(rho=rho,
                       lambda=lambda,
                       quoi="vari");
    cat("\n\n\n rho =",rho," et lambda =",lambda,"\n\n");
    form3display(vv5);
    form3display(round(solve(vv5),3));
    dia5 <- svd(vv5);
    form3display(dia5$d);
    form3display(round(dia5$u,4));
  }
}
pause("diagonalisation");
