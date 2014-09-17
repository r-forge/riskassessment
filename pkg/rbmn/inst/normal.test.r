#
# 14_08_19
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
## testing the simulation
set.seed(1234);
sisi1 <- simulate8mn(rbmn0$mn1$v,5);
sssform3display(sisi1,pau=debug);
loi <- list(mu=c(D=2,E=4),
            rho=matrix(1:6,2,dimnames=list(LETTERS[4:5],
                                          LETTERS[1:3])),
            gamma=matrix(c(1,1,1,2),2,dimnames=list(LETTERS[4:5],LETTERS[4:5])));
cova <- matrix(runif(36),12,dimnames=list(NULL,LETTERS[1:3]));
sisi2 <- simulate8gmn(loi,cova,12);
sssform3display(sisi2,pau=debug);
#
## testing print8mn
print8mn(rbmn0$mn1$v);
loi <- list(mu=c(D=2,E=4),
            rho=matrix(1:6,2,dimnames=list(LETTERS[4:5],
                                          LETTERS[1:3])),
            gamma=matrix(c(1,1,1,2),2,dimnames=list(LETTERS[4:5],LETTERS[4:5])));
print8mn(loi);
ssspause("printing",top=voir);
#
## finished
sssform3title("normal.test.r finished at the end",8);
