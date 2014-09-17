#
# 14_08_13
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
## testing the topological ordering
#
sssform3display(topo9(list(A=NULL,B=NULL,C=list(parents=c("A","B")))),pau=pau);
sssform3display(topo9(list(A=list(parents="B"),B=NULL,C=list(parents=c("A","B")))),pau=pau);
sssform3display(topo9(list(A=list(parents="B"),
                        B=list(parents="C"),
                        C=list(parents=c("A","B")))),pau=pau);
sssform3display(topo9(list(A=list(parents="B"),
                        B=list(parents="C"),
                        C=list(parents=c("C")))),pau=pau);
sssform3display(topo9(list(A=list(parents=character(0)),
                        B=list(parents="C"),
                        C=list(parents=c("B")))),pau=pau);
sssform3display(topo9(list(A=list(parents=character(0)),
                        B=list(parents="D"),
                        C=list(parents="B"),
                        D=list(parents="C"))),pau=pau);
sssform3display(topo9(list(A=list(parents="C"),
                        B=list(parents="A"),
                        C=list(parents="B"),
                        D=list(parents="C"))),pau=pau);
#
## finished
sssform3title("nbnmi.test.r finished at the end",8);
