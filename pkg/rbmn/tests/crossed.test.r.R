#
# 14_04_02 14_08_12 14_08_14
#
debug <- FALSE;
# loading the package
if (debug) {
  source8file(Sys.glob("*.code.r"));
  voir <- 1;
} else {
  library(rbmn);
  voir <- NA;
}
rbmn0 <- complete8rbmn0(rbmn0);
#
library(bnlearn);
library(igraph);
#
gg <- function(nbn) {
  dag <- nbn2bnfit(nbn$nbn,TRUE);
  dag.ig <- igraph.from.graphNEL(as.graphNEL(dag));
  V(dag.ig)$label <- V(dag.ig)$name;
  plot(dag.ig,layout=nbn$posi[V(dag.ig)$label,]);
}
#
pdf("crossed.test.pdf");
par(mfrow=c(1,1));
nbn1 <- generate8grid7crossed(2,2);
print8nbn(nbn1$nbn);
gg(nbn1);
#
nbn2 <- generate8grid7crossed(rhorow=matrix(1:2,2,3));
print(nbn2$cova2vari);
print(nbn2$row2row);
print(nbn2$col2col);
print(nbn2$vari2cova);
ssspause("arcs",top=voir);
print8nbn(nbn2$nbn);
gg(nbn2);
#
nbn3 <- generate8grid7crossed(rhorow=matrix(1:2,2,3),
                              rhocol=matrix(10*(1:2),2,3),
                              xrhorow=matrix(100*(1:3),3),
                              xrhocol=matrix(1000*(1:3),3));
print8nbn(nbn3$nbn);
gg(nbn3);
#
nbn4 <- generate8grid7crossed(nrow=4,ncol=5,xrow=2,xcol=4);
print8nbn(nbn4$nbn);
gg(nbn4);
dev.off();
#
for (nro in c(2,4,7,12)) {
  for (nco in c(2,4,7,12)) {
    for (xro in c(0,2,7)) {
      for (xco in c(0,2,7)) {
        uuu <- generate8grid7crossed(nro,nco,xro,xco);
        cat(nro,nco,xro,xco,"\n");
        checo <- check8nbn(uuu$nbn);
        if (length(checo)>0) {
          print(checo);
          stop("va pas !");
        }
      }
    }
  }
}
print8nbn(uuu$nbn);
