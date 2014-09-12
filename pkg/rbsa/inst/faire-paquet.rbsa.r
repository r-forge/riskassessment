#
# 14_05_20 14_05_21 14_06_17 14_08_04 14_08_25
# 14_08_29 14_09_01 14_09_04
#
rbsapa     <- TRUE;
documairpa <- TRUE;
rbsach     <- "/home/jbdenis/attente.liana/inra/paquets/rbsa/pro/perso/";
documairch <- "/home/jbdenis/attente.liana/inra/paquets/documair/pro/perso/";
rbsach     <- "/home/jbdenis/bananier/inra/p/r/paquets/rbsa/pro/perso/";
documairch <- "/home/jbdenis/bananier/inra/p/r/paquets/documair/pro/perso/";
if (documairpa) {
  library(documair);
} else {
  if (rbsapa) {
    library(rbsa);
  } else {
    rbsaso <- Sys.glob(paste0(rbsach,"*.code.r"));
    if (length(rbsaso)==0) {
      print(rbsaso);
      stop("No /rbsa/ code files found!");
    }
    for (rbsafi in rbsaso) {
      source(rbsafi);
    }
  }
  documairso <- Sys.glob(paste0(documairch,"*.code.r"));
  if (length(documairso)==0) {
    print(documairso);
    stop("No /documair/ code files found!");
  }
  for (documairfi in documairso) {
    source(documairfi);
  }
}
#
build8pkg("rbsa",".","../resu",display=TRUE);
#
cat("faire-paquet a fini son travail !\n");
