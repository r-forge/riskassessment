chemin <- searchpaths()[grep("prr",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# probalizing the RR genuine chain
# 
# 10_11_29
#
#

prr3k("RESET");
tpau <- FALSE;
set.seed(1234);

# usual warning
if (!exists("chemin")) {
    cat("A file into the private 'files' directory is needed\n");
    erreur("The package must be installed to run this demo");
}

sink("prr.demo.rr.txt");

tt1 <- paste(chemin,"prr.demo.ex03.txt",sep="");
#
############################################################
tit <- paste("Reading the chain from",tt1);
form3titre(tit);
#
rrchain <- read8chain(tt1);
print(rrchain);
if (tpau) { pause(tit);}
#
############################################################
tit <- paste("Simulating the obtained chain (first six simulations)");
form3titre(tit);
#
rrsimu <- simu8chain(rrchain,5000);
cat("\n");
print(t(head(rrsimu$df)));
cat("\n\n");
print(rrsimu$rr);
if (tpau) { pause(tit);}
#
############################################################
tit <- paste("Summarizing the simulations");
form3titre(tit);
#
cat("\n");
pdf("prr.demo.rr.pdf");
cat("\n\n\n\n");
look4simu(rrsimu,what="u",how="Sp",who="X",
          selec=c("ProbP","ProbD","Disea","CRisk","CRank"))
dev.off();
if (tpau) { pause(tit);}
#
sink();
