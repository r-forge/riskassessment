#
# 14_06_25
#
# testing dipa and fidi9 functions
#
# "rbsa0.code.r" and "loop.code.r" are supposed to have been sourced
# (or rbsa library loaded).
library(rbsa);
# Introduce here any directories/files existing/imaginary of your own
# using right and wrong separators
mydifi <- c( "/","~", "~/.bashrc",  "~/.bashrc/", "~/","tututtu",  "/tmp/ouafe",
            "//","~","~//.bashrc","~//.bashrc//","~//","tututtu", "//tmp/ouafe",
            "\\","~","~\\.bashrc","~\\.bashrc\\","~\\","tututtu","\\tmp\\ouafe"
           );
#
# Adding some existing directory and file
prdi <- getwd();
prfi <- Sys.glob("*");
prdifi <- file.path(prdi,prfi);
difi <- c(mydifi,prdi,prfi,prdifi);
#
# testing
for (xx in difi) {
  nn <- dipa(xx);
  rx <- fidi9(xx);
  rn <- fidi9(nn);
  cat("            ",rx," : ",xx,"\n");
  cat(" normalized ",rn," : ",nn,"\n");
}
