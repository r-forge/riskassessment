chemin <- searchpaths()[grep("rbsb",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# using empirical objects and related functions
# 
# 11_01_19
#
#

# from an existing text file
if (!exists("chemin")) {
    cat("A file into the private 'files' directory is needed\n");
    erreur("The package must be installed to run this demo");
}

############################################################
form3titre("Inversing a parabola");
mfao <- rbsb.mfa;
rbsb.mfa <- FALSE;
xx <- 1:10; 
for (ky in 0:2) { for (kx2 in 0:2) { for (kx in 0:2) { for (k0 in 0:2) {
  yy <- -(kx2*xx^2 + kx*xx + k0);
  if (ky!=0) { yy <- yy/ky;}
  uu <- solve2degree(yy,ky,kx2,kx,k0);
  cat("ky =",ky," kx2 =",kx2," kx =",kx," k0=",k0,"\n");
  yya <- yy*sign(ky);
  if(!all((uu[,1]-xx)*(uu[,ncol(uu)]-xx)==0,na.rm=TRUE)) {
    print(cbind(yya,xx,uu));
    if (!all(is.na(uu)) &
        (ky!=0)         &
        (kx2!=0) & (k0!=0)
       ) { pause("Bad?");};
  }
}}}}
rbsb.mfa <- mfao;
#
# more trials
print(solve2degree(seq(0,5,0.5),-1,1,2,0,c(0,20)));

#
xx <- seq(0,1,0.1);
for (k in c(-1,1)) {
  ky <- -k; kx2 <- 2*k; kx <- -1*k; kk <- 0;
  yy <- (kx2*xx^2 + kx*xx + kk)/k;
  r1 <- solve2degree(yy,ky,kx2,kx,kk);
  r2 <- solve2degree(yy,ky,kx2,kx,kk,range(xx));
  print(round(cbind(xx,yy,r1=r1,r2=r2,dif=xx-r2),3));
}

############################################################
form3titre("Checking [p|q] empirical");
N <- 10000;
yy <- seq(0,1,length=N);
for (ii in bc(7)) {
  vv <- paste("ee <- rbsb.epi",ii,sep="");
  eval(parse(text=vv));
  xr <- range5empirical(ee)$remp;
  xx <- seq(xr[1],xr[2],length=N);
  yyy <- pempirical(xx,ee);
  xxx <- qempirical(yy,ee);
  par(mfrow=c(3,1));
  plot(ee,what="c");
  plot(xx,yyy,type="l",ylim=0:1,main="pempirical");
  plot(xxx,yy,type="l",ylim=0:1,main="qempirical");
  #pause();
}

############################################################
form3titre("drawing pseudo-random values");
for (ii in 2:6) {
  N <- 10^ii;
  cat("N =",N,system.time(rempirical(N,triangular2empirical())),
#              system.time(rtriang(N)),
      "\n");
}
