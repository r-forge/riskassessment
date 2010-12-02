chemin <- searchpaths()[grep("prr",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# exemples of pseudo-random drawings with /prr/ facilities
# 
# 10_11_18 10_11_22 10_11_29 10_12_03
#
#

prr3k("RESET");

# from an existing text file
if (!exists("chemin")) {
    cat("A file into the private 'files' directory is needed\n");
    erreur("The package must be installed to run this demo",
           "or chemin must be defined accordingly");
}

############################################################
form3title("Some tests about the /distri/ objects","box");
#
tpau <- FALSE;
nbdraws <- 100;
#
#
dpb1 <- new("distri",
           probab=new("probab",family="normal",pafixe=c(0,Inf)),
           nbdraw=100,
           parand=matrix(c(0.0001,50),nrow=1)
          );
print(dpb1);
print(rdistri(dpb1));
if (tpau) {pause("normal onto [0,Inf]");}
#
#
dpb2 <- new("distri",
           probab=new("probab",family="lognormal",pafixe=c(0,1,1,Inf)),
           nbdraw=10,
           parand=matrix(c(1.1,50),nrow=1)
          );
print(dpb2);
print(rdistri(dpb2));
if (tpau) {pause("lognormal with 10000");}
#
#
form3titre("/distri/ for a Beta distribution",2);
d1 <- new("distri",probab=new("probab",family="beta",
                              pafixe=c(10,20,10,20)),
                   nbdraw=nbdraws,
                   parand=matrix(c(11,20),ncol=2)
         );
for (empha in c(0,1,2,10)) {
  print(d1,empha=empha);
  if (tpau) { pause(paste("beta /distri/",empha));}
}
r1 <- rdistri(d1);
print(head(r1));
if (tpau) { pause("Drawing with 'd1'");}
#
#
form3titre("/distri/ for a normal distribution",2);
d2 <- new("distri",probab=new("probab",
                              family="normal",
                              pafixe=c(-Inf,Inf)),
                   nbdraw=nbdraws,
                   parand=matrix(c(11,20),nrow=1)
         );
for (empha in c(0,1,2,10)) {
  print(d2,empha=empha);
  if (tpau) { pause(paste("normal /distri/",empha));}
}
r2 <- rdistri(d2);
print(head(r2));
if (tpau) { pause("Drawing with 'd2'");}
#
#
form3titre("/distri/ for a lognormal distribution",2);
d3 <- new("distri",probab=new("probab",family="lognormal",
                   pafixe=c(0,1,0,Inf)),
                   nbdraw=nbdraws,
                   parand=matrix(c(11,20),nrow=1)
         );
for (empha in c(0,1,2,10)) {
  print(d3,empha=empha);
  if (tpau) { pause(paste("lognormal /distri/",empha));}
}
r3 <- rdistri(d3);
print(head(r3));
if (tpau) { pause("Drawing with 'd3'");}
#
#
form3titre("/distri/ for a categorical distribution",2);
d4 <- new("distri",probab=new("probab",family="categorical",
                   pafixe=c(10:20)),
                   nbdraw=nbdraws,
                   parand=matrix(c(11,20),nrow=1)
         );
for (empha in c(0,1,2,10)) {
  print(d4,empha=empha);
  if (tpau) { pause(paste("categorical /distri/",empha));}
}
r4 <- rdistri(d4);
print(head(r4));
if (tpau) { pause("Drawing with 'd4'");}
#
#
form3titre("the /distri/ constants",2);
print(prr.distri1,empha=1);
print(prr.distri2,empha=1);
print(prr.distri3,empha=1);
print(prr.distri4,empha=1);
if (tpau) { pause("the standard distributions provided as constants");}
