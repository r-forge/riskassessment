chemin <- searchpaths()[grep("prr",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# exemples of pseudo-random drawings with /prr/ facilities
# 
# 10_11_22 10_11_24 10_11_25
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
form3title("Some plots about the /distri/ objects","box");
#
pau <- FALSE;
nbdraws <- 100;
#
#
tit <- "plot a /distri/ Beta distribution";
form3titre(tit,2);
d1 <- new("distri",probab=new("probab",family="beta",pafixe=c(10,20,10,20)),
                   nbdraw=nbdraws,
                   parand=matrix(c(11,50),ncol=2)
                   
         );
plot8distri(d1);
if (pau) { pause(tit);}
#
#
tit <- "plot a /distri/ normal distribution";
form3titre(tit,2);
d2 <- new("distri",probab=new("probab",family="normal",pafixe=c(-1,2)),
                   nbdraw=nbdraws,
                   parand=matrix(c(-0.1,100),ncol=2)
                   
         );
plot8distri(d2);
if (pau) { pause(tit);}
#
#
tit <- "plot a /distri/ lognormal distribution";
form3titre(tit,2);
d3 <- new("distri",probab=new("probab",family="lognormal",pafixe=c(0,1,1,2)),
                   nbdraw=nbdraws,
                   parand=matrix(c(1.5,100),ncol=2)
                   
         );
plot8distri(d3);
if (pau) { pause(tit);}
#
#
tit <- "plot a /distri/ categorical distribution";
form3titre(tit,2);
d4 <- new("distri",probab=new("probab",family="categorical",pafixe=c(0:10)),
                   nbdraw=nbdraws,
                   parand=matrix(c(3,50),ncol=2)
                   
         );
plot8distri(d4);
if (pau) { pause(tit);}
#
#
pdf("pourvoir.pdf");

############################################################
form3title("Varying the parameters of /nod/s","box");
#
nbdraws <- 10000;
#
#
tit <- "For a Beta distribution";
form3titre(tit)
n1 <- new("nod",name="n1",
                probab=new("probab",family="beta",
                                    pafixe=c(0,1,0,1)
                          ),
                assess=new("assess",x="0.2",cV=20,cU=80
                          ),
                format = c(4,-1,-1)
         );
mplot8nod(n1,nbdraws,cV=c(10,90),cU=c(10,90));
if (pau) { pause(tit);}
#
#
tit <- "For a Normal distribution";
form3titre(tit)
n2 <- new("nod",name="n2",
                probab=new("probab",family="normal",
                                    pafixe=c(-Inf,Inf)
                          ),
                assess=new("assess",x="10",cV=20,cU=80
                          ),
                format = c(4,-1,-1)
         );
mplot8nod(n2,nbdraws,cV=c(10,90),cU=c(10,90));
if (pau) { pause(tit);}
#
#
tit <- "For a LogNormal distribution";
form3titre(tit)
n3 <- new("nod",name="n3",
                probab=new("probab",family="lognormal",
                                    pafixe=c(0,1,0,50)
                          ),
                assess=new("assess",x="0.5",cV=10,cU=8
                          ),
                format = c(4,-1,-1)
         );
mplot8nod(n3,nbdraws,cV=c(10,90),cU=c(10,90));
if (pau) { pause(tit);}
#
#
tit <- "For a categorical distribution";
form3titre(tit)
n4 <- new("nod",name="n4",
                probab=new("probab",family="categorical",
                                    pafixe=c(0:10)
                          ),
                assess=new("assess",x="3",cV=10,cU=8
                          ),
                format = c(4,-1,-1)
         );
mplot8nod(n4,nbdraws,cV=c(10,90),cU=c(10,90));
if (pau) { pause(tit);}


dev.off();
