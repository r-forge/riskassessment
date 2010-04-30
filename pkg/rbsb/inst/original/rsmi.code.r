
rs003k("reset");
rsba3k("reset");
rsgn3k("reset");
rsbn3k("reset");

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rebastaba <- function()
#TITLE (mi) draws some Bayesian networks for illustration
#DESCRIPTION
#   This function draws some Bayesian networks using
# rebastaba spelling to serve as a cover figure for the manual
#DETAILS
#KEYWORDS
#INPUTS
#[INPUTS]
#VALUE
# nothing but a plot is performed
#EXAMPLE
# rsgn3k("RESET");
# rebastaba();
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_25
#REVISED 10_02_02
#--------------------------------------------
{
# computing some constants
nom <- new("nom",x=list(RE="",B1="",B2="",A1="",A2="",A3="",ST=""));
posi <- matrix(c(1, 2, 2,  3, 3,  3, 4,
                 2, 3, 1,3.5, 2,0.5, 2,
                 0, 0, 0,  0, 0,  0, 0,
                 0, 0, 0,  0, 0,  0, 0),ncol=4,byrow=FALSE);
zoom <- c(0,0,0,1);
xr <- c(0,5); yr <- c(0,4);
XR <- c(0,16.5);   YR <- c(-1,15);
B <- 2:3; A <- 4:6;
fle <- matrix(211,6,3);
# performing the plots
pg <- rbsb.pgr; pg@cexscale <- 0.6;
pg@diar <- 0.25; pg@arrowlength <- 0.06;
par(mfrow=c(1,1));
bas <- c(0.5,-0.5); dx <- 4; dy <- 5;
par(mar=c(2,2,2,2));
plot(1,1,xlim=XR,ylim=YR,typ="n",
     xlab="",ylab="",axes=FALSE);
for (i in 0:4) { lines(rep(bas[1]+i*dx,2),c(bas[2],bas[2]+3*dy));}
for (j in 0:3) { lines(c(bas[1],bas[1]+4*dx),rep(bas[2]+j*dy,2));}
k <- 0;
for (a1 in A) { for (b1 in B) { for (a2 in A) {
    if (a2 != a1) { for (b2 in B) { if (b2 != b1) {
        for (a3 in A) { if (!(a3 %in% c(a1,a2))) {
            fle[1,1:2] <- c( 1,b1);
            fle[2,1:2] <- c(b1,a1);
            fle[3,1:2] <- c(a1, 7);
            fle[4,1:2] <- c( 7,a2);
            fle[5,1:2] <- c(a2,b2);
            fle[6,1:2] <- c(b2,a3);
            gg <- new("gn",description=new("des",name="gg"),nom=nom,
                      item="n",
                      arc=new("arc",nbn=7,fle=fle),
                      pos=new("pos",posi=posi,
                                    zoom=zoom,
                                    view=c(0,0)));
            k <- k + 1;
            jg <- which(a1==A); ig <- k - 4*(jg-1);
            ajout <- matrix(rep(c((ig-1)*4,(jg-1)*5),7),7,byrow=TRUE);
            xy <- pos2pos(gg@pos)@posi[,1:2] + ajout;
            gg@pgr <- pg;
            plot8cgn(gg,xy);
        }}
    }}}
}}}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
