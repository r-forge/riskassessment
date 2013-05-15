#1
options(digits=4, prompt="R>", 
        SweaveHooks=list(fig=function() par(mar=c(5.1, 4.1, 1.1, 2.1))))
set.seed(1234)

#2
library("fitdistrplus")
data("groundbeef")
str(groundbeef)

#3
plotdist(groundbeef$serving)

#4
descdist(groundbeef$serving, boot=1000)

#5
fw <- fitdist(groundbeef$serving, "weibull")
summary(fw)

#6
fg <- fitdist(groundbeef$serving,"gamma")
fln <- fitdist(groundbeef$serving,"lnorm")
par(mfrow=c(2, 2))
denscomp(list(fw,fln,fg), legendtext=c("Weibull", "lognormal", "gamma"))
qqcomp(list(fw,fln,fg), legendtext=c("Weibull", "lognormal", "gamma"))
cdfcomp(list(fw,fln,fg), legendtext=c("Weibull", "lognormal", "gamma"))
ppcomp(list(fw,fln,fg), legendtext=c("Weibull", "lognormal", "gamma"))

#7
data("endosulfan")
str(endosulfan)

#8
ATV <-endosulfan$ATV
fendo.ln <- fitdist(ATV, "lnorm")
library("actuar")
fendo.ll <- fitdist(ATV, "llogis", start=list(shape=1, scale=500))
fendo.P <- fitdist(ATV, "pareto", start=list(shape=1, scale=500))
fendo.B <- fitdist(ATV, "burr", start=list(shape1=0.3, shape2=1, rate=1))
cdfcomp(list(fendo.ln, fendo.ll, fendo.P, fendo.B), xlogscale = TRUE,
        ylogscale = TRUE, legendtext = 
          c("lognormal", "loglogistic", "Pareto", "Burr"))

#9
quantile(fendo.B, probs = 0.05)
quantile(ATV, probs = 0.05)

#10
gofstat(list(fendo.ln, fendo.ll, fendo.P, fendo.B),
        fitnames = c("lnorm","llogis","Pareto","Burr"))

#11
bendo.B <- bootdist(fendo.B, niter=1001)
summary(bendo.B)
plot(bendo.B)

#12
quantile(bendo.B, probs = 0.05)

#13
fendo.ln.ADL <- fitdist(ATV,"lnorm",method="mge",gof="ADL")
fendo.ln.AD2L <- fitdist(ATV,"lnorm",method="mge",gof="AD2L")
cdfcomp(list(fendo.ln, fendo.ln.ADL, fendo.ln.AD2L),
        xlogscale = TRUE, ylogscale = TRUE,
        main = "Fitting a lognormal distribution",xlegend = "bottomright",
        legendtext = c("MLE","Left-tail AD", "Left-tail AD 2nd order"))

#14
(HC5.estimates <- c(
  empirical = as.numeric(quantile(ATV, probs=0.05)),
  Burr = as.numeric(quantile(fendo.B, probs=0.05)$quantiles),
  lognormal_MLE = as.numeric(quantile(fendo.ln, probs=0.05)$quantiles),
  lognormal_AD2 = as.numeric(quantile(fendo.ln.ADL, probs=0.05)$quantiles),
  lognormal_AD2L = as.numeric(quantile(fendo.ln.AD2L,probs=0.05)$quantiles)))

#15
data("danishuni")
str(danishuni)
fdanish.ln.MLE <- fitdist(danishuni$Loss, "lnorm")
fdanish.ln.MME <- fitdist(danishuni$Loss, "lnorm", method="mme", order=1:2)
cdfcomp(list(fdanish.ln.MLE, fdanish.ln.MME), 
        legend=c("lognormal MLE", "lognormal MME"), 
        main="Fitting a lognormal distribution",
        xlogscale=TRUE, datapch=20)

#16
library("actuar")
fdanish.P.MLE <- fitdist(danishuni$Loss, "pareto", 
                         start=c(shape=10, scale=10), lower=2+1e-6, upper=Inf)
memp <- function(x, order) sum(x^order)/length(x)
fdanish.P.MME <- fitdist(danishuni$Loss, "pareto", method="mme", order=1:2, 
                         memp="memp", start=c(shape=10, scale=10), lower=c(2+1e-6,2+1e-6), 
                         upper=c(Inf,Inf))
par(mfrow=c(1, 2))
cdfcomp(list(fdanish.ln.MLE, fdanish.ln.MME), 
        legend=c("lognormal MLE", "lognormal MME"), main="Fitting a lognormal distribution",
        xlogscale=TRUE, datapch=20)
cdfcomp(list(fdanish.P.MLE, fdanish.P.MME), 
        legend=c("Pareto MLE", "Pareto MME"), main="Fitting a Pareto distribution",
        xlogscale=TRUE, datapch=20)

#17
cdfcomp(list(fdanish.P.MLE, fdanish.P.MME), 
        legend=c("Pareto MLE", "Pareto MME"), 
        main="Fitting a Pareto distribution", xlogscale=TRUE, datapch=".")
gofstat(list(fdanish.ln.MLE, fdanish.P.MLE, 
             fdanish.ln.MME, fdanish.P.MME),
        fitnames = c("lnorm.mle","Pareto.mle","lnorm.mle","Pareto.mme"))

#18
fdanish.ln.QME1 <- fitdist(danishuni$Loss, "lnorm", method="qme", 
                           probs=c(1/3, 2/3))
fdanish.ln.QME2 <- fitdist(danishuni$Loss, "lnorm", method="qme", 
                           probs=c(8/10, 9/10))
cdfcomp(list(fdanish.ln.MLE, fdanish.ln.QME1, fdanish.ln.QME2), 
        legend=c("MLE", "QME(1/3, 2/3)", "QME(8/10, 9/10)"), 
        main="Fitting a lognormal distribution", xlogscale=TRUE, datapch=20)

#19
cdfcomp(list(fdanish.ln.MLE, fdanish.ln.QME1, fdanish.ln.QME2), 
        legend=c("MLE", "QME(1/3, 2/3)", "QME(8/10, 9/10)"), main="Fitting a lognormal distribution",
        xlogscale=TRUE, datapch=20)

#20
data("groundbeef")
fNM <- fitdist(groundbeef$serving, "gamma", optim.method="Nelder-Mead")
fBFGS <- fitdist(groundbeef$serving, "gamma", optim.method="BFGS") 
fSANN <- fitdist(groundbeef$serving, "gamma", optim.method="SANN")
fCG <- try(fitdist(groundbeef$serving, "gamma", optim.method="CG", 
                   control=list(maxit=10000)))
if(class(fCG) == "try-error")
  fCG <- list(estimate=NA)

#21
mygenoud <- function(fn, par, ...) 
{
  require(rgenoud)
  res <- genoud(fn, starting.values=par, ...)        
  standardres <- c(res, convergence=0)
  return(standardres)
}

#22
fgenoud <- mledist(groundbeef$serving, "gamma", custom.optim= mygenoud, 
                   nvars=2, max.generations=10, Domains=cbind(c(0,0), c(10,10)), 
                   boundary.enforcement=1, hessian=TRUE, print.level=0, P9=10)
cbind(NM = fNM$estimate,
      BFGS = fBFGS$estimate,
      SANN = fSANN$estimate,
      CG = fCG$estimate,
      fgenoud = fgenoud$estimate)
#23
data("salinity")
str(salinity)

#24
plotdistcens(salinity,Turnbull = FALSE)

#25
fsal.ln <- fitdistcens(salinity, "lnorm")
fsal.ll <- fitdistcens(salinity, "llogis", start=list(shape=5, scale=40))
summary(fsal.ln)
summary(fsal.ll)

#26
cdfcompcens(list(fsal.ln, fsal.ll), 
            legendtext=c("lognormal", "loglogistic "))

#27
data("toxocara")
str(toxocara)

#28
(ftoxo.P <- fitdist(toxocara$number, "pois"))
(ftoxo.nb <- fitdist(toxocara$number, "nbinom"))

#29
cdfcomp(list(ftoxo.P,ftoxo.nb),
        legendtext=c("Poisson", "negative binomial"))

#30
gofstat(list(ftoxo.P,ftoxo.nb),
        fitnames = c("Poisson","negative binomial"))




