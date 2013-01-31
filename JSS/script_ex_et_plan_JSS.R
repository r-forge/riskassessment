# Exemples pour le papier JSS
require(fitdistrplus)
require(actuar)
options(digits=3)

#### 1/ intro

#### 2/ Fitting distributions to continuous non-censored data

#### 2.1/ Choice of candidate distributions

data(endosulfan)
ATV <-endosulfan$ATV
plotdist(ATV) # pas sûr que celui-là soit très intéressant sur cet exemple
# mais on n'est pas obligé de mettre toutes les figures

descdist(ATV,boot=1000)
# sur celle-là je commenterais le fait qu'en général les écotoxicologues
# ajustent soit une lognormale soit une loglogistique sur ce type de données
# mais qu'il semble qu'ici la skewness soit plus forte qu'attendue 
# pour une loi normale, par rapport à la kurtosis

# cet exemple n'est pas forcément très illustratif pour cette partie mais intéressant
# pour la suite
# ???? FAUT-IL démarrer par un exemple plus simple (l'actuel de la vignette)
# ???? et présenter celui-là ensuite uniquement?
# ???? j'attends ton avis là-dessus

#### 2.2/ Fit of distributions by MLE and comparison of fits

fln <- fitdist(ATV, "lnorm")
summary(fln)
gofstat(fln)

# use of plotdist to find good reasonable initial values for parameters
plotdist(ATV, "llogis", para=list(shape=1,scale=500))
fll <- fitdist(ATV, "llogis", start=list(shape=1,scale=500))

# use of plotdist to find good reasonable initial values for parameters
plotdist(ATV, "pareto", para=list(shape=1,scale=500))
fP <- fitdist(ATV, "pareto", start=list(shape=1,scale=500))

# definition of the initial values from the fit of the Pareto
# as the Burr distribution is the Pareto when shape2 == 1
fB <- fitdist(ATV, "burr", start=list(shape1=0.3,shape2=1,rate=1))

x11()
par(mfrow=c(2,2))
cdfcomp(list(fln,fll,fP,fB),xlogscale=TRUE,
          legendtext = c("lognormal","loglogistic","Pareto","Burr"))
qqcomp(list(fln,fll,fP,fB),xlogscale=TRUE,ylogscale=TRUE,
       legendtext = c("lognormal","loglogistic","Pareto","Burr"))
# A partir de ce QQplot, on peut bien expliquer le fait que si l'on veut
# utiliser ensuite l'ajustement pour estimer un quantile extrême (ex.
# qui est le cas en écotox sur ce type de données, on veut la HC5 qui protège
# 95% des espèces, donc le quantile à 5%)
# il est mieux d'utiliser la loi de Burr ou la limite celle de Pareto
ppcomp(list(fln,fll,fP,fB),xlogscale=TRUE,ylogscale=TRUE,
       legendtext = c("lognormal","loglogistic","Pareto","Burr"))

# ci-dessous deux versions de denscomp : du fait qu'on ne peut pas mettre d'argument
# log="x" dans la fonction hist et donc pas non plus dans denscomp qui appelle hist
# je préfère la première version. 
# ???? qu'en penses-tu?
# ???? vois-tu une meilleure solution
# ???? on peut aussi ne pas montrer le denscomp mais c'est un peu dommage
# ???? et on risque de nous le demander non?

denscomp(list(fln,fll,fP,fB),
         legendtext = c("lognormal","loglogistic","Pareto","Burr"),
         breaks=seq(0,40000,5),xlim=c(0.001,200),ylim=c(0,0.1)) # without great interest as hist does accept argument log="x"
denscomp(list(fln,fll,fP,fB),
         legendtext = c("lognormal","loglogistic","Pareto","Burr"),
         breaks=exp(seq(-3,11,1)),xlim=c(0.001,100),ylim=c(0,0.3)) # without great interest as hist does accept argument log="x"

summary(fln)$aic
summary(fll)$aic
summary(fP)$aic
summary(fB)$aic

gofstat(fln)
gofstat(fll)
gofstat(fP)
gofstat(fB)

# Discussion à partir des summary et des gofstat des méthodes de comparaison
# de goodness-of-fit et notamment des intérêts et limites des stat du style
# AD, CvM
# ???? je ne sais pas s'il faut séparer cette partie en 2 ou non
# ???? on pourrait faire ajustement (2.2 puis comparaison des ajustements 2.3)
# ???? mais alors où met-on les graphes d'ajustement, et ne va-t-on pas faire
# ???? des aller-retour inutiles sur les AIC, BIC qui sont en sortie de summary
# ???? et sont finalement assez importants pour comparer des ajsutements
# ???? notamment de distributions n'ayant pas le même nombre de par
# ???? (une des raisons de mon intérêt pour cet exemple d'ailleurs)
# ???? TON AVIS?

# on pourrait terminer par la comparaison des quantiles (HC5 ou HC10)
# estimés par la loi classiquement utilisée (lognormale) et par la loi
# qui semble la meilleure ici, pour montrer que l'impact n'est pas négligeable
quantile(fln,probs=c(0.05,0.1))
quantile(fB,probs=c(0.05,0.1))


#### 2.3/ Uncertainty in parameter estimates

bfB <- bootdist(fB)
plot(bfB)
# je trouve que c'est une bel exemple pour l'ajustement d'une loi à trois par
# qui montre bien les corrélations entre paramètres

quantile(bfB,probs=c(0.05,0.1))
# utilisation du boostrap pour obtenir l'incertitude sur un quantile

# puis ouverture sur l'utilisation en MC2D via le package mc2d, 
# avec ou sans exemple;à voir peut-être en discutant avec Régis

#### 3/ Advanced topics

#### 3.1/ Alternative methods for parameter estimates

# Maximum goodness-of-fit estimation
flnMGECvM <- fitdist(ATV,"lnorm",method="mge",gof="CvM")
flnMGEAD <- fitdist(ATV,"lnorm",method="mge",gof="AD")
flnMGEADL <- fitdist(ATV,"lnorm",method="mge",gof="ADL")
flnMGEAD2L <- fitdist(ATV,"lnorm",method="mge",gof="AD2L")
x11()
cdfcomp(list(flnMGECvM, flnMGEAD, flnMGEADL, flnMGEAD2L),
        xlogscale = TRUE, main = "GOF estimation with different stat. distances",
        legendtext = c("Cramer-von Mises (CvM)", "Anderson-Darling",
                       "Left-tail Anderson-Darling", "Left tailed Anderson-Darling of second order"),cex=0.7,
        xlegend = "bottomright")
# pour présenter ces méthodes je pense qu'on peut rester sur l'exemple précédent qui me paraît 
# très intéressant car on peut montrer que pour estimer à peu près correctement
# une HC5 sans chercher une loi compliquée, on peut aussi utiliser 
# pour l'ajustement une stat qui donne plus de poids à la queue de 
# distribution qui nous intéresse
quantile(fln,probs=c(0.05,0.1))
quantile(flnMGEAD2L,probs=c(0.05,0.1))
quantile(fB,probs=c(0.05,0.1))

# Moment matching estimation

# Quantile matching estimation

# ???? Aurais-tu un exemple intéressant pour illustrer ces deux méthodes, voire même
# ???? aussi le 3.2, sachant que celui que tu avais mis dans la vignette ne me plaît pas trop
# ???? dansle sens ou la distribution ajustée ajuste clairement mal les données
# ???? on peut continuer avec l'exemple d'écotox mais je ne sais pas s'il sera très illustratif
# ???? pour la suite et ça serat bien aussi que tu mettes un exemple actuariat.
# ???? DONC GRAND BESOIN DE TOI ICI ET POUR LE 3.2

#### 3.2/ Customization of the optimization algorithm

#### 3.3/ Fitting distributions to oter types of data

# censored data

# ???? j'hésite entre un exemple risque alimentaire 
# ???? A/ celui qui était déjà dns la vignette :
# ???? avantage on change de domaine d'application et il a été publié dans ce cadre 
# ???? (intérêt de la prise en compte de la censure lors de l'ajustement d'une loi normale sur ce type de données)
# ???? figure pas très belle
# ???? B/ un autre exemple en écotox : salinity

# ex. A
data(smokedfish)
str(smokedfish)
unique(smokedfish)
# unique() pour illustrer l'ensemble des types de résultats que l'onpeu avoir sur un 
# échantillon de saumon fumé

log10C <- log10(smokedfish)
flog10Cn <- fitdistcens(log10C, "norm")
summary(flog10Cn)
plot(flog10Cn)
# ou 
cdfcompcens(flog10Cn,xlab="bacterial concentration (log10[CFU/g])", 
            ylab="F",legendtext="normal distribution")

# ex. B
data(salinity)
str(salinity)
log10LC50 <-log10(salinity)
fln <- fitdistcens(log10LC50,"norm")
fll <- fitdistcens(log10LC50,"logis")
summary(fll)
summary(fln)
cdfcompcens(list(fln,fll),legendtext=c("normal","logistic"),
            xlab = "log10(LC50)",xlim=c(0.5,2),lines01 = TRUE)



# ouverture sur discrete data sans forcément en faire trop de cas
data(toxocara)
str(toxocara)

fp <- fitdist(toxocara$number, "pois")
fnb <- fitdist(toxocara$number, "nbinom")

summary(fp)
summary(fnb)

x11()
plot(fnb)

# ???? faut-il mettre l'exemple?
# ???? si oui faut-il mettre la figure?



# ???? questions générales : est-ce qu'on reste comme cela sur deux niveaux de
# ???? titre ou est-ce qu'on met un niveau en plus comme on avait fait 
# ???? dans la vignette?

# 4/ Conclusion

