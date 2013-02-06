# Exemples pour le papier JSS
require(fitdistrplus)
require(actuar)
options(digits=3)

#### 1/ intro

#### 2/ Fitting distributions to continuous non-censored data

#### 2.1/ Choice of candidate distributions

data(groundbeef)
str(groundbeef)
plotdist(groundbeef$serving)
descdist(groundbeef$serving, boot=1000)




#### 2.2/ Fit of distributions by MLE and comparison of fits

fw <- fitdist(groundbeef$serving, "weibull")
summary(fw)
fg <- fitdist(groundbeef$serving,"gamma")
fln <- fitdist(groundbeef$serving,"lnorm")
x11()
par(mfrow=c(2, 2))
denscomp(list(fw,fln,fg), legendtext=c("Weibull", "lognormal", "gamma"),
         xlab="serving sizes (g)")
qqcomp(list(fw,fln,fg), legendtext=c("Weibull", "lognormal", "gamma"))
cdfcomp(list(fw,fln,fg), legendtext=c("Weibull", "lognormal", "gamma"))
ppcomp(list(fw,fln,fg), legendtext=c("Weibull", "lognormal", "gamma"))


data(endosulfan)
str(endosulfan)
ATV <-endosulfan$ATV
# plotdist(ATV) 
# pas sûr que celui-là soit très intéressant sur cet exemple
# mais on n'est pas obligé de mettre toutes les figures
# ---
# oui c'est pas hyper intéressant sur cet exemple

# descdist(ATV,boot=1000)
# sur celle-là je commenterais le fait qu'en général les écotoxicologues
# ajustent soit une lognormale soit une loglogistique sur ce type de données
# mais qu'il semble qu'ici la skewness soit plus forte qu'attendue 
# pour une loi [log?]normale, par rapport à la kurtosis
# ---
# oui, en effet pour une loi lognormale la skewness serait plus petite. 
# pour la log-logistique on ne peut pas comparer? il me semblait qu'il y avait
# un graph de C-F avec la loglogistique?


# cet exemple n'est pas forcément très illustratif pour cette partie mais intéressant
# pour la suite
# ???? FAUT-IL démarrer par un exemple plus simple (l'actuel de la vignette)
# ???? et présenter celui-là ensuite uniquement?
# ???? j'attends ton avis là-dessus
# ---
# pour les fonctions utilisés ci dessus, je pense groundbeef est mieux que ATV. 
# car les graphes sont plus démonstratifs et facilement commentable.

fendo.ln <- fitdist(ATV, "lnorm")
# summary(fendo.ln)
# gofstat(fendo.ln)

# use of plotdist to find good reasonable initial values for parameters
# plotdist(ATV, "llogis", para=list(shape=1,scale=500))
fendo.ll <- fitdist(ATV, "llogis", start=list(shape=1,scale=500))

# use of plotdist to find good reasonable initial values for parameters
# plotdist(ATV, "pareto", para=list(shape=1,scale=500))
fendo.P <- fitdist(ATV, "pareto", start=list(shape=1,scale=500))

# definition of the initial values from the fit of the Pareto
# as the Burr distribution is the Pareto when shape2 == 1
fendo.B <- fitdist(ATV, "burr", start=list(shape1=0.3,shape2=1,rate=1))

x11() #pourquoi l'utiliser?
par(mfrow=c(1,2), mar=c(4, 4, 2, 1)) 
#en modifiant les marges on a deja qqch de mieux. il faut aussi jouer sur la taille de la fenetre.

cdfcomp(list(fendo.ln,fendo.ll,fendo.P,fendo.B),xlogscale=TRUE,
          legendtext = c("lognormal","loglogistic","Pareto","Burr"))
qqcomp(list(fendo.ln,fendo.ll,fendo.P,fendo.B),xlogscale=TRUE,ylogscale=TRUE,
       legendtext = c("lognormal","loglogistic","Pareto","Burr"))
# A partir de ce QQplot, on peut bien expliquer le fait que si l'on veut
# utiliser ensuite l'ajustement pour estimer un quantile extrême (ex.
# qui est le cas en écotox sur ce type de données, on veut la HC5 qui protège
# 95% des espèces, donc le quantile à 5%)
# il est mieux d'utiliser la loi de Burr ou la limite celle de Pareto
# ---
# oui
#ppcomp(list(fln,fll,fP,fB),xlogscale=TRUE,ylogscale=TRUE,
#       legendtext = c("lognormal","loglogistic","Pareto","Burr"))

# ci-dessous deux versions de denscomp : du fait qu'on ne peut pas mettre d'argument
# log="x" dans la fonction hist et donc pas non plus dans denscomp qui appelle hist
# je préfère la première version. 
# ???? qu'en penses-tu?
# ???? vois-tu une meilleure solution
# ???? on peut aussi ne pas montrer le denscomp mais c'est un peu dommage
# ???? et on risque de nous le demander non?
# ---
# oui. je ne me souviens plus pourquoi je n'avais pas mis l'option log=x (au moins, log=y parait plus complique)?
# ça vaudrait le coup que j'essaie car les relecteurs vont trouver ça bizarre de ne pas fournir log="x"!

#denscomp(list(fln,fll,fP,fB),
#         legendtext = c("lognormal","loglogistic","Pareto","Burr"),
#         breaks=seq(0,40000,5),xlim=c(0.001,200),ylim=c(0,0.1)) # without great interest as hist does accept argument log="x"
#denscomp(list(fln,fll,fP,fB),
#         legendtext = c("lognormal","loglogistic","Pareto","Burr"),
#        breaks=exp(seq(-3,11,1)),xlim=c(0.001,100),ylim=c(0,0.3)) # without great interest as hist does accept argument log="x"

# ---
#le deuxième est un peu mieux mais ça vaut pas un log=x
         
(HC5 <- quantile(fendo.B,probs = 0.05))

# A remplacer par le print de gofstat
# avec la nouvelle version de fitdistrplus
cbind(
  ln = c(aic=summary(fendo.ln)$aic,bic=summary(fendo.ln)$bic, unlist(gofstat(fendo.ln)[c("cvm","ks","ad")])),
  ll = c(aic=summary(fendo.ll)$aic, bic=summary(fendo.ll)$bic, unlist(gofstat(fendo.ll)[c("cvm","ks","ad")])),
  P = c(aic=summary(fendo.P)$aic, bic=summary(fendo.P)$bic, unlist(gofstat(fendo.P)[c("cvm","ks","ad")])),
  B = c(aic=summary(fendo.B)$aic, bic=summary(fendo.B)$bic, unlist(gofstat(fendo.B)[c("cvm","ks","ad")]))
)


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
# ---
# en fait, vu le premier exemple, le lecteur sait déjà à quoi sert summary 
# et gofstat. du coup, je serais d'avis de stocker les summary$aic et 
# gofstat dans un tableau. on voit que Burr est tjs la meilleure loi.
#
# soit dans R, par ex 
cbind(
LN=c(aic=summary(fln)$aic, unlist(gofstat(fln)[c("cvm","ks","ad")])),
LL=c(aic=summary(fll)$aic, unlist(gofstat(fll)[c("cvm","ks","ad")])),
Pa=c(aic=summary(fP)$aic, unlist(gofstat(fP)[c("cvm","ks","ad")])),
Bu=c(aic=summary(fB)$aic, unlist(gofstat(fB)[c("cvm","ks","ad")]))
)
# soit carrément en latex?
#
# d'ailleurs dans la fonction gofstat, il y a des traces automatiques avec cat.
# ne faudrait-il pas rajouter dans la todolist de faire une classe gofstat.fitdist
# et la fonction générique print?
#
# en termes d'organisation, je pense que les mesures AIC, BIC et les gof stat
# ont l'air leur place dans la section 2.2 Fit of distributions by MLE and comparison of fits
# je ne pense pas qu'il faille séparer en deux cette section, qui reste très cohérente.
# on peut meme rajouter des subsubsection pour avoir 2.2.1 et 2.2.2?
  



# on pourrait terminer par la comparaison des quantiles (HC5 ou HC10)
# estimés par la loi classiquement utilisée (lognormale) et par la loi de Burr
# qui semble la meilleure ici, pour montrer que l'impact n'est pas négligeable
quantile(fln,probs=c(0.05,0.1))
quantile(fB,probs=c(0.05,0.1))


# -- 
# il faudrait rappeler HC5 et HC10 sont des notations de toxicologie.
# en finance/assurance, on appelerait ça VaR 5% et VaR 10%!
# mais oui il est intéresser de comparer des quantiles très faibles
# et aussi très élevés 90% et 95% ?
t(rbind(
LN = quantile(fln,probs=c(0.9,0.95))$quant,
Bu = quantile(fB,probs=c(0.9,0.95))$quant
))


#### 2.3/ Uncertainty in parameter estimates

bfB <- bootdist(fB)
plot(bfB)
# je trouve que c'est une bel exemple pour l'ajustement d'une loi à trois par
# qui montre bien les corrélations entre paramètres
# --
# oui!

quantile(bfB,probs=c(0.05,0.1))
# utilisation du boostrap pour obtenir l'incertitude sur un quantile

# puis ouverture sur l'utilisation en MC2D via le package mc2d, 
# avec ou sans exemple;à voir peut-être en discutant avec Régis
# --
# on peut au moins mentionner mc2d

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

# --
# oui par contre on est plus tot mauvais sur les quantiles élevées. 
# ainsi on enchaine en disant que suivant le type d'application, le choix d'un
# ajustement peut etre radicalement différent.


# Moment matching estimation
data(danishuni)
dloss <- danishuni$Loss
memp  <-  function(x, order) sum(x^order)/length(x)

danishmmelnorm <- fitdist(dloss,"lnorm",method="mme", order=1:2)
danishmlelnorm <- fitdist(dloss,"lnorm",method="mle")

danishmmeburr <- fitdist(dloss,"burr",method="mme", order=1:3, start=list(shape1=5, shape2=5, rate=5), memp=memp, lower=c(2+1e-6,+1e-6,1e-6), upper=c(25, 25, 25))
danishmleburr <- fitdist(dloss,"burr",method="mle", start=list(shape1=5, shape2=5, rate=5), lower=c(1e-6,+1e-6,1e-6), upper=c(25, 25, 25))

danishmmepareto <- fitdist(dloss,"pareto",method="mme", order=1:2, start=list(shape=10, scale=10), memp=memp, lower=c(2+1e-6,0))
danishmlepareto <- fitdist(dloss,"pareto",method="mle", start=list(shape=10, scale=10))



cdfcomp(list(danishmmelnorm, danishmlelnorm, danishmmepareto, danishmlepareto),
        xlogscale = TRUE, main = "GOF estimation with different stat. distances",
        legendtext = c("Log normal - MME", "Log normal - MLE", "Pareto - MME", "Pareto - MLE"),cex=0.7,
        xlegend = "bottomright", ylim=c(.9, 1), xlim=c(5, 250))

# sur cet exemple, que ce soit pour la lognormale ou la pareto, le fit par MME
# est plus conservateur car dans les quantiles élevée, la fonction de répartition
# calibrée F(x) est inférieure à la version empirique jusqu'à x <= 100 à peu près.
# tandis que pour les MLE, c'est vraiment suelement jusqu'à x <= 10 ou 5
# j'ai laissé tomber la burr car elle est trop difficile à calibrer.


# Quantile matching estimation
flndanishQME1 <- fitdist(dloss, "lnorm", method="qme", probs=c(1/3, 2/3))
flndanishQME2 <- fitdist(dloss, "lnorm", method="qme", probs=c(9/10, 9.5/10))
cdfcomp(list(flndanishQME1, flndanishQME2, danishmlelnorm), 
        legend=c("QME(1/3, 2/3)", "QME(9/10, 9.5/10)", "MLE"), 
        main="Fitting lognormal distribution",
        xlogscale=TRUE, datapch="*", ylim=c(.9, 1), xlim=c(5, 250))

# on s'ameliore au fur et a mesure qu'on choisit des probabilités élevées.


danishqmepareto1 <- fitdist(dloss, "pareto", method="qme", probs=c(1/3, 2/3), start=list(shape=10, scale=10))
danishqmepareto2 <- fitdist(dloss, "pareto", method="qme", probs=c(9/10, 9.5/10), start=list(shape=10, scale=10))
cdfcomp(list(danishqmepareto1, danishqmepareto2, danishmlepareto), 
        legend=c("QME(1/3, 2/3)", "QME(9/10, 9.5/10)", "MLE"), 
        main="Fitting lognormal distribution",
        xlogscale=TRUE, datapch="*", ylim=c(.9, 1), xlim=c(5, 250))

# on s'ameliore au fur et a mesure qu'on choisit des probabilités élevées.
# en "bonus", on minor la fonction de répartition empirique, donc on est conservateur.
# l'ajustement est meme meilleur qu'avec MME en ne regardant que les prob élevées.






# ???? Aurais-tu un exemple intéressant pour illustrer ces deux méthodes, voire même
# ???? aussi le 3.2, sachant que celui que tu avais mis dans la vignette ne me plaît pas trop
# ???? dansle sens ou la distribution ajustée ajuste clairement mal les données
# ???? on peut continuer avec l'exemple d'écotox mais je ne sais pas s'il sera très illustratif
# ???? pour la suite et ça serat bien aussi que tu mettes un exemple actuariat.
# ???? DONC GRAND BESOIN DE TOI ICI ET POUR LE 3.2

# --
# danishuni me semble un bon exemple d'application pour mme et qme
# les deux dernières graphes pourraient être mis cote à cote avec un par(mfrow=c(1, 2))


# --
# faut-il faire un exemple avec une loi mélange?





#### 3.2/ Customization of the optimization algorithm
data(groundbeef)
fgroundbeefNM <- fitdist(groundbeef$serving, "gamma", optim.method="Nelder-Mead")
fgroundbeefBFGS <- fitdist(groundbeef$serving, "gamma", optim.method="BFGS") 
fgroundbeefSANN <- fitdist(groundbeef$serving, "gamma", optim.method="SANN")


mygenoud <- function(fn, par, ...) 
{
   require(rgenoud)
   res <- genoud(fn, starting.values=par, ...)        
   standardres <- c(res, convergence=0)
   return(standardres)
}


fgroundbeefgenoud <- mledist(groundbeef$serving, "gamma", custom.optim= mygenoud, nvars=2, 
    max.generations=10, Domains=cbind(c(0,0), c(10,10)), boundary.enforcement=1, 
    hessian=TRUE, print.level=0, P9=10)
cbind(NM=fgroundbeefNM$estimate,
BFGS=fgroundbeefBFGS$estimate,
SANN=fgroundbeefSANN$estimate,
GA=fgroundbeefgenoud$estimate)

# --
# j'ai viré CG car il ne présente pas d'interet et mais très longtemps avec converger!


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

# --
# B est plus joli mais ça reste de l'écotox. t'as pas sous la main un exemple risque alimentaire avec plus d'observations? ce serait parfait


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
# --
# oui

# ???? si oui faut-il mettre la figure?
# --
# pourquoi ne pas mettre?
cdfcomp(list(fp, fnb), legendtext=c("Poisson", "negative binomial"))



# ???? questions générales : est-ce qu'on reste comme cela sur deux niveaux de
# ???? titre ou est-ce qu'on met un niveau en plus comme on avait fait 
# ???? dans la vignette?
# --
# j'ai parcouru qq articles publiés dans le JSS. généralement, il n'y a qu'un niveau de titre.


# 4/ Conclusion

# --
# dans la conclusion actuelle, je liste tous les packages nous citant.
# il suffit de parcourir la biblio pour savoir quelles fonctions utilisent.
# généralement, plotdist, fitdistcens, bootdist





