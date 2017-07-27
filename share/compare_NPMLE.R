require(fitdistrplus)
require(interval)
require(npsurv)
require(Icens)
options(digits = 3)

# par(mfrow = c(2,2))
# d1 Exemple de la publi de Guolin sur lequel npsurv donne un résultat différent de tous les autres
d1 <- data.frame(left = c(1, 2, 3, 4, 3, 7), right = c(2, 5, 3, 7, 8, 9))
# d3 Exemple où toute les méthodes diffèrent et où on voit bien le problème des masses proches de 0
d3 <- data.frame(left = c(-1.4, 1.18, -1.4, 2, -1.4, 0),
                 right = c(1, 1.18, 2, NA, 0, 2))
# d4 exemple sur lequel toutes les méthodes s'accordent sauf Turnbull de survival et PGM de Icens
data(fluazinam)
d4 <- as.data.frame(log10(fluazinam))
# d5 exemple sans censure sur lequel toutes les méthodes font ce qu'il faut sauf PGM de Icens
r <- rnorm(20)
d5 <- data.frame(left = r, right = r)
# d6 exemple de Fay 2010 sur lequel toutes les méthodes s'accordent sauf Turnbull de survival (avec très petites masses non à 0)
data(bcos)
d6 <- subset(bcos, treatment == "Rad")[,1:2]
d6$right[is.infinite(d6$right)] <- NA
# d7 exemple avec censures à droite
data(salinity)
d7 <- log10(salinity)


listd <- list(d1 = d1,  d3 = d3,  d6 = d6, d7 = d7)
for (i in 1:4)
{
  d <- listd[[i]]
  ## transformation des NA en -Inf ou Inf necessaire pour certains packages
  db <- d 
  db$left[is.na(db$left)] <- -Inf
  db$right[is.na(db$right)] <- Inf
  
  e1 <- VEM(db) # A Vertex-exchange-method in $D$-optimal Design Theory , D. Bohning, Metrika, 1986.
  e1$numiter
  # trial to analyse the difference with d1
  e1bis <- VEM(db, tol = 1e-10, tolbis = 1e-10) # A Vertex-exchange-method in $D$-optimal Design Theory , D. Bohning, Metrika, 1986.
  e1bis$numiter
  
  # plot(e1)
  e2 <- EM(db) # Turnbull 1976
  # plot(e2)
  e3 <- PGM(db) # Some Algorithmic Aspects of the Theory of Optimal Designs, C.-F. Wu, 1978, Annals.
  # plot(e3)
  e4 <- EMICM(db) # A hybrid algorithm for computation of the nonparametric maximum likelihood 
  # estimator from censored data, J. A. Wellner and Y. Zhan, 1997, JASA.
  # plot(e4)
  #e5 <- ISDM(db) # An Algorithm for Computing the Nonparametric MLE of a Mixing Distribution, 
  # Lesperance, Mary L. and Kalbfleisch, John D., JASA, 1992
  # plot(e5)
  # Methode trop lente sur les gros jeux de donnes !!!!!!!!!!!!!!!!!!!!
  
  mass.Icens.VEM <- e1$pf
  mass.Icens.EM <- e2$pf
  mass.Icens.PGM <- e3$pf
  mass.Icens.EMICM <- e4$pf
  #mass.Icens.ISDM <- e5$pf
  
  # pour avoir les bornes des intervalles d'equivalence
  # e1$intmap
  
  # comparatif avec algo de npsurv
  f <- npsurv(db, verb = 0)$f
  mass.npsurv <- f$p
  # pour avoir les bornes des intervalles d'equivalence
  # rbind(f$left, f$right)
  
  # npsurv(db, verb = 0, tol = 1e-10)$numiter
  # npsurv(db, verb = 0, tol = 1e-12)$f
  
  # comparatif avec Turnbull de survival
  survdata <- Surv(time = d$left, time2 = d$right, type="interval2")
  survfitted <- survfit(Surv(left, right, type="interval2") ~ 1, data = d)
  s <- survfitted$surv
  ns <- length(s)
  savant <- c(1, s[-ns])
  mass.survival <- savant - s
  
  # comparatif avec icfit
  fit <- icfit(Surv(left, right, type ="interval2") ~ 1, data = db)
  mass.interval <- fit$pf
  names(mass.interval) <- NULL
  
  
  mass.Icens.VEM
  mass.Icens.EM
  mass.Icens.PGM
  mass.Icens.EMICM
  mass.Icens.ISDM
  mass.survival
  mass.interval
  mass.npsurv
  
  par(mfrow = c(2,2))
  plot(e1, surv = TRUE, main = "")
  points(survfitted$time, survfitted$surv, col = "red", pch = 16)
  lines(survfitted$time, survfitted$surv, type = "s", col = "red", lty = 2)
  legend("bottomleft", legend = "VEM(Icens) en vert et Turnbull(survival) en rouge")
  
  plot(f)
  points(survfitted$time, survfitted$surv, col = "red", pch = 16)
  lines(survfitted$time, survfitted$surv, type = "s", col = "red", lty = 2)
  legend("bottomleft", legend = "npsurv en bleu et Turnbull(survival) en rouge")
  
    plot(fit)
    points(survfitted$time, survfitted$surv, col = "red", pch = 16)
    lines(survfitted$time, survfitted$surv, type = "s", col = "red", lty = 2)
    legend("bottomleft", legend = "interval en gris et turnbull(survival en rouge")
}
