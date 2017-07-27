require(npsurv)
require(fitdistrplus)
#require(interval)
options(digits = 3)
################## CHOIX du JEU DE DONNEES ###########################################
# exemple avec uniquement censures par intervalle
# cf. papier Guolin 2008
# DATA 1 : npsurv ne converge pas vers les mêmes si que survival et interval
d1 <- data.frame(left = c(1, 2, 3, 4, 3, 7), right = c(2, 5, 3, 7, 8, 9))

# exemple avec tous types de censures et censures à gauche mises à très faible valeur
# pour que icfit tourne
# DATA 2 npsurv ne visualise pas les valeurs négatives (il suppose qu'on est sur de la survie)
# mais les calcule - sur cet exemple survival, interval et npsurv sont cohérents
data(smokedfish)
d2 <- as.data.frame(log10(smokedfish))
# minleft  <- min(d2$left[!is.na(d2$left)])
# d2$left[is.na(d2$left)] <- minleft - 1


# exemple avec censures à droite uniquement
# exemple où les algos de Turnbull de survival et du package interval diffèrent un chouilla
# DATA 4 survival, interval et npsurv sont cohérents à un chouilla près
data(fluazinam)
d4 <- as.data.frame(log10(fluazinam))

## Exemple sans censure
# DATA 5
r <- rnorm(20)
d5 <- data.frame(left = r, right = r)


# exemple de Fay (JSS)
# DATA 6 survival, interval et npsurv sont cohérents
#data(bcos)
#d6 <- subset(bcos, treatment == "Rad")[,1:2]
#d6$right[is.infinite(d6$right)] <- NA

# exemple salinity
data(salinity)
d7 <- log10(salinity)

################# FONCTIONS ########################
#### Function to plot the empirical CDF from censored data ###################
CDFplotcens <- function(d, plot = TRUE, add.Turnbull = TRUE)
# if add.Turnbull == TRUE add of the plot provided by survival
# output : vectors with Q and corresponding P values  for GOF stat
{
  db <- d
  db$left[is.na(db$left)] <- -Inf
  db$right[is.na(db$right)] <- Inf
  f <- npsurv(db)$f
  bounds <- c(f$right, f$left)
  finitebounds <- bounds[is.finite(bounds)]
  upper <- max(finitebounds)
  lower <- min(finitebounds)
  width <- upper - lower
  xlimup <- upper + width * 0.2 
  xlimlow <- lower - width * 0.2
  xlimupinf <- upper + width * 100 
  xlimlowinf <- lower - width * 100
  ## mettre la possibilité de rentrer des xlim et ylim par l'utilisateur bien sûr
  k <- length(f$left)
  F <- cumsum(f$p) 
  
  ## calul des points points pour Q et P dnas les GOF stat et graph
  Fbefore <- c(0, F[-k])
  Fmid <- (Fbefore + F)/2
  df <- data.frame(left = f$left, right = f$right, Fmid = Fmid)
  # restriction to equivalence intervals without Inf
  dfb <- subset(df, (left != - Inf) & (right != Inf) ) 
  dfb$xmid <- (dfb$left + dfb$right) / 2
  
  if (plot)
  {
    ### tracé de la ligne à droite des zones indéterminées
    dright <- c(f$left[1], rep(f$right, rep(2,k)), f$right[k]) 
    Fright <- rep(c(0,F), rep(2,k+1))
    plot(dright, Fright, type = "l", xlim = c(xlimlow, xlimup), xlab = "x", ylab = "F")
    ### tracé de la ligne à gauche des zones indéterminées
    dleft = rep(c(f$left,f$right[k]), rep(2,k+1))
    Fleft = c(0,rep(F, rep(2,k)),1)
    lines(dleft, Fleft)
    ### Ajout des rectangles
    Fc <- c(0, F, 1)
    for(i in 1:k) {
      if(f$right[i] - f$left[i] > 0) 
        rect(f$left[i], Fc[i], f$right[i], Fc[i+1], border = "black",
             col = "lightgrey")
    }
    abline(h = 0, lty = 2, col = "grey")
    abline(h = 1, lty = 2, col = "grey")
    # ajout des rectangles à droite et à gauche si besoin
    if(f$right[k] == Inf) rect(f$left[k], F[k - 1], xlimupinf, 1, border = "black",
                               col = "lightgrey")
    if(f$left[1] == - Inf) rect(xlimlowinf, F[1], f$right[1], 0, border = "black",
                                col = "lightgrey")
    
    #### Ajout des points qu'on prendrait en compte pour les GOF graph et stat
    points(dfb$xmid, dfb$Fmid, pch = 16, col = "blue")
    
    #### Ajout de l'ancienne présentation pour voir
    if (add.Turnbull)
    {
      (survdata <- Surv(time = d$left, time2 = d$right, type="interval2"))
      survfitted <- survfit(Surv(left, right, type="interval2") ~ 1, data = d)
      lines(survfitted$time, 1- survfitted$surv, type = "s", col = "red")
    }
  }
  return(list(Qi = dfb$xmid, Pi = dfb$Fmid))
}


#### Function to plot GOF graphs from a fit on censored data ########################
#### It will certainly be difficult to plot more than one fit on the same plot ######
#### as it is proposed for non censored data ########################################
plotfitcens <- function(fti, CDFplot = TRUE, PPplot = TRUE, QQplot = TRUE, add.midpoints = TRUE)
  # output : vectors with Q and corresponding P values  for GOF stat
{
  d <- fti$censdata
  db <- d
  db$left[is.na(db$left)] <- -Inf
  db$right[is.na(db$right)] <- Inf
  f <- npsurv(db)$f
  bounds <- c(f$right, f$left)
  finitebounds <- bounds[is.finite(bounds)]
  upper <- max(finitebounds)
  lower <- min(finitebounds)
  width <- upper - lower
  xlimup <- upper + width * 0.2 
  xlimlow <- lower - width * 0.2
  xmax <- xlimup # pour le plots ensuite
  xmin <- xlimlow
  xlimupinf <- upper + width * 100 
  xlimlowinf <- lower - width * 100
  ## mettre la possibilité de rentrer des xlim et ylim par l'utilisateur bien sûr
  k <- length(f$left)
  F <- cumsum(f$p) 
  
  ## calul des points points pour Q et P dans les GOF stat et graph
  Fbefore <- c(0, F[-k])
  Fmid <- (Fbefore + F)/2
  df <- data.frame(left = f$left, right = f$right, Fmid = Fmid)
  dfb <- subset(df, (left != - Inf) & (right != Inf) ) # without Inf
  dfb$xmid <- (dfb$left + dfb$right) / 2
  
  Qi <- dfb$xmid # dim k if no Inf, k-1 or k-2
  Pi <-  dfb$Fmid

  # Definition of vertices of each rectangle
  Qi.left <- df$left # dim k
  Qi.left4plot <- Qi.left
  if (Qi.left4plot[1] == - Inf) Qi.left4plot[1] <- xlimlowinf
  Qi.right <- df$right
  Qi.right4plot <- Qi.right
  if (Qi.right4plot[k] == Inf) Qi.right4plot[k] <- xlimupinf
  Pi.low <- Fbefore
  Pi.up <- F
  
  # recuperation des parametres ajustees et du nom de la distribution ajustee
  para <- c(as.list(fti$estimate), as.list(fti$fix.arg))
  distname <- fti$distname
  pdistname <- paste("p", distname, sep="")
  qdistname <- paste("q", distname, sep="")
  
  if (CDFplot)
  {
    plot(0, 0, type = "n", xlim = c(xlimlow, xlimup), ylim = c(0,1), xlab = "x", ylab = "F")
    for(i in 1:k) {
        rect(xleft = Qi.left4plot, ybottom = Pi.low, xright = Qi.right4plot, ytop = Pi.up, 
             border = "black", col = "lightgrey")
    }
    abline(h = 0, lty = 2, col = "grey")
    abline(h = 1, lty = 2, col = "grey")

    ### Add of the two lines (only one is need if the rectangles are plotted)
    # the one at right
    dright <- c(f$left[1], rep(f$right, rep(2,k)), f$right[k]) 
    Fright <- rep(c(0,F), rep(2,k+1))
    lines(dright, Fright, type = "l", xlim = c(xlimlow, xlimup), xlab = "x", ylab = "F")
    ### the one at left
    dleft = rep(c(f$left,f$right[k]), rep(2,k+1))
    Fleft = c(0,rep(F, rep(2,k)),1)
    lines(dleft, Fleft)
    
    #### Add of the mid points
    if (add.midpoints) points(Qi, Pi, pch = 16, col = "black")
    
    ### Ajout de la courbe théo ajustée
    sfin <- seq(xmin, xmax, by=(xmax-xmin)/100)
    theopfin <- do.call(pdistname, c(list(q=sfin), as.list(para)))
    lines(sfin, theopfin, col = "red")
  } ############################# END CDFplot
  
  # if (PPplot | QQplot) # dans les b on ne garde que les inf et sup differents
  # {
  #   Pib <- Pi[Qiinf != Qisup]
  #   Qib <- Qi[Qiinf != Qisup]
  #   Qiinfb <- Qiinf[Qiinf != Qisup]
  #   Qisupb <- Qisup[Qiinf != Qisup]
  # }
  if (PPplot)
  {
    plot(0, 0, type = "n", main = "PPplot", xlim = c(0,1), ylim = c(0, 1))
    
    # plot of rectangles
    Pitheo.low <- do.call(pdistname, c(list(q=Qi.left), as.list(para)))
    Pitheo.up <- do.call(pdistname, c(list(q=Qi.right), as.list(para)))
    rect(xleft = Pitheo.low, ybottom = Pi.low, xright = Pitheo.up, ytop = Pi.up, 
           border = "black", col = "lightgrey")
    abline(0,1)
    
    # add of mid moints
    if (add.midpoints)
    {
      Pitheo <- do.call(pdistname, c(list(q = Qi), as.list(para)))
      points(Pitheo, Pi, pch = 16, col = "black")
    }
  } ############# END of PPplot
  
  if (QQplot)
  {
    plot(0, 0, type = "n", main = "QQplot", xlim = c(xmin, xmax), ylim = c(xmin, xmax))

    # plot of rectangles
    Qitheo.left <- do.call(qdistname, c(list(p = Pi.low), as.list(para)))
    Qitheo.right <- do.call(qdistname, c(list(p = Pi.up), as.list(para)))
    Qitheo.left4plot <- Qitheo.left
    if (Qitheo.left4plot[1] == - Inf) Qitheo.left4plot[1] <- xlimlowinf
    Qitheo.right4plot <- Qitheo.right
    if (Qitheo.right4plot[k] == Inf) Qitheo.right4plot[k] <- xlimupinf
    rect(xleft = Qitheo.left4plot, ybottom = Qi.left4plot, xright = Qitheo.right4plot, ytop = Qi.right4plot, 
           border = "black", col = "lightgrey")
    abline(0,1)

    
    # add of mid moints
    if (add.midpoints)
    {
      Qitheo <- do.call(qdistname, c(list(p = Pi), as.list(para)))
      points(Qitheo, Qi, pch = 16, col = "black")
    }
  }
}

######### Essai de la fonction CDFplotcens sur les jeux de données
par(mfrow = c(2,3))
par(mar = c(4, 4, 0.5, 0.5))
listd <- list(d1= d1, d2 = d2, d4 = d4, d5 = d5, d7 = d7)
for (i in 1:length(listd))
{
  CDFplotcens(listd[[i]], add.Turnbull = TRUE)  
}

## essai sur données avec tous types de censures
d <- d2
f <- fitdistcens(d, "norm") 
par(mfrow = c(2,2))
plotfitcens(f)

## essai sur données avec censures à droite 
d <- d4
f <- fitdistcens(d, "norm") 
par(mfrow = c(2,2))
plotfitcens(f)

## essai sur données avec censures à droite sans zone d'indétermination à droite
d <- d7
f <- fitdistcens(d, "norm") 
par(mfrow = c(2,2))
plotfitcens(f)


# Comparatif avec methode classqiue sur un jeu de données non censurées
par(mfrow = c(2, 3))
par(mar = c(4, 4, 0.5, 0.5))
d <- d5
f <- fitdistcens(d, "norm")
plotfitcens(f)

fbis <- fitdist(d$left, "norm")
cdfcomp(fbis)
ppcomp(fbis)
qqcomp(fbis)
