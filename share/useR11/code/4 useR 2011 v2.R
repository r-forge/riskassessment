library(fitdistrplus)

?mledist
par(mar=c(4,4,2,1))


#------------------------------------------------------------------
x1 <- c(6.4,13.3,4.1,1.3,14.1,10.6,9.9,9.6,15.3,22.1,13.4,
13.2,8.4,6.3,8.9,5.2,10.9,14.4)
x <- seq(0, 1.1*max(x1), length=100)

(f1 <- mledist(x1,"norm"))

dgumbel<-function(x,a,b) 1/b*exp((a-x)/b)*exp(-exp((a-x)/b))

(f2 <- mledist(x1,"gumbel",start=list(a=10,b=5)))

(f3 <- mledist(x1, "exp"))

hist(x1, 10, prob=TRUE)
lines(x, dnorm(x, f1$estimate[1], f1$estimate[1]), col="red")
lines(x, dgumbel(x, f2$estimate[1], f2$estimate[1]), col="green")
lines(x, dexp(x, f3$estimate[1]), col="blue")

legend("topright", lty=1, leg=c("Normal","Gumbel","Exp"), col=c("red","green","blue"))


plotdist(x1)

pgumbel<-function(q,a,b) exp(-exp((a-q)/b))


plot(ecdf(x1))
lines(x, pnorm(x, f1$estimate[1], f1$estimate[2]), col="red")
lines(x, pgumbel(x, f2$estimate[1], f2$estimate[2]), col="green")
lines(x, pexp(x, f3$estimate[1]), col="blue")

legend("bottomright", lty=1, leg=c("Normal","Gumbel","Exp"), col=c("red","green","blue"))


descdist(x1, boot=10, boot.col="blue")


(f4 <- mledist(x1, "gumbel", start=list(b=5), fix.arg=list(a=7) ))

plot(ecdf(x1))
lines(x, pgumbel(x, f2$estimate[1], f2$estimate[2]), col="green")
lines(x, pgumbel(x, 8, f4$estimate[1]), col="red")

legend("bottomright", lty=1, leg=c("Gumbel(a,b)","Gumbel(a,b), a fixed"), col=c("green","red"))


#------------------------------------------------------------------

fit1 <- mledist(x1, "gamma")

fit1bis <- mledist(x1, "gamma", optim.method="BFGS")

#wrap genoud function 
mygenoud <- function(fn, par, ...) 
{
	require(rgenoud)
    	res <- genoud(fn, starting.values=par, ...)        
	standardres <- c(res, convergence=0)
	return(standardres)
}

#call mledist with a 'custom' optimization function
fit2 <- mledist(x1, "gamma", custom.optim=mygenoud, 
	nvars=2, Domains=cbind(c(0,0), c(10, 10)), 
	boundary.enforcement=1, print.level=0, hessian=TRUE)

cbind(NelderMead=fit1$estimate, BFGS=fit1bis$estimate, 
Genoud=fit2$estimate)


#------------------------------------------------------------------

?mmedist

(g1 <- mmedist(x1, "norm"))
cbind(MLE=f1$estimate, MME=g1$estimate)

(g3 <- mmedist(x1, "exp"))

cbind(f3$estimate, g3$estimate)

#empirical raw moment
memp <- function(x, order)
	ifelse(order == 1, mean(x), sum(x^order)/length(x))

#euler constant
euler <- 0.5772156649

#theoretical raw moment
mgumbel <- function(order, a, b)
{
	mean <- a + b*euler
	if(order == 1)
		return(mean)
	else
		return(mean^2 + pi^2*b^2/6)
}

g2 <- mmedist(x1, "gumbel", order=c(1, 2), memp="memp", start=c(10, 5))

cbind(MLE=f2$estimate, MLEfix=c(8, f4$estimate[1]), MME=g2$estimate)

par(mar=c(4,4,2,1))

plot(ecdf(x1), main="Gumbel fit on x1")
lines(x, pgumbel(x, f2$estimate[1], f2$estimate[2]), col="green")
lines(x, pgumbel(x, 8, f4$estimate[1]), col="red")
lines(x, pgumbel(x, g2$estimate[1], g2$estimate[2]), col="blue")

legend("bottomright", lty=1, leg=c("MLE","MLE, fixed a param","MME"), col=c("green","red","blue"))


#------------------------------------------------------------------

(h1 <- qmedist(x1, "norm", prob=c(1/2, 2/3) ))
h1bis <- qmedist(x1, "norm", prob=c(1/3, 2/3) )
cbind(MLE=f1$estimate, MME=g1$estimate, 
QME1=h1$estimate, QME2=h1bis$estimate)

#empirical quantiles from the quantile() function

#theoretical quantiles
qgumbel <- function(p, a, b)
	a - b*log(-log(p))	

h2 <- qmedist(x1, "gumbel", prob=c(1/3, 2/3), start=list(a=10, b=5))
h2bis <- qmedist(x1, "gumbel", prob=c(1/2, 3/4), start=list(a=10, b=5))



cbind(MLE=f2$estimate, MME=g2$estimate, 
QME1=h2$estimate, QME2=h2bis$estimate)



x <- seq(0, 1.26*max(x1), length=100)

par(mar=c(4,4,2,1))

plot(ecdf(x1), main="Gumbel fit on x1", xlim=c(0, 1.25*max(x1)))
lines(x, pgumbel(x, f2$estimate[1], f2$estimate[2]), col="green")
lines(x, pgumbel(x, g2$estimate[1], g2$estimate[2]), col="blue")
lines(x, pgumbel(x, h2$estimate[1], h2$estimate[2]), col="turquoise")
lines(x, pgumbel(x, h2bis$estimate[1], h2bis$estimate[2]), col="red")

legend("bottomright", lty=1, leg=c("MLE","MME","QME, p=1/3;2/3","QME, p=1/2;3/4"), col=c("green","blue","turquoise","red"))



#------------------------------------------------------------------
?mgedist

i1_1 <- mgedist(x1, "norm", "CvM")
i1_2 <- mgedist(x1, "norm", "KS")
i1_3 <- mgedist(x1, "norm", "AD")


cbind(MLE=f1$estimate, CvM= i1_1$estimate, 
KS= i1_2$estimate, AD= i1_3$estimate)


i2_1 <- mgedist(x1, "gumbel", "CvM", start=list(a=10, b=5))
i2_2 <- mgedist(x1, "gumbel", "KS", start=list(a=10, b=5))
i2_3 <- mgedist(x1, "gumbel", "AD", start=list(a=10, b=5))


cbind(MLE=f2$estimate, CvM= i2_1$estimate, 
KS= i2_2$estimate, AD= i2_3$estimate)

par(mar=c(4,4,2,1))

plot(ecdf(x1), main="Gumbel fit on x1", xlim=c(0, 1.25*max(x1)))

# lines(x, pgumbel(x, f2$estimate[1], f2$estimate[2]), col="green", lty="dotted")
# lines(x, pgumbel(x, g2$estimate[1], g2$estimate[2]), col="blue", lty="dotted")
# lines(x, pgumbel(x, h2$estimate[1], h2$estimate[2]), col="red", lty="dotted")

lines(x, pgumbel(x, i2_1$estimate[1], i2_1$estimate[2]), col="green")
lines(x, pgumbel(x, i2_2$estimate[1], i2_2$estimate[2]), col="blue")
lines(x, pgumbel(x, i2_3$estimate[1], i2_3$estimate[2]), col="red")

legend("bottomright", lty=1, leg=c("CvM","KS","AD"), col=c("green","blue","red"))


#------------------------------------------------------------------
data(smokedfish)


log10C <- data.frame(left = log10(smokedfish$left), right = log10(smokedfish$right))

plotdistcens(log10C)

smokedfish <- smokedfish[sample(1:103, 103), ]
rownames(smokedfish) <- 1:103

head(smokedfish, 10)

plotdistcens(smokedfish)
?plotdistcens

mledistcens <- fitdistcens

f1 <- mledistcens(log10C, "norm")
f2 <- mledistcens(log10C, "gumbel", start=list(a=10, b=5))

x <- seq(-3, 3, length=100)

par(mar=c(4,4,2,1))
plotdistcens(log10C)

par(new=TRUE) #add new graph "layers"
plot(range(x), c(0,1), type="n", xlab="", ylab="", axes=FALSE) #redo the x/y limits
lines(x, pnorm(x, f1$estimate[1], f1$estimate[2]), col="blue")
lines(x, pgumbel(x, f2$estimate[1], f2$estimate[2]), col="red")

legend("bottomright", leg=c("Normal", "Gumbel"), lty=1, col=c("red","blue"))

res <- plot(f1)

res$xlog




#------------------------------------------------------------------
f0 <- fitdist(x1, "gamma", method="mle")

summary(f0)

par(mar=c(4,4,2,1))
plot(f0, col="turquoise")

descdist(x1, boot=10, boot.col="turquoise")

?plot.fitdist

