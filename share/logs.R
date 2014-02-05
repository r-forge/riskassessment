
#----------------------------------------------------------------------------------------
# download

  # Here's an easy way to get all the URLs in R
  # http://cran-logs.rstudio.com/
  start <- as.Date('2012-10-01')
  today <- as.Date('2014-02-05')

  all_days <- seq(start, today, by = 'day')

  year <- as.POSIXlt(all_days)$year + 1900
  urls <- paste0('http://cran-logs.rstudio.com/', year, '/', all_days, '.csv.gz')

setwd("~/Documents/recherche-enseignement/data/CRANlogs")
# dir.create("CRANlogs")
for(i in 1:length(urls))
	download.file(urls[i], destfile=paste("./CRANlogs/", all_days[i], ".csv.gz", sep=""))


#----------------------------------------------------------------------------------------
# count hits by month

setwd("~/Documents/recherche-enseignement/data/CRANlogs/")


nbdaysbymonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)
names(nbdaysbymonth) <- c("janvier","février", "mars","avril","mai","juin","juillet",
"août","septembre","octobre", "novembre","décembre")
monthfile <- c("1-jan", "2-feb", "3-mar", "4-apr", "5-may", "6-jun", "7-jul", "8-aug", "9-sep", "10-oct", "11-nov", "12-dec")

startdate <- as.Date('2012-10-01')
startdate <- as.Date('2013-12-01')
enddate <- as.Date("2014-01-30") 

while(startdate < enddate)
{
	alldays <- seq(startdate, by = 'day', length= nbdaysbymonth[months(startdate)])
	startdate <- tail(alldays, 1)+1
	logfiles <- paste0(alldays, '.csv.gz')
	
	idata <- NULL
	for(i in logfiles)
	{
	temp <- read.csv(gzfile(i))
	idata <- c(idata, as.character(temp[,"package"]))
	}
	
	
	#----------
	# merge
	
	logcsvfile <- paste(substr(alldays[1], 1, 4), "-", 
	monthfile[names(nbdaysbymonth) == months(alldays[1])], ".csv", sep="")
	print(logcsvfile)
	
	write.csv(data.frame(pkg = names(table(idata)), dwld= as.numeric(table(idata))), file=logcsvfile)

}

tbfiles <- list.files()
tbfiles <- tbfiles[substr(tbfiles, nchar(tbfiles)-2, nchar(tbfiles)) == "csv"]
tbfiles <- tbfiles[substr(tbfiles, 1, 4) != "stat"]
idata <- read.csv(tbfiles[1])[,-1]

for(f in tbfiles[-1])
{
	print(f)
	temp <- read.csv(f)
	idata <- merge(idata, temp[,-1], by="pkg")
}
colnames(idata) <- c("pkg", paste("dwld", 1:length(tbfiles),sep=""))
head(idata)
write.csv(idata, "stat-2012oct-2014jan.csv")


#----------------------------------------------------------------------------------------
# plot

allmonths <- seq(as.Date('2012-10-01'), as.Date('2014-01-30'), by = 'month')

par(mar=c(4,4,2,1))
plot(allmonths, subset(idata, pkg == "fitdistrplus")[-1], ylab="fitdistrplus", type="b", main="monthly downloads from cran-logs.rstudio.com/")
lines(allmonths, subset(idata, pkg == "mc2d")[-1], ylab="mc2d", lty=2)
lines(allmonths, subset(idata, pkg == "distrMod")[-1], ylab="distrMod", lty=3)
legend("bottomright", lty=1:3, leg=c("fitdistrplus", "mc2d", "distrMod"))



plot(allmonths, subset(idata, pkg == "gumbel")[-1], ylab="gumbel", type="b")
plot(allmonths, subset(idata, pkg == "actuar")[-1], ylab="actuar", type="b")
plot(allmonths, subset(idata, pkg == "randtoolbox")[-1], ylab="randtoolbox", type="b")
plot(allmonths, subset(idata, pkg == "rhosp")[-1], ylab="rhosp", type="b")

plot(allmonths, subset(idata, pkg == "Matrix")[-1], ylab="Matrix", type="b")

