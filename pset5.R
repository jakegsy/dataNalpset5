#Question 2
#(a)
time <- 0:99
xdata <- sin(2*pi*time/20)
spec <- fft(xdata,inverse=TRUE)/length(xdata)
pspec <- abs(spec)
rspec <- Re(spec)
ispec <- Im(spec)
freq <- (0:99)/100
plot(freq,pspec,"l")
plot(freq,rspec,"l")
plot(freq,ispec,"l")


#(b)
xdata7 <- sin(2*pi*time/7)
spec7 <- fft(xdata7,inverse=TRUE)/length(xdata7)
pspec7 <- abs(spec7)
rspec7 <- Re(spec7)
ispec7 <- Im(spec7)
plot(freq,pspec7,"l")
plot(freq,rspec7,"l")
plot(freq,ispec7,"l")

#(c)
xdatapad <- sin(2*pi*time/20)
xpad<- rep(0.0,1000)
xpad[1:100] <- xdata
specpad <- fft(xpad,inverse=TRUE)/length(xdata)
fpad <- (0:999)/1000
plot(fpad,specpad,"l")
lines(fpad/10,specpad,col="red")


#Question 3
#(a)
xdata <- rnorm(1000,mean=0,sd=1)
spec <- fft(xdata,inverse=TRUE)/length(xdata)
perid <- abs(spec)^2
fpad <- (0:999)/1000
plot(fpad,perid,"l")
mean(perid)

#(b)
xlim = 0.006
bin = 100
hist(perid,xlim=c(0.0,xlim),breaks=bin)
xx <- seq(0,20,by=0.1)
yy <- dchisq(xx,df=2)
factor = 2/mean(perid)
xx <- xx/factor
yy <- yy*factor
yy <- yy*1000
binwidth = bin/xlim
yy <- yy/(binwidth)
lines(xx,yy,col="red")

#(c)
perid_sort = sort(perid)
a95 = perid_sort[950]
a99 = perid_sort[990]
a999 = perid_sort[999]
expect95 = mean(perid) * qchisq(.95,df=2)/2
expect99 = mean(perid) * qchisq(.99,df=2)/2
expect999 = mean(perid) * qchisq(.999,df=2)/2

#From here undocumented
#(d)
pgram3 <- rep(0,998)
pgram3 <- (perid[1:998]+perid[2:999]+perid[3:1000])/3
plot(fpad[2:999],pgram3,"l")

#(d)
xlim = 0.004
bins = 100
hist(pgram3,xlim=c(0.0,xlim),breaks=bins)
xx <- seq(0,20,by=0.1)
yy <- dchisq(xx,df=6)
factor <- 6/mean(pgram3)
xx <- xx/factor
yy <- yy*factor
yy <- yy * length(pgram3)
binwidth = bins/xlim
yy <- yy/binwidth
lines(xx,yy,col="red")


#Problem 4
xdat <- rnorm(1000,mean=0,sd=1)
xdatspec <- fft(xdat,inverse=TRUE)/length(xdat)
xperid <- abs(xdatspec)^2
plot(fpad,xperid,"l")
xlim <- 0.007
xbin <- 100
hist(xdatperid,xlim=c(0.0,xlim),breaks=xbin)
xx <- seq(0,20,by=0.1)
yy <- dchisq(xx,df=2) 

ydat <- rep(0.0,1000)
ydat[2:1000] <- (xdat[2:1000]+xdat[1:999])/2
ydatspec <- fft(ydat,inverse=TRUE)/length(ydat)
yperid <- abs(ydatspec)^2
plot(fpad,yperid,"l")

#(b)
sqrtX <- sqrt(xperid)
sqrtY <- sqrt(yperid)
