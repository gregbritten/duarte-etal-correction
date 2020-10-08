rm(list=ls())
d          <- read.csv('RAMLDB v4.491 (assessment data only)_timeseries_values_views.csv',stringsAsFactors=FALSE)
stocks     <- unique(d$stockid)
nstocks    <- length(stocks)
yrs        <- 1950:2015

par(mfrow=c(1,1),mar=c(2,3,2,3),oma=c(1,1,1,1),cex.axis=0.8)
	plot(-999,xlim=c(1980,2015),ylim=c(0.4,3.0),xlab='',ylab='',bty='n',type='l')
		abline(h=1,lty=3)

##################################################################################################
## USE TOTAL BIOMASS REFERENCE POINTS WHERE AVAILABLE, OTHERWISE APPROXIMATE #####################
##################################################################################################
matbbmsy <- matrix(NA,nrow=2020,ncol=nstocks)
matb     <- matrix(NA,nrow=2020,ncol=nstocks)
matbmsy  <- matrix(NA,nrow=2020,ncol=nstocks)

for(i in 1:nstocks){
	d_tmp   <- d[d$stockid==stocks[i],]
	TB      <- d_tmp$TB
	TBTBMSY <- d_tmp$TBdivTBmsy
	year    <- d_tmp$year
	
if(sum(!is.na(TB))>0){	
	B      <- TB	
	TBMSY  <- mean(TB/TBTBMSY,na.rm=TRUE)
	if(is.finite(TBMSY)){
		BMSY <- TBMSY
		}else{BMSY <- 0.5*max(TB,na.rm=TRUE)}
	
	matbbmsy[year,i] <- B/BMSY
 	matb[year,i]     <- B
	matbmsy[year,i]  <- BMSY
}}
			
w           <- colMeans(matb,na.rm=TRUE)
bbmsy_w=ptb <- numeric()
for(i in yrs){
  row_dat  <- matbbmsy[i,]
  bbmsy_w  <- c(bbmsy_w,weighted.mean(row_dat,w=w,na.rm=TRUE))
  ptb      <- c(ptb,sum(!is.na(row_dat))/nstocks)
}

points(yrs[yrs>1980],bbmsy_w[yrs>1980])
	mtext(side=2,expression('B/B'['MSY']),line=2.5)
par(new=TRUE)
plot(-999,xlim=c(1980,2015),ylim=c(0,1.0),xlab='',ylab='',bty='n',type='l',axes=FALSE)
	points(yrs[yrs>1980],ptb[yrs>1980],pch=5,col=adjustcolor('purple',alpha.f=0.3))
	axis(side=4,col='purple')
	mtext(side=4,'Proportion of Database',line=2)

