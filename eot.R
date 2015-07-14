#Download the raw data from dryad: doi:10.5061/dryad.42p4q
# Load necessary packages.
require(plyr)
require(lmPerm)
require(ggplot2)        
require(grDevices)
require(lme4)

# set directory where the data is stored
##setwd("C:/Users/Agus/Dropbox/thermal safety/paper files")
setwd("C:/Users/Antonio/Documents/R/Doc/Analysis")
data<-read.csv("temp_WS.csv", header = TRUE, sep = ";", dec = ".")
#data <- read.csv("temperature.csv",header=TRUE)#data from datalogers and Weather stations 
head(data)# WS locality available for WS data only (to see it select data with depth=="WS")
# 10/02/2011 excluded for being ambiguous (clouds and rain mixed with sun) 
data$hour=as.factor(data$hour)


#ilustrating problems of the mean
#########################################################################

###############################################################################
# how the mean miss represents the availability of operative temperatures

## Calculate parameters of operative temperatures for each measurement event.
eventmatrix=ddply(data,.(hour,date,locality),
                  summarise,
                  meanTemp=mean(temperature),
                  minTemp=min(temperature),
                  maxTemp=max(temperature)
)

# Calculate the daily mean OT and the daily maximal range in simultaneously measured OT
dailymatrix=ddply(eventmatrix,.(date,locality),
                  summarise,
                  meanOT=mean(meanTemp),
                  maxerror=max(maxTemp-minTemp)
)

summary(dailymatrix)

# Compare the relationship of mean temperatures with the daily error 
m1=lmer(maxerror~meanOT+(1|locality),data=dailymatrix)
m2=lmer(maxerror~(1|locality),data=dailymatrix)
anova(m1,m2)
summary(m1)

m3=lmer(maxerror~meanOT+(1|localidade),data=dailymatrix2)
m4=lmer(maxerror~(1|localidade),data=dailymatrix2)
anova(m3,m4)
summary(m3)
# test if there is correlation of residuals with index (autocorrelation)
summary(lm(residuals(m1)~seq(1:length(residuals(m1)))))# no autocorrelation
summary(lm(residuals(m3)~seq(1:length(residuals(m3)))))
# visualize potential autocorrelation of residuals
plot(residuals(m1, type = "response"))#
plot(residuals(m3, type = "response"))#
###############################################################
#### do parameters of the central tendency in the data like the mean detects less diferences among local microsites than extreme values?
# excerpt WS data
qdata=data[data$depth!="WS",]
qdata$depth=factor(qdata$depth)

##modfiy lagustin
qdata2=data2[data2$bioma,]
qdata2$bioma=factor(qdata2$bioma)

# F-quantile Regression 

# Sequence that will keep the obtained F values for each quantile
Fvalue=rep( NA, 100)
## sequence of used quantiles
quantile=seq (from= 0.01, to= 1, by = 0.01)
# put them together in a datframe
result=data.frame(Fvalue,quantile)

# for each quantile obtain the F value of a two way anova model fitted by permutation
for (l in quantile)
{
  
  qmatrix=ddply(qdata,.(depth,date,vegetation),
                summarise,
                Temp=quantile(temperature, probs = l, na.rm=TRUE),
                quant=l*100,
                id=("all quantiles")
  )  
  M=summary(lm(pqmatrix$Temp~qmatrix$depth*qmatrix$vegetation))
  
  result[qmatrix$quant[1],]=cbind(c(M$"fstatistic"[1],qmatrix$quant[1])) 
  print(result[qmatrix$quant[1],])
}
head(result)
# re-rank quantiles from the center to the extremes
result$rank=c(seq(49, 0, by=-1),seq(0, 49, by=1))
length(result$rank)

# Summary of the F-quantile rank  regression
summary(lmp(result$Fvalue~result$rank))


### Utility of EOT
##############################################################3
# FIRST EXAMPLE
# How daily maximum and minimum temperatures vary across field microsites and WS

#generate a factor that identifies the different microsites 
data$microsite=as.factor(paste(data$depth, data$vegetation, sep = "_"))
levels(data$microsite)=c("0cm cov" ,  "0cm uncov","10cm cov","10cm uncov","WS")

# generate a matrix of daily minimum temperatures across microsites
minmatrix=ddply(data,.(microsite,date, locality), # generate a new data base to compare minimuntemperatures  microsites among them and with WS
                summarise,
                Temp=min(temperature)
)
head(minmatrix)

# evaluation of locality effects
min1=lmp(Temp~locality, data=minmatrix)
summary(min1)#conclusion: it is necessary to add locality as random factor

# fit a model
min1=lmer(Temp~microsite+(1|locality), data=minmatrix)
min2=lmer(Temp~(1|locality), data=minmatrix)
anova(min1,min2)# significant effect of position of microsite over minimum temperatures

# compare operative temperatures with WS
require (multcomp)
minwht <- glht(min1, mcp(microsite = "Tukey"))
#especify which levels compare, mcp(sfactor = "Tukey") gives all against all

### 95% simultaneous confidence intervals
par(mfrow=c(1,1))
plot(print(confint(minwht)), main="")

### corresponding adjusted p values
summary(minwht)

# repeat with maximum temperatures

maxmatrix=ddply(data,.(microsite, date,locality),
                summarise,
                Temp= max(temperature)
)


# evaluation of locality effects
max1=lmp(Temp~locality, data= maxmatrix)
summary( max1)#conclusion: it is necessary to add locality as random factor

# fit a model
max1=lmer(Temp~microsite+(1|locality), data= maxmatrix)
max2=lmer(Temp~(1|locality), data= maxmatrix)
anova( max1, max2)# significant effect of position of microsite over  maximum temperatures

# compare operative temperatures with WS
require (multcomp)
maxwht <- glht( max1, mcp(microsite = "Tukey"))
#especify which levels compare, mcp(sfactor = "Tukey") gives all against all

### 95% simultaneous confidence intervals
plot(print(confint( maxwht)), main="")

### corresponding adjusted p values
summary( maxwht)

# data to plot maximum and  maximum temperatures
combmatrix=rbind(maxmatrix,minmatrix)
levels(combmatrix$microsite)=c("surface\ncovered" ,  "surface\nuncovered","subsoil\ncovered","subsoil\nuncovered","weather\nstation")
combmatrix$id=c(rep("max",81),rep("min",81))
############################################################################
# Second example
# Relationship between WSdata(TaMAX and TaR) with the minimum, mean,  
# maximum, and the highest minimum available OT across field microsites.


# get EOT per measurement event, keep weather and locality
ldata=data[data$depth!="WS",]
Mmatrix=ddply(ldata,.(hour,date,locality,weather),
              summarise,
              mxOT=max(temperature),
              mnOT=min(temperature)
)

# get daily min, daily max, daily max of minimal and 
# daily  min of maximal temperatures across microsites,
# keep weather and locality
OTmatrix=ddply(Mmatrix,.(date,locality,weather),
               summarise,
               minOT=min(mnOT),
               maxOT=max(mxOT),
               maxminOT=max(mnOT),
               minmaxOT=min(mxOT)
)

#generate ws data based predictors
ldata=data[data$depth=="WS",]

WSpred=ddply(ldata,.(date,locality),
             summarise,
             Tamax=max(temperature),
             TaR=max(temperature)-min(temperature)
)
# merge datasets
mmatrix=merge(WSpred,OTmatrix,by="date")
head(mmatrix)

###evaluating effects of WS parameters on minOT
m1=lmer(minOT~Tamax+TaR+(1|locality.x),REML =F,data=mmatrix)
m2=lmer(minOT~weather+(1|locality.x),REML =F,data=mmatrix)
anova(m1,m2)
# ws factors are better than random factor plus weather

# visualize potential autocorrelation of residuals
plot(residuals(m2, type = "response"),ylab="residuals m1")
# no autocorrelation visible

###evaluating effects of WS parameters on max OT
m1=lmer(maxOT~Tamax+TaR+(1|locality.x),REML =F,data=mmatrix)
m2=lmer(maxOT~weather+(1|locality.x),REML =F,data=mmatrix)
anova(m1,m2)
# ws factors are better than random factor plus weather

# Visualize potential autocorrelation of residuals
plot(residuals(m2, type = "response"),ylab="residuals m1")
# no autocorrelation visible


###evaluating effects of WS parameters on maxmin OT
m1=lmer(maxminOT~Tamax+TaR+(1|locality.x),REML =F,data=mmatrix)
m2=lmer(maxminOT~weather+(1|locality.x),REML =F,data=mmatrix)
anova(m1,m2)
# ws factors are better than random factor plus local weather

# visualize potential autocorrelation of residuals
plot(residuals(m2, type = "response"),ylab="residuals m1")
# no autocorrelation visible

###evaluating effects of WS parameters on maxmin OT
m1=lmer(minmaxOT~Tamax+TaR+(1|locality.x),REML =F,data=mmatrix)
m2=lmer(minmaxOT~weather+(1|locality.x),REML =F,data=mmatrix)
anova(m1,m2)

# ws factors are better than random factor plus weather

# visualize potential autocorrelation of residuals
plot(residuals(m2, type = "response"),ylab="residuals m1")
# no autocorrelation visible
################################################################################333
# Figures

par()              # view current settings
opar <- par()      # make a copy of current settings
par(opar)          # restore original settings



# Figure 2# problems of the mean
require(ggplot2)        
require(grDevices)

par(mfrow=c(2,1))
## panel A
with(dailymatrix,plot(maxerror~meanOT, axes = F,
                      xlab = NA,
                      ylab = NA))
box(col="black",lwd=1.5)
axis(side = 1, tck = -.015, at = seq(24,38,by=2))
axis(side = 2, tck = -.015, las = 1,at = seq(0,max(dailymatrix$maxerror),by=5))
axis(side = 1, lwd = 0, line = -.4,lab=NA)
axis(side = 2, lwd = 0, line = -.4, lab=NA)
mtext(side = 1, "daily meanOT (°C)", line = 2)
mtext(side = 2, "daily maximum error\n in OT (°C)", line = 2)
text(0.6,5,"A")

##repeteco
par(mfrow=c(2,1))
## panel A
with(dailymatrix2,plot(maxerror~meanOT, axes = F,
                      xlab = NA,
                      ylab = NA))
box(col="black",lwd=1.5)
axis(side = 1, tck = -.015, at = seq(24,38,by=2))
axis(side = 2, tck = -.015, las = 1,at = seq(0,max(dailymatrix2$maxerror),by=5))
axis(side = 1, lwd = 0, line = -.4,lab=NA)
axis(side = 2, lwd = 0, line = -.4, lab=NA)
mtext(side = 1, "daily meanOT (°C)", line = 2)
mtext(side = 2, "daily maximum error\n in OT (°C)", line = 2)
text(0.6,5,"A")


## panel B
plot(result$rank,result$Fvalue, axes = F,
     xlab = NA,
     ylab = NA)
box(col="black",lwd=1.5)
axis(side = 1, tck = -.015, labels = NA)
axis(side = 2, tck = -.015, labels = NA)
axis(side = 1, lwd = 0, line = -.4)
axis(side = 2, lwd = 0, line = -.4, las = 1)
mtext(side = 1, "percentile rank", line = 2)
mtext(side = 2, "F value", line = 2)
text(24,27,"B")

#figure 3, 

par(oma=c(0, 0.1, 0.1,0.1))
par(mgp=c(1.5, .5, 0))
par(fig= c(0, 0.5, 0.4, 1))

## panel A: relationships of WS data and different descriptors of OT

# lowest daily available temperature
with(mmatrix,plot(TaR,minOT, ylab="field OT (°C)",
                  xlab= expression(paste("T" ["a"], "r")),
                  ,ylim=c(20,75)))

# highest daily available OT
par(new=TRUE,col="black")
with(mmatrix,plot(TaR,maxOT,ylab="",xlab= "", 
                  ,ylim=c(20,75), 
                  type = 'p', col = 'red', pch=16))

# highest daily minimum available OT
par(new=TRUE,col="black")
with(mmatrix,plot(TaR,maxminOT,ylab="",xlab= "", 
                  ,ylim=c(20,75), 
                  type = 'p', col = 'blue', pch=16))

# highest daily available OT
par(new=TRUE,col="black")
with(mmatrix,plot(TaR,maxOT,ylab="",xlab= "", 
                  ,ylim=c(20,75), 
                  type = 'p', col = 'red', pch=16))

# lowest daily maximum available OT
par(new=TRUE,col="black")
with(mmatrix,plot(TaR,minmaxOT,ylab="",xlab= "", 
                  ,ylim=c(20,75), 
                  type = 'p', col = 'grey', pch=16))

# compare Tamax with the minimum, the mean and the maximum 
# of the daily maximum OT across microsites

#panel B
par(oma=c(0, 0.1, 0.1,0.1))
par(fig= c(0.5, 1, 0.4, 1),new=TRUE)

with(mmatrix,plot(Tamax,minOT, ylab="field OT (ºC)",
                  xlab= expression(paste("T" ["a"], "max")),
                  xlim=c(26,36),ylim=c(20,75),
                  atx=seq(25,75, by=5)))
par(new=TRUE,col="black")
with(mmatrix,plot(Tamax,maxminOT,ylab="",xlab= "", 
                  xlim=c(26,36),ylim=c(20,75), 
                  type = 'p', col = 'blue', pch=16,,
                  atx=seq(25,75, by=5)))
par(new=TRUE,col="black")
with(mmatrix,plot(Tamax,maxOT,ylab="",xlab= "", 
                  xlim=c(26,36),ylim=c(20,75), 
                  type = 'p', col = 'red', pch=16,,
                  atx=seq(25,70, by=5)))
par(new=TRUE,col="black")
with(mmatrix,plot(Tamax,minmaxOT,ylab="",xlab= "", 
                  xlim=c(26,36),ylim=c(20,75), 
                  type = 'p', col = 'grey', pch=16,,
                  atx=seq(25,70, by=5)))

# Panel C
par(oma=c(0.1, 0.1, 0,0.1))
par(mgp=c(2.1, 1, 0))
par(fig= c(0, 1, 0, 0.55),new=TRUE)
with(combmatrix,boxplot(Temp ~ microsite, axes=F,
                        boxwex = 0.25,at = 1:5- 0.2 , cex.lab=1,cex.axis = 1,
                        range=0, #whiskers extend to the data extremes
                        col = "grey",subset = id == "max",
                        xlab = "",
                        ylab = "temperature (ºC)",
                        xlim = c(0.5, 5.8), 
                        ylim = c(15, 75), 
                        yaxs = "i"
))
axis(1, at = 1:5,tck=-.01, labels = levels(combmatrix$microsite))
axis(2, tck=-.05,)
box()
with(combmatrix,boxplot(Temp ~ microsite,  axes=F,add = TRUE,
                        boxwex = 0.25, at = 1:5+0.2 ,range=0,cex.lab=1,cex.axis = 1,
                        subset = id == "min", col = "white"))
axis(1, at = 1:5,tck=-.01, labels = levels(combmatrix$microsite))
axis(2, tck=-.01,)
abline(h=46, col = "red", lwd = 1)
