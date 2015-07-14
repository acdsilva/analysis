#Modificado de Camanho et.al., 2015
##Pacotes requiridos
require(plyr)
require(lmPerm)
require(ggplot2)        
require(grDevices)
require(lme4)

#Para mudaro diretorio de trabalho setwd("C:/Users/Antonio/Documents/R/Doc/analysis")
data<-read.csv("temp_WS.csv", header = TRUE, sep = ";", dec = ".")
data2<-read.csv("tese.csv", header = TRUE, sep = ";", dec = ".")
head(data)
head(data2)
data$hour=as.factor(data$hour)
data2$bioma=as.factor(data2$bioma)
data2$estacao=as.factor(data2$estacao)
###############################################################################
# how the mean miss represents the availability of operative temperatures

## Calculate parameters of environmental temperatures for each measurement event at biome or WS.
#adicionei a estatistica da moda, mediana e desvio padrao nao coloquei nenhum aviso para casos de NA
eventmatrix=ddply(data,.(hour,date,locality),
                  summarise,
                  meanTemp=mean(temperature),
                  minTemp=min(temperature),
                  maxTemp=max(temperature),
                  medianTemp=median(temperature),
                  sdTemp=sd(temperature)
                  
)
##fazendo algumas alterações no script do Agustin
# Calculate the daily mean OT and the daily maximal range in simultaneously measured OT
ctmatrix=ddply(data2,.(bioma,localidade,ordem,alt,tcmax),
                  summarise,
                  meanCT=mean(tcmax),
                  minCT=min(tcmax),
                  maxCT=max(tcmax),
                  varmaxCT=sd(tcmax),
                  minsize=min(peso),
                  maxsize=max(peso),
                  resbodysize=resid(peso),
                  bodysize=sqrt(maxsize-minsize)+1,
                  meanTE=mean(tempSOL),
                  minTE=min(tempSOL),
                  maxTE=max(tempSOL),
                  maxerrorT=max(maxTE-minTE)
               
)

summary(ctmatrix)
write("summary.txt")#try to save results
sink("summary.txt")
#capture.output(summary, "ctmatrix.txt")

#################################################
## x[2:4,1:3] # rows 2,3,4 of columns 1,2,3 para capturar coisas especificas da matrix
### para Matrix
#d <- c(1,2,3,4)
#e <- c("red", "white", "red", NA)
#f <- c(TRUE,TRUE,TRUE,FALSE)
#mydata <- data.frame(d,e,f)
#names(mydata) <- c("ID","Color","Passed") # variable names
#####There are a variety of ways to identify the elements of a data frame .
#myframe[3:5] # columns 3,4,5 of data frame
#myframe[c("ID","Age")] # columns ID and Age from data frame
#myframe$X1 # variable x1 in the data frame
############################################################

# Compare the relationship of mean temperatures with the daily error 
m1=lmer(maxerror~meanOT+(1|locality),data=dailymatrix)
m2=lmer(maxerror~(1|locality),data=dailymatrix)
anova(m1,m2)
summary(m1)

#######fazendo algumas alterações no script do Agustin
# Compare the relationship of mean temperatures critical maximum with the  maxerror 
m3=lmer(bodysize~tcmax +(1|localidade),data=ctmatrix)
m4=lmer(bodysize~(1|localidade),data=ctmatrix)
anova(m3,m4)
summary(m3)

#viagem toninho...
m3=lmer(tcmax~bodysize:ordem +(1|localidade),data=ctmatrix)
m4=lmer(tcmax~(1|localidade),data=ctmatrix)


### tentando outras possibilidades regressao linear
lm1<-lm(tcmax~bodysize,data=ctmatrix)
lm2<-lm(tcmax~bodysize:ordem,data=ctmatrix)
lm3<-lm(tcmax~bodysize:bioma,data=ctmatrix)
summary(lm1)
summary(lm2)
summary(lm3)
plot(lm1)
plot(lm2)
plot(lm3)

## repetindo um gráfico de Camachoet al 2015. Observar como seus dados se comportam
a=ctmatrix$tcmax
b=ctmatrix$bodysize
plot(b,a, xlab = "Peso(mg)", ylab = "Temperatura Crítica Máxima (°C)")

# test if there is correlation of residuals with index (autocorrelation)
summary(lm(residuals(m3)~seq(1:length(residuals(m3)))))# no autocorrelation
summary(lm(residuals(m4)~seq(1:length(residuals(m4)))))
# visualize potential autocorrelation of residuals
plot(residuals(m3, type = "response"))#
plot(residuals(m4, type = "response"))#


##basic adjust
length(data2$tcmax)

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


