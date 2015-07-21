###Escolher diret?rio
setwd("~/pessoas/Braz/R data") #ctrl+Enter para rodar comando
#verificar diret?rio de trabalho
getwd()
#carregar pacotes
require(picante)
require(ape)
require(geiger)

#leitura de ?rvore
read.nexus("17 Esp?cies.nex")->MRTD
#selecionar as ?rvores
MRTD[[1]]->AllEqual1.0
MRTD[[6]]->Rho0.2

#checar ?rvores
plot(AllEqual1.0)
plot(Rho0.2)

#Abrir dados
read.table("MRTD.TXT",header=TRUE)->dadosMRTD

#Formar um dataframe
data.frame(dadosMRTD[,2:6])->df.dadosMRTD
attach(df.dadosMRTD)

#Associar nomes com cada vari?vel
rownames(df.dadosMRTD)<-dadosMRTD[,1]
rownames(df.dadosMRTD)->names(Massa)
rownames(df.dadosMRTD)->names(RPAE)
rownames(df.dadosMRTD)->names(TPAE)
rownames(df.dadosMRTD)->names(Habito)
rownames(df.dadosMRTD)->names(Habitat)

#checar classes de vari?ves
str(dadosMRTD)

#checar correspondecia entre ?rvores e o dados
name.check(AllEqual1.0,df.dadosMRTD)
name.check(Rho0.2,df.dadosMRTD)

#Sinal Filogen?tico K (Blomberg)
phylosignal(Massa,AllEqual1.0)#primeiro vari?vel, depois ?rvore

#Regress?o convencional
gls(RPAE~Massa+Habito+Habitat)->regconv
#resultado
summary(regconv)
#para checar residuos
plot(regconv)

#regress?o filogen?tica
#construindo a matriz de distancia e covariancia
corBrownian(phy=AllEqual1.0)-> bm.1.0
gls(RPAE~Massa+Habito+Habitat, correlation=bm.1.0, data=df.dadosMRTD)->regfil#modelo, matriz, dados
summary(regfil)

corPagel(0, phy=AllEqual1.0)-> pagel.1.0
gls(RPAE~Massa+Habito+Habitat, correlation=pagel.1.0, data=df.dadosMRTD)->regfil.pagel
summary(regfil.pagel)

#pagel com lambda fixo
corPagel(0, phy=AllEqual1.0, fixed=TRUE)-> pagel.1.0.lambdafix
gls(RPAE~Massa+Habito+Habitat, correlation=pagel.1.0.lambdafix, data=df.dadosMRTD)->regfil.pagel
summary(regfil.pagel)

#PCA
prcomp(~Massa+RPAE+TPAE)->PCA.MRTD
summary(PCA.MRTD)#propor??o cumulativa
predict(PCA.MRTD)#scores
print(PCA.MRTD)#pesos das variaveis
#salvar scores em dataframe
as.data.frame(predict(PCA.MRTD), stringsAsFactors=FALSE)->PCs
list(df.dadosMRTD, PCs)->TOTAL
data.frame(TOTAL)->df.TOTAL

#contrastes independentes
pic(Massa, AllEqual1.0)->cimassa
pic(RPAE, AllEqual1.0)->ciRPAE

#regress?o dos contrastes pela origem (-1)
lm(ciRPAE~cimassa-1)->regconMR
summary(regconMR)
plot(ciRPAE~cimassa)
abline(regconMR)

#PARA LIMPAR WORKSPACE: rm(list=ls())
