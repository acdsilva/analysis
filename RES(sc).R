setwd("C:/Users/Zuza/Dropbox/M+D/Análises com variáveis ambientais/Todas e separados")
setwd("C:/Users/Owner/Dropbox/M+D/Análises com variáveis ambientais/Todas e separados/Teste")

require(ape)
require(nlme)

read.nexus("REWL(sc).nex")->MRR
MRR[[1]]->rewl

plot(rewl)

read.table("RESlog(sc).txt",header=TRUE)->dados
data.frame(dados[,2:5])->df.dados
attach(df.dados)
rownames(df.dados)<-dados[,1]
rownames(df.dados)->names(MAS)
rownames(df.dados)->names(RES)
rownames(df.dados)->names(C1)
rownames(df.dados)->names(C2)
rownames(df.dados)->ESP
str(df.dados)

# Tirando o efeito da massa
C.rewl<-vcv.phylo(rewl)

x<-as.matrix(MAS)
Y<-as.matrix(RES)
phyl_resid<-function(C,x,Y)
  {
  m<-ncol(Y); n<-nrow(Y)
  invC<-solve(C)
  r<-matrix(,n,m)
  X<-matrix(,n,2)
  X[,1]<-1; X[,2]<-x
  for(i in 1:m){
  beta<-solve(t(X)%*%invC%*%X)%*%(t(X)%*%invC%*%Y[,i])
  r[,i]<-Y[,i]-X%*%beta
  }
  phyl_resid<-r
}
r.rewl<-phyl_resid(C.rewl,x,Y)

RESc.rewl<-as.vector(r.rewl)
names(RESc.rewl)<-ESP
RESc.rewl
df.dados<-cbind(RESc.rewl,df.dados)

#Regressão filogenética
corPagel(value=1, phy=rewl, fixed=FALSE)->pagel.rewl
m01<-gls(RESc.rewl~C1+C2,correlation=pagel.rewl,method="ML",data=df.dados)

summary(m01)

fit<-data.frame(m01$fitted)
fit

#Regressão para mostrar o efeito da massa filogenéticamente
m02<-gls(RES~MAS,correlation=pagel.rewl,method="REML",data=df.dados)
summary(m02)