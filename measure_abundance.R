########## Para carregar a base  ##########
abu <- read.delim(file = 'abu.txt',sep = "", dec = ".", header = TRUE)
# Criando data frame com as medidas de abundância
medidas.abundancia <- data.frame(prop.Pseudoscorpionida = 
                                   abu[,'Pseudoscorpionida']/sum(abu[,'Pseudoscorpionida']),
                                 prop.X0 = abu[,'X0']/sum(abu[,'X0']),
                                 prop.Acari = abu$Acari/sum(abu$Acari),
                                 prop.Araneae = abu$Araneae/sum(abu$Araneae),
                                 prop.Blattodea = abu[,'Blattodea']/sum(abu[,'Blattodea']),
                                 prop.Coleoptera=abu$Coleoptera/sum(abu$Coleoptera),
                                 prop.Collembola=abu$Collembola/sum(abu$Collembola),
                                 prop.Dermaptera=abu$Dermaptera/sum(abu$Dermaptera),
                                 prop.Geophilomorpha=abu$Geophilomorpha/sum(abu$Geophilomorpha),
                                 prop.Hemiptera=abu$Hemiptera/sum(abu$Hemiptera),
                                 prop.Hymenoptera=abu[,'Hymenoptera']/sum(abu$Hymenoptera),
                                 prop.Isopoda=abu$Isopoda/sum(abu$Isopoda),
                                 prop.Isoptera=abu$Isoptera/sum(abu$Isoptera),
                                 prop.Ixodida=abu$Ixodida/sum(abu$Ixodida),
                                 prop.Neuropter=abu$Neuroptera/sum(abu$Neuroptera),
                                 prop.Opiliones=abu$Opiliones/sum(abu$Opiliones),
                                 prop.Orthoptera=abu$Orthoptera/sum(abu$Orthoptera),
                                 prop.Phasmatodea=abu$Phasmatodea/sum(abu$Phasmatodea),
                                 prop.Polydesmida=abu$Polydesmida/sum(abu$Polydesmida),
                                 prop.Stemmiulida=abu$Stemmiulida/sum(abu$Stemmiulida),
                                 prop.Thysanoptera=abu$Thysanoptera/sum(abu$Thysanoptera),
                                 prop.Zygentoma=abu$Zygentoma/sum(abu$Zygentoma),
                                 #prop.Pseudoscorpionida.1=abu$Pseudoscorpionida.1/sum(abu$Pseudoscorpionida.1),
                                 prop.Diplura=abu$Diplura/sum(abu$Diplura),
                                 prop.Diptera=abu$Diptera/sum(abu$Diptera),
                                 prop.Lepidoptera=abu$Embioptera/sum(abu$Embioptera),
                                 prop.Scorpiones=abu$Scorpiones/sum(abu$Scorpiones),
                                 prop.Scutigeromorpha=abu$Stemmiulida/sum(abu$Stemmiulida),
                                 prop.Scolopendromorph=abu$Scolopendromorpha/sum(abu$Scolopendromorpha))
summary(medidas.abundancia)


##sum(abu$Pseudoscorpionida)#
#hist(medidas.abundancia$prop.Pseudoscorpionida) precisa ver o que ta dando pau#
#plot(medidas.abundancia)#

##Calculate Ecololgical Diversity Indices and rarefaction species Richness##
library("vegan")
library("EnvStats")
library("ape")
library("ggplot2")

########## Para carregar a base  ##########
abu <- read.delim(file = 'abu.txt',sep = "", dec = ".", header = TRUE)

##diversity
shadiver<-diversity(abu, index = "shannon", MARGIN = 1, base = exp(1))
simdiver<-diversity(abu, index = "simpson", MARGIN = 1, base = exp(1))

#simplify the diversity index
##data(abu)## Use the current data.frame
H <- diversity(abu)
simp <- diversity(abu, "simpson")
invsimp <- diversity(abu, "inv")
## Unbiased Simpson of Hurlbert 1971 (eq. 5):
unbias.simp <- rarefy(abu, 2) - 1
## Fisher alpha
alpha <- fisher.alpha(abu)
##Plot all index
pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue" )
## Species richness (S) adaptated vegan eg.
S <- specnumber(abu2) ## rowSums(abu > 0) 
# To calculate Pielou's evenness (J)
J <- H/log(S)


#Jaccad index 
#create the graph for input in function

abu.env <- read.delim(file = 'abu_env_biomas.txt',sep = "", dec = ".", header = TRUE)
View(abu.env)
abu.env2<-read.delim(file = 'abu2_env_biomas.txt',sep = "", dec = ".", header = TRUE)
View(abu.env2)

#g<-pair()
#similarity.jaccard(g, vids = V(graph), mode = c("all", "out", "in",
        #                                                   "total"), loops = FALSE)
##### Calculate NMDS###
library("MASS")
library("vegan")
library("permute")
library("lattice")
##load data
data.frame(abu)
myabu<-abu
## dealing with dissimilarity 
abu.dis <- vegdist(abu)
abu.mds0 <- isoMDS(abu.dis)
## Check the function
stressplot(abu.mds0, abu.dis)


##load environmental data set

data.frame(abu.env)
myabu.env<-abu.env
myabu2.env<-abu.env2
####1. Escalonamento Multidimensional Não Métrico ###
# (non-metric multidimensional scaling,NMDS)
# mapeamento das dessemelhanças da comunidade em uma forma não linear sobre o
#"espaço da ordenação" e pode lidar com respostas não-lineares das espécies
abu.mds <- metaMDS(myabu, trace = FALSE)
abu.mds


##Get Species or Site Scores from an Ordination
##Function to access either species or site scores for specified axes in some ordination methods.
abu.pca <- prcomp(abu)
scores(abu.pca, choices=c(1,2,3,4,5,6,7,8))

###### stress, valor de 0 a 1
#1. Function used Bray-Curtis dissimilarities.
#2. Function run isoMDS with several random starts, and stopped either
#   after a certain number of tries, or after nding two similar
#   congurations with minimum stress. In any case, it returned the
#   best solution.
#3. Function rotated the solution so that the largest variance of site
#   scores will be on the first axis.
#4. Function scaled the solution so that one unit corresponds to halving
#   of community similarity from the replicate similarity.
#5. Function found species scores as weighted averages of site scores,
#   but expanded them so that species and site scores have equal variances.
#   This expansion can be undone using shrink = TRUE in display
#   commands.
#6. the test use function isoMDS for NMDS - Non Metric multidimensional scaling
plot(abu.mds, type = "t")

plot(abu$Acari)

#####Fits an Environmental Vector or Factor onto an Ordination#####
###function "envfit",  
#The function fits environmental vectors or factors onto an ordination.
#The projections of points onto vectors have maximum correlation 
#with corresponding environmental variables, and the factors show the averages of factor levels.
# vai encaixa/ajustar vetores ambientais ou fatores para uma ordenação
# resultado com:
# "r" = Goodness of fit (Testes de adequação (aderência))
# e "pvals" = valores de P para cada variável.

ef <- envfit(abu.mds, myabu.env, permu = 999)#error in original myabu.env! 
ef

#try fix error use data of myabu2.env  here the same number in x
ef <- envfit(abu.mds, myabu2.env, permu = 999)
ef

# gráfico com os dados ambientais 
windows(width=5,height=5)
plot(abu.mds,type = "t", display = "sites")
plot(ef, p.max = 0.05) # somente os mais significativas

##SIMPER
(sim <- with(abu.env, simper(abu)))
summary(sim)


##Use the pca for ordenation and scores in substitution of abu.mds
plot(scores, type = "n")

library(Matrix)


