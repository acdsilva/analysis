## Analise de CTmax

dat<-read.delim(file = "CTmax.txt", dec = ".", sep = "", header=TRUE)
plot(tcmax_final,Classe)
qplot(tcmax_final,Classe)

ggplot(dat, aes(x=tcmax_final, y=reorder(Classe, tcmax_final))) +
  geom_point(size=3) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(dat, aes(x=tcmax_final, y=reorder(Ordem, tcmax_final ))) +
  geom_point(size=3) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(dat, aes(x=tcmax_final, y=reorder(Famila, tcmax_final ))) +
  geom_point(size=3) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(dat, aes(x=tcmax_final, y=reorder(Genero, tcmax_final ))) +
  geom_point(size=3) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(dat, aes(x=Latitude, y=reorder(tcmax_final, Latitude ))) +
  geom_point(size=3) +  # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))
#facet_grid(.~Latitude,labeller = label_value)

ggplot(dat, aes(x=Altitude, y=reorder(tcmax_final, Latitude ))) +
  geom_point(size=3) +  # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey59", linetype="dashed"))

##Learning to use command qplot()
dat<-CTmax# manipulando a tabela, teve bug de importação assim funciona.
dat
qplot(tcmax_final, Altitude,data = dat)
qplot(tcmax_final,Ordem, data = dat)
qplot(Localizacao,tcmax_final, data = dat)
qplot(tcmax_final,Localizacao, data = dat, alpha = I(1/10), color= I("red")) 
qplot(tcmax_final, data = dat, stat = "bin", geom = "density") 
qplot(tcmax_final,Localizacao, data = dat, stat ="identity")
library(mgcv)
qplot(Localizacao,tcmax_final, data = dat, geom = c("point", "smooth"), method = "gam", formula = y ~ s(x))


