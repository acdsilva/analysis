########## Para identificar a pasta da biblioteca (antonio olnly) 
read.delim(file = '~/R/Doc/abu.txt',sep = "", dec = ".", header = TRUE)

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
                                 prop.Pseudoscorpionida.1=abu$Pseudoscorpionida.1/sum(abu$Pseudoscorpionida.1),
                                 prop.Diplura=abu$Diplura/sum(abu$Diplura),
                                 prop.Diptera=abu$Diptera/sum(abu$Diptera),
                                 prop.Lepidoptera=abu$Embioptera/sum(abu$Embioptera),
                                 prop.Scorpiones=abu$Scorpiones/sum(abu$Scorpiones),
                                 prop.Scutigeromorpha=abu$Stemmiulida/sum(abu$Stemmiulida),
                                 prop.Scolopendromorph=abu$Scolopendromorpha/sum(abu$Scolopendromorpha))

sum(abu$Pseudoscorpionida)
hist(medidas.abundancia$prop.Pseudoscorpionida)
plot(medidas.abundancia)
