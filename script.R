##alfadiversidade
library(ggplot2)
library(ggsignif)
library(car)
##carregar matriz de comunidades
comunidades<-read.csv("comunidades.csv",header = FALSE)
colnames(comunidades)=c("tipo_restinga","especies","abundancia")
head(comunidades)

#teste para especies 
teste.shapiro<-shapiro.test(comunidades$especies)
teste.shapiro
leveneTest(especies~tipo_restinga,data = comunidades)
teste.wilcox<-wilcox.test(comunidades$especies~comunidades$tipo_restinga)
teste.wilcox

#teste para abundancia
teste.shapiro_a<-shapiro.test(comunidades$abundancia)
teste.shapiro_a
leveneTest(especies~tipo_restinga,data = comunidades)
teste.wilcox_a<-wilcox.test(comunidades$abundancia~comunidades$tipo_restinga)
teste.wilcox_a
leveneTest(especies~tipo_restinga,data = comunidades)

##plotagem grafica  

#especies
ggplot(comunidades, aes(y = comunidades$especies, x = comunidades$tipo_restinga, fill = tipo_restinga)) +
  labs (x= "Tipo de Restinga", y= "Espécies" )+
  geom_boxplot()+
  geom_signif(comparisons = list(c("aberta", "fechada")),
              map_signif_level=TRUE)+
  scale_fill_manual(values = c("aberta" = "grey70", "fechada" = "grey30")) +
  theme_bw()+theme(legend.position = "none")
#abundancia
ggplot(comunidades, aes(y = comunidades$abundancia, x = comunidades$tipo_restinga, fill = tipo_restinga)) +
  labs (x= "Tipo de Restinga", y= "Abundância" )+
  geom_boxplot()+
  geom_signif(comparisons = list(c("aberta", "fechada")),
              map_signif_level=TRUE)+
  scale_fill_manual(values = c("aberta" = "grey70", "fechada" = "grey30")) +
  theme_bw()+theme(legend.position = "none")



##betadiversidade
library(betapart)
library(vegan)
library(reshape)
##carregar matrizes de restinga aberta e fechada
restinga_fechada<-read.csv("restinga_fechada.csv",header=FALSE)
rownames(restinga_fechada)<-c("p1","p2","p3","p4","p5","p6","p7")
colnames(restinga_fechada)=c("sp_1","sp_2","sp_3","sp_4","sp_5","sp_6","sp_7",
                             "sp_8","sp_9","sp_10","sp_11","sp_12","sp_13","sp_14",
                             "sp_15","sp_16","sp_17","sp_18","sp_19","sp_20","sp_21",
                             "sp_22","sp_23","sp_24","sp_25","sp_26","sp_27","sp_28","sp_29")
restinga_aberta<-read.csv("restinga_aberta.csv",header=FALSE)
rownames(restinga_aberta)<-c("p1","p2","p3","p4","p5","p6","p7")
colnames(restinga_aberta)=c("sp_1","sp_2","sp_3","sp_4","sp_5","sp_6","sp_7",
                             "sp_8","sp_9","sp_10","sp_11","sp_12","sp_13","sp_14",
                             "sp_15","sp_16","sp_17","sp_18","sp_19","sp_20","sp_21",
                             "sp_22","sp_23","sp_24")




##Calcular a betadiversdidade par-a-par entres parcelas

beta.restinga_aberta<-beta.pair.abund(restinga_aberta, index.family = "bray")
beta.restinga_fechada<-beta.pair.abund(restinga_fechada, index.family = "bray")

## reamostragem para 7 parcelas 100 vezes 
beta.restinga_aberta<-beta.sample.abund(restinga_aberta, index.family="bray", sites=7, samples=100)
beta.restinga_fechada<-beta.sample.abund(restinga_fechada, index.family="bray", sites=7, samples=100)
#verificar valor medio de bray 
beta.restinga_aberta
beta.restinga_fechada
# verificar p valor 
p.value.beta.BRAY<-length(which(beta.restinga_aberta$sampled.values$beta.BRAY<
                                  beta.restinga_fechada$sampled.values$beta.BRAY))/100
p.value.beta.BRAY
#para 100 amostrar p<0,01


##plotagem grafica 

plot(density(beta.restinga_aberta$sampled.values$beta.BRAY), 
     col = "grey70", 
     xlim = c(0, 1), 
     lwd=2,
     main = "Beta Diversidade em Restinga Aberta e Restinga Fechada",
     xlab = "Beta Diversidade",
     ylab = "Densidade")
#linha de densidade 
lines(density(beta.restinga_fechada$sampled.values$beta.BRAY), col = "grey30",
      lwd=2)

# adicionar legenda 
legend("right",                
       legend = c("Restinga Aberta", "Restinga Fechada"), 
       col = c("grey70", "grey30"),    
       lty = 1,                  
       cex = 0.8,                 
       box.lty = 0)              

