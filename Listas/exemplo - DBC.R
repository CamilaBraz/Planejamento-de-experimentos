# Exemplo - Slide

#----------------------------------
rm(list = ls())
#----------------------------------

trat <- rep(1:9,each=3)
trat <- factor(trat)

bloco <- rep(1:3,9)
bloco <- factor(bloco)

resposta <- c(145,155,166,200,190,190,183,186,208,190,175,186,
              180,160,156,130,160,130,206,165,170,250,271,230,
              164,190,193)

dados <- data.frame(bloco,trat,resposta)
head(dados)
str(dados)

summary(dados)

#Boxplot
boxplot(resposta~trat)
boxplot(resposta~bloco)

########################################################
#Ajustando o modelo
modelo = lm(resposta~trat+bloco)
modelo

#Summary do modelo
summary(modelo)

#Gr�fico para analisar as pressuposicoes da ANAVA
par(mfrow=c(2,2))
plot(modelo)
par(mfrow=c(1,1))

#Teste de normalidade
shapiro.test(rstandard(modelo)) 

#ANAVA
anova(modelo)

#Teste de Tukey
require(TukeyC)
modelo.aov = aov(resposta~trat+bloco)
TukeyC(modelo.aov)

plot(TukeyC(modelo.aov))

#########################################################

require(ExpDes.pt)

#Usando o teste de Tukey
dbc(trat,bloco,resposta)

#----------------------------------
rm(list = ls())
#----------------------------------

# Um artigo no peri�dico American Industrial  
# Hygiene Association Journal (Vol. 37, 1976, pp. 418-422) 
# descreveu um teste de campo para detectar a presen�a de 
# ars�nio em amostras de urina. 

ind <- rep(1:4, 3)
ind <- factor(ind)
teste <- rep(c("Estagiario","Analista","Laboratorio"),each=4)
teste <- factor(teste)
y <- c(0.05,0.05,0.04,0.15,0.05,0.05,0.04,0.17,0.04,0.04,0.03,0.10)

dados <- data.frame(ind,teste,y)
head(dados)
str(dados)

summary(dados)

#Boxplot
boxplot(y~teste)
boxplot(y~ind)

########################################################
#Ajustando o modelo
modelo = lm(y~teste+ind)
modelo

#Summary do modelo
summary(modelo)

#Gr�fico para analisar as pressuposicoes da ANAVA
par(mfrow=c(2,2))
plot(modelo)
par(mfrow=c(1,1))

#Teste de normalidade
shapiro.test(rstandard(modelo)) 

#ANAVA
anova(modelo)

#Teste de Tukey
require(TukeyC)
modelo.aov = aov(y~teste+ind)
TukeyC(modelo.aov)

plot(TukeyC(modelo.aov))

#########################################################

require(ExpDes.pt)

#Usando o teste de Tukey
dbc(teste,ind,y)