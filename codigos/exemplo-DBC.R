# Exemplo - Slide

#----------------------------------
rm(list = ls())
#----------------------------------

trat <- rep(1:3,each=4)
trat <- factor(trat)

bloco <- rep(1:4,3)
bloco <- factor(bloco)

resposta <- c(0.05, 0.05, 0.04, 0.15, 0.05, 0.05, 0.04, 0.17, 0.04, 0.04,
              0.03, 0.1)

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

# LETRA A

# H0: T1 = T2 = T3
# H1: Ti != Tj, para todo i != j

# Como o p-valor = 0.003052 < 0.05, temos evidências para rejeitar
# a hipótese nula, portanto, podemos afirmar ao nível de 5% de sig-
# nificância que os tratamentos são diferentes.

#Gr�fico para analisar as pressuposicoes da ANAVA
par(mfrow=c(2,2))
plot(modelo)
par(mfrow=c(1,1))

# No gráfico 1 (Resíduos vs valores ajustados), verificamos que
# as observações possuem média zero e no gráfico do QQ-plot ob-
# servamos que os resíduos parecem ter distribuição Normal.

#Teste de normalidade
shapiro.test(rstandard(modelo))

# Como o p-valor = 0.4123 > 0.05, não temos evidências para re-
# jeitar H0, portanto, podemos afirmar ai nível de 5% de signi-
# ficância que os dados seguem uma distribuição Normal.

#ANOVA
anova(modelo)

# olha para os tratamentos

# como verificamos que os tratamentos sao 

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