#Limpando a memoria
rm(list=ls(all=TRUE))

#carregando a biblioteca laestData
#install.packages("http://leg.ufpr.br/∼walmes/pacotes/labestData_0.1.17.458.zip",repos = NULL)
require(labestData)

dados = PimentelEg7.3
dados

#fatores
#criando o fator 1
fatorMineral = dados$mineral

is.factor(fatorMineral)

#veja que o fator 1 não é do tipo FACTOR

fatorMineral = as.factor(fatorMineral)

is.factor(fatorMineral)

#criando o fator 2
fatorVinhaca = dados$vinhaca

is.factor(fatorVinhaca)

#veja que o fator 2 não é do tipo FACTOR

fatorVinhaca = as.factor(fatorVinhaca)

is.factor(fatorVinhaca)

#bloco
bloco = dados$bloco
bloco

#Resposta
resposta = dados$y
resposta

#Análise Exploratória
require(lattice)
require(latticeExtra)

#Contagem
xtabs(~fatorVinhaca+fatorMineral)

#Fator 1
xyplot(resposta~fatorMineral)

boxplot(resposta~fatorMineral)

#Fator 2
xyplot(resposta~fatorVinhaca)

boxplot(resposta~fatorVinhaca)

#interacao
interaction.plot(fatorMineral,fatorVinhaca,resposta)
interaction.plot(fatorVinhaca,fatorMineral,resposta)

#Usando o ExpDes
require(ExpDes.pt)

analise = fat2.dbc(fator1 = fatorMineral,
                   fator2 = fatorVinhaca,
                   bloco = bloco,
                   resp = resposta,
                   fac.names = c("Mineral","Vinhan�a"))
analise

#plot gráfico dos resíduos
plotres(analise)

#----------------------------------------------------

#Limpando a memoria
rm(list=ls(all=TRUE))

#carregando a biblioteca laestData
#install.packages("http://leg.ufpr.br/∼walmes/pacotes/labestData_0.1.17.458.zip",repos = NULL)
require(labestData)

dados = BarbinPg125
dados

#fatores
#criando o fator 1
fatorN = dados$N

is.factor(fatorN)

#veja que o fator 1 não é do tipo FACTOR

fatorN = as.factor(fatorN)

is.factor(fatorN)

#criando o fator 2
fatorP = dados$P

is.factor(fatorP)

#veja que o fator 2 não é do tipo FACTOR

fatorP = as.factor(fatorP)

is.factor(fatorP)

#criando o fator 3
fatorK = dados$K

is.factor(fatorK)

#veja que o fator 3 não é do tipo FACTOR

fatorK = as.factor(fatorK)

is.factor(fatorK)

#bloco
bloco = dados$bloc
bloco

#Resposta
resposta = dados$prod
resposta

#Análise Exploratória
require(lattice)
require(latticeExtra)

#Contagem
xtabs(~fatorN+fatorP+fatorK)

#Fator 1
xyplot(resposta~fatorN)

boxplot(resposta~fatorN)

#Fator 2
xyplot(resposta~fatorP)

boxplot(resposta~fatorP)

#Fator 3
xyplot(resposta~fatorK)

boxplot(resposta~fatorK)

#interacao
interaction.plot(fatorN,fatorP,resposta)
interaction.plot(fatorP,fatorN,resposta)

interaction.plot(fatorN,fatorK,resposta)
interaction.plot(fatorK,fatorN,resposta)

interaction.plot(fatorP,fatorK,resposta)
interaction.plot(fatorK,fatorP,resposta)

#Usando o ExpDes
require(ExpDes.pt)

analise = fat3.dbc(fator1 = fatorN,
                   fator2 = fatorP,
                   fator3 = fatorK,
                   bloco = bloco,
                   resp = resposta,
                   fac.names = c("N","P","K"))

#Gráfico dos resíduos
plotres(analise)


#----------------------------------------------------
# Sembraram-se duas variedades de batata (V0 e V1) a
# dois densidades (D0,D1) a n�vel de estufa, com a 
# finalidade de identificar o melhor rendimento.
# Considerouse um DIC com 7 repeti��es

#Limpando a memoria
rm(list=ls(all=TRUE))
#

rendimento <- c(50,50,75,75,75,75,50,
                125,50,100,75,100,100,100,
                50,50,100,100,75,75,75,
                50,50,75,50,75,50,50)
variedade <- rep(c("V0","V1"),each=14)
densidade <- rep(rep(c("D0","D1"),each=7),2)


dados <- data.frame(variedade,densidade,rendimento)
str(dados)

dados$variedade <- as.factor(dados$variedade)
dados$densidade <- as.factor(dados$densidade)
str(dados)

require(lattice)
require(latticeExtra)
#Fator 1
xyplot(rendimento~variedade,data=dados)
boxplot(rendimento~variedade,data=dados)

#Fator 2
xyplot(rendimento~densidade,data=dados)
boxplot(rendimento~densidade,data=dados)

#interacao
interaction.plot(variedade,densidade,rendimento)

#Usando o ExpDes
require(ExpDes.pt)

analise = fat2.dic(fator1 = variedade,
                   fator2 = densidade,
                   resp = rendimento,
                   fac.names = c("V","D"))

#Gr�fico dos res�duos
plotres(analise)