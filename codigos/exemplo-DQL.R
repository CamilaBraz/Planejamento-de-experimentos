require(devtools)
url <- "https://gitlab.c3sl.ufpr.br/pet-estatistica/labestData.git"
devtools::install_git(url=url, branch = "master")

#Limpando a memória
rm(list = ls())

#Carregandos os dados do pacote labestData
require(labestData)

dados = PimentelEg6.2

#Veriticando a estrutura do arquivo de dados importado
str(dados)

#Estatística Descritiva
summary(dados)

#
attach(dados)

#Nome das colunas do arquivo importado
names(dados)

#Boxplot
boxplot(prod~varied)
boxplot(prod~linha)
boxplot(prod~coluna)

#Forma de analisar os dados
require(ExpDes.pt)

#Usando o teste de Tukey
modelo = dql(varied,linha,coluna,prod)

#Analisando os resíduos
plotres(modelo)

###############################################################

#Limpando a memória
rm(list = ls())

#Carregandos os dados do pacote labestData
require(labestData)

dados = PimentelEx6.6.3
dados

#Veriticando a estrutura do arquivo de dados importado
str(dados)

#Estatística Descritiva
summary(dados)

#
attach(dados)

#Nome das colunas do arquivo importado
names(dados)

#Boxplot
boxplot(peso~castracao)
boxplot(peso~leitegada)
boxplot(peso~coluna)

#Analisando o conjunto de dados
require(ExpDes.pt)

#Usando o teste de Tukey
modelo = dql(castracao,leitegada,coluna,peso)

#Analisando os resíduos
plotres(modelo)
