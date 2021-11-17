library(devtools)
require(lmtest)
require(TukeyC)

# Do GitLab.
url <- "https://gitlab.c3sl.ufpr.br/pet-estatistica/labestData.git"
install_git(url = url, branch = "master")

library(labestData)

dados = PimentelEg5.2
View(dados)

## LETRA A

# Competição de Variedades de Batatinha
# 
# DESCRIÇÃO: 
# Experimento de competição de variedades de batatinha feito pelo 
# Engenheiro Agrônomo Oscar A. Garay em Balcare, Argentina. 
# O experimento foi realizado em blocos casualizados.
# 
# FORMATO:
# Um data.frame com 32 observações e 3 variáveis, em que:
# 
# - BLOCO
# Fator de 4 níveis qualitativos, usado para controle local.
# 
# - VARIEDADE
# Fator de 8 níveis qualitativos que são as variedades de batatinha.
# 
# - PRODUÇÃO
# Produção de batatinha, em ton ha^{-1}, nas unidades experimentais.

## LETRA B

# H0: mu1 == mu2 == mu3 == mu4
# H1: mui != muj, para i != j

## LETRA C

# Interpretar os erros tipo I e II.

## LETRA D

# Construa a tabela da ANOVA, interprete e construa a análise.

# variedade = tratamento
# producao = resposta

names(dados) = c("bloco", "trat", "resposta")

summary(dados)

# bloco            trat      resposta    
# I  :8   B 116-51   :4   Min.   : 9.20  
# II :8   B 1-52     :4   1st Qu.:14.90  
# III:8   B 25-50 E  :4   Median :20.55  
# IV :8   B 72-53 A  :4   Mean   :19.71  
# Buena Vista:4   3rd Qu.:24.60  
# Huinkul    :4   Max.   :29.90  
# (Other)    :8             

#Boxplot
boxplot(dados$resposta~dados$trat)
boxplot(dados$resposta~dados$bloco)

########################################################
#Ajustando o modelo
modelo = lm(resposta~trat+bloco, data = dados)
modelo

# Call:
#   lm(formula = resposta ~ trat + bloco, data = dados)
# 
# Coefficients:
#   (Intercept)       tratB 1-52    tratB 25-50 E  
# 20.550           -0.225           -6.000  
# tratB 72-53 A  tratBuena Vista      tratHuinkul  
# 0.300          -10.075            2.550  
# tratKennebec  tratS. Rafalela          blocoII  
# -11.800            2.950            3.500  
# blocoIII          blocoIV  
# 2.275            2.025 

#Summary do modelo
summary(modelo)

anova(modelo)

# Analysis of Variance Table
# 
# Response: resposta
# Df Sum Sq Mean Sq F value    Pr(>F)    
# trat       7 919.72 131.389 15.3744 5.723e-07 ***
#   bloco      3  50.53  16.843  1.9709    0.1493    
# Residuals 21 179.46   8.546                      
# ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# LETRA E

#Teste de Tukey

modelo.aov = aov(resposta~trat+bloco, data = dados)
TukeyC(modelo.aov)
plot(TukeyC(modelo.aov))
# Results
# Means G1 G2 G3
# S. Rafalela 25.45  a      
# Huinkul     25.05  a      
# B 72-53 A   22.80  a  b   
# B 116-51    22.50  a  b   
# B 1-52      22.27  a  b   
# B 25-50 E   16.50     b  c
# Buena Vista 12.42        c
# Kennebec    10.70        c
# 
# Sig.level
# 0.05
# 
# Diff_Prob
# S. Rafalela Huinkul B 72-53 A B 116-51
# S. Rafalela       0.000   0.400     2.650    2.950
# Huinkul           1.000   0.000     2.250    2.550
# B 72-53 A         0.896   0.952     0.000    0.300
# B 116-51          0.835   0.912     1.000    0.000
# B 1-52            0.780   0.872     1.000    1.000
# B 25-50 E         0.006   0.009     0.093    0.122
# Buena Vista       0.000   0.000     0.001    0.002
# Kennebec          0.000   0.000     0.000    0.000
# B 1-52 B 25-50 E Buena Vista Kennebec
# S. Rafalela  3.175     8.950      13.025   14.750
# Huinkul      2.775     8.550      12.625   14.350
# B 72-53 A    0.525     6.300      10.375   12.100
# B 116-51     0.225     6.000      10.075   11.800
# B 1-52       0.000     5.775       9.850   11.575
# B 25-50 E    0.150     0.000       4.075    5.800
# Buena Vista  0.002     0.522       0.000    1.725
# Kennebec     0.000     0.146       0.989    0.000
# 
# MSD
# S. Rafalela Huinkul B 72-53 A B 116-51
# S. Rafalela       0.000   6.933     6.933    6.933
# Huinkul           6.933   0.000     6.933    6.933
# B 72-53 A         6.933   6.933     0.000    6.933
# B 116-51          6.933   6.933     6.933    0.000
# B 1-52            6.933   6.933     6.933    6.933
# B 25-50 E         6.933   6.933     6.933    6.933
# Buena Vista       6.933   6.933     6.933    6.933
# Kennebec          6.933   6.933     6.933    6.933
# B 1-52 B 25-50 E Buena Vista Kennebec
# S. Rafalela  6.933     6.933       6.933    6.933
# Huinkul      6.933     6.933       6.933    6.933
# B 72-53 A    6.933     6.933       6.933    6.933
# B 116-51     6.933     6.933       6.933    6.933
# B 1-52       0.000     6.933       6.933    6.933
# B 25-50 E    6.933     0.000       6.933    6.933
# Buena Vista  6.933     6.933       0.000    6.933
# Kennebec     6.933     6.933       6.933    0.000

plot(TukeyC(modelo.aov))

## LETRA F

shapiro.test(rstandard(modelo))

# Shapiro-Wilk normality test
# 
# data:  rstandard(modelo)
# W = 0.96895, p-value = 0.471

# variancia
bptest(modelo)

# studentized Breusch-Pagan test
# 
# data:  modelo
# BP = 11.955, df = 10, p-value = 0.2881


