rm(list = ls())
trat = rep(c("A", "B", "C", "D"), each = 5)
trat = factor(trat)
produtividade = c(25, 26, 20, 23, 21, 31, 25, 28, 27, 24, 22, 26, 28,
                  25, 29, 33, 29, 31, 34, 28)

dados = data.frame(produtividade, trat)
head(dados)

model.aov = aov(produtividade ~ trat, data = dados)
anova(model.aov)

c1 = c(1, 0, 0, -1)
c2 = c(1, 1, -1, -1)

mat = cbind(c1, c2)

contrasts = (dados$trat) = mat

model.contra = aov(produtividade ~trat, data = dados)

summary(model.contra)
summary(model.contra, split = list(trat=list(1, 2)))

# exemplo 3

rm(list = ls())

resistencia = c(7, 7, 15, 11, 9, 12, 17, 12, 18, 18, 14, 18, 18,
                19, 19, 19, 25, 22, 19, 23, 7, 10, 11, 15, 11)
palgodao = factor(rep(c(15, 20, 25, 30, 25), each = 5))

dados = data.frame(resistencia = resistencia, palgodao = palgodao)
head(dados)

modelo.aov = aov(resistencia ~ palgodao, data = dados)
anova(modelo.aov)

c1 = c(0, 0, 0, 1, -1)
c2 = c(1, 0, 1, -1, -1)
c3 = c(1, 0, -1, 0, 0)
c4 = c(-1, 4, -1, -1)
mat = cbind(c1, c2, c3, c4)

contrasts(dados$palgodao) = mat

model.contra = aov(resistencia ~ palgodao, data = dados)

summary(model.contra)
summary(model.contra, split = list(palgodao = list(2, 2, 3, 4)))
