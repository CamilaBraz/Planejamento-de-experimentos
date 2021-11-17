library(ggplot2)
library(dplyr)

ctrat = c(5.3, 4.03, 4.03, 4, 2.56, 2.05, 5.06, 4.06, 2.08, 4.03, 2.04, 1.18)
strat = c(8.02, 13.18, 7.15, 8.23, 9.11, 6.66, 12.15, 16.3, 9.2, 6.35, 7.15, 8.66)
lotes = data.frame(ctrat, strat)

boxplot(ctrat, strat, names = c("Com tratamento", "Sem tratamento"),
        main = "Boxplot dos tratamentos por grupo")

fBasics::basicStats(ctrat); fBasics::basicStats(strat)


anova(ctrat ~ strat, data = lotes)

#H0: as médias dos lotes com tratamento e sem tratamento são iguais
#H0: a proporção média dos discos que apresentaram cometas é igual
# nos lotes com tratamento e sem tratamento
#H0: mu0 = mu1

#H1: as médias dos lotes com tratamento e sem tratamento são diferentes
#H1: mu0 dif mu1

require(fBasics)

ctrat <- c(5.30, 4.03, 4.03, 4, 2.56, 2.05, 5.06, 4.06, 2.08, 4.03, 2.04, 1.18)
strat <- c(8.02, 13.18, 7.15, 8.23, 9.11, 6.66, 12.15, 16.3, 9.2, 6.35, 7.15, 8.66)
lotes <- data.frame(ctrat,strat)

boxplot(ctrat, strat, names = c("Com Tratamento", "Sem tratamento"),
        main = "Boxplot dos tratamentos por grupo")

cvstrat <- 100*(sd(strat)/mean(strat)); cvctrat <- 100*(sd(ctrat)/mean(ctrat))
cvstrat; cvctrat

basicStats(ctrat); basicStats(strat)

t.test(lotes)
install.packges("lmtest")
require(lmtest)
tratamentos <- rep(c("Com Tratamento", "Sem Tratamento"), each=12)
tratamentos <- factor(tratamentos)
resposta <- c(ctrat,strat)
modelo1 <- lm(resposta~tratamentos)
anova(modelo1)

residuos <- residuals(modelo1)
res_jack <- rstandard(modelo1)

bptest(modelo1)
shapiro.test(res_jack)

qqnorm(res_jack); qqline(res_jack, col=5, ylab="Quantis amostrais", 
                         xlab="Quantis teóricos")
plot(res_jack)
#H1: a proporção média dos discos que apresentaram cometas é diferente
# nos lotes com tratamento e sem tratamento