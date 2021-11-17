# questao 6

trat = c(3129, 3000, 2865, 2890,
         3200, 3300, 2975, 3150,
         2800, 2900, 2985, 3050,
         2600, 2700, 2600, 2765)
labels = rep(c("A", "B", "C", "D"), each = 4)
labels

dados = data.frame(labels, trat)
ml = lm(trat ~labels)
analise = anova(ml)
res = residuals(ml)

res = MASS::studres(ml)
fit = fitted(ml)
resfit = data.frame(res, fit)
ggplot(resfit, aes(x = fit, y = res)) +
  geom_point() +
  theme_classic() +
  labs(x = "Ajustado", y = "Resíduos")

require(lmtest)
bptest(ml)
# questao 9

densidade = c(21.8, 21.9, 31.7, 21.6, 21.7, 21.5, 21.8,
              21.7, 21.4, 21.5, 21.5,
              21.9, 21.8, 21.8, 21.6, 21.55,
              21.9, 21.7, 21.8, 21.7, 21.6, 21.8)
temp = c(rep(100, 7), rep(125, 4), rep(150, 5), rep(175, 6))
temp = as.factor(temp)

anova(lm(densidade ~ temp))
1 - pf(0.7212, 3, 18)
