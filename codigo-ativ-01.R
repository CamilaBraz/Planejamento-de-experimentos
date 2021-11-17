trat = rep(c("A", "B", "C", "D"), each = 5)

rand_trat = sample(trat, 20, replace = F)
matrix(rand_trat, ncol = 5)

# calculo anova
trat
ganhopeso = c(35, 19, 31, 15, 30, 40, 35, 46, 41, 33, 39, 27, 20, 29, 45,
              27, 12, 13, 28, 30)
anova(lm(ganhopeso ~ trat))

