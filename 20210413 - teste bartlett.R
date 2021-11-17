d = c(150, 125, 125, 175, 200, 225, 75, 75, 100, 75, 75, 500, 
      75, 75, 75, 75, 50, 50)
trat = rep(c("d1", "d2", "d3"), each = 6)
anova(lm(d ~ trat))

# Df Sum Sq Mean Sq F value Pr(>F)
# trat       2  34444   17222  1.6489 0.2253
# Residuals 15 156667   10444

require(lmtest)
m1 = lm(d ~trat)
bptest(m1)

res = residuals(m1)

###### teste bartlett ########
sigma_hat =  10444/(3*(6-1))


J1 = c(150, 125, 125, 175, 200, 225)
J2 = c(75, 75, 100, 75, 75, 500)
J3 = c(75, 75, 75, 75, 50, 50)
J = list(J1, J2, J3)
I = 3
j = 6

#nu = for(i in 1:3){sum(J[i] - 1)}
nu = c( (length(J1) - 1), ((length(J2) - 1)),
        ((length(J3) - 1)))
NU = sum(length(J1) + length(J2) + length(J3))

VARi = c(var(J1), var(J2), var(J3))

soma = sum(nu*log(VARi[1]), nu*log(VARi[2]), nu*log(VARi[3]))

q = NU*log(sigma_hat) - soma
c = 1 + (1/(3*(j-1)))*sum(1/nu - 1/NU)
u = q/c
u
