# Exemplo densiddades do solo

d1 <- c(175,200,225,150,125,125)
d2 <- c(75,75,100,75,75,50)
d3 <- c(75,50,75,75,50,75)
trat <- factor(rep(1:3,each=6))
produ <- c(d1,d2,d3)

# Encontrando a estat�stica do teste
# Tomamos a decis�o via regi�o cr�tica
# Tomamos a decis�o via p-valor

nu <- 3*(length(d1)-1)

c <- 1+(1/(3*2))*(3*(1/5)-(1/nu))
q <- nu*log(694.4)-
  5*(log(var(d1))+log(var(d2))+log(var(d3)))
U1 <- q/c
U2 <- qchisq(0.05,2,lower.tail = FALSE)

pvalor <- pchisq(U1,2,lower.tail = FALSE)

# Utilizando o packages ExpDes.pt

require("ExpDes.pt")
saida.DIC <- dic(trat,produ,hvar="bartlett")
plotres(saida.DIC)

# Utilizando a fun��o para o teste Bartlett

bartlett.test(produ~trat)