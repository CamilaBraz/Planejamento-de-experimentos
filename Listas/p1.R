setwd("~/UFBA/2021-1/Planjemanto de Experimento/Listas")

library(readr)
dados = read_table2("dados.txt")

d <- c(d1,d2,d3)
fator <- rep(c("d1","d2","d3"), each=6)
dados <- data.frame(d,fator)

mod1 <- lm(d~fator,data=dados)
anov <- anova(mod1)

sighat <- c(var(d1),var(d2),var(d3))

vi <- ji-1
v <- sum(vi)

q <- v*log(anov$`Mean Sq`[2])-sum(vi*log(sighat))
c <- 1+(1/(3*(I-1)))*(sum(1/vi)-1/v)

u <- q/c;u
sttest <- qchisq(0.95,I-1);sttest

pvalor <- 1-pchisq(u,I-1)


###########################3



porcentagem <- rep(c("15","20","25","30","35"), each=5)
porcentagem <- factor(porcentagem)

tensao <- c(7,7,15,11,9,12,17,12,18,18,14,18,19,19,18,19,25,22,19,23,7,10,11,15,11)

install.packages("ExpDes.pt")
require(ExpDes.pt)
dados1 <- data.frame(porcentagem,tensao)

dic(dados1$porcentagem, dados1$tensao, quali=FALSE, sigF = 0.05)

anova(lm(porcentagem ~ tensao))
###########################3




d1 = dados %>% filter(`Porcentagemdealgodão(%)`==15)
d2 = dados %>% filter(`Porcentagemdealgodão(%)`==20)
d3 = dados %>% filter(`Porcentagemdealgodão(%)`==25)
d4 = dados %>% filter(`Porcentagemdealgodão(%)`==30)
d5 = dados %>% filter(`Porcentagemdealgodão(%)`==35)

nfact = table(dados$`Porcentagemdealgodão(%)`)
a = length(nfact) 
ni = c(nrow(d1),nrow(d2),nrow(d3),nrow(d4),nrow(d5))

mod1 = lm(`Tensão(lb/pulg2)`~factor(`Porcentagemdealgodão(%)`),data=dados)
anov = anova(mod1)
sighat = c(var(d1$`Tensão(lb/pulg2)`),var(d2$`Tensão(lb/pulg2)`),var(d3$`Tensão(lb/pulg2)`),var(d4$`Tensão(lb/pulg2)`),var(d5$`Tensão(lb/pulg2)`))

vi = ni-1
v = sum(vi)

q = v*log(anov$`Mean Sq`[2])-sum(vi*log(sighat))
c = 1+(1/(3*(a-1)))*(sum(1/vi)-1/v)

u = 2.3026*(q/c);u
sttest = qchisq(0.95,a-1);sttest

pvalor = 1-pchisq(u,a-1)
pvalor
