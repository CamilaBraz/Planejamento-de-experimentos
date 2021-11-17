# Exemplo 4.3 Montgomery

#Limpando a memória
rm(list = ls())

linha <- factor(rep(1:5,5))
coluna <- factor(rep(1:5,each=5))
trat <- factor(c("A","B","C","D","E","B","C","D","E","A","C",
                 "D","E","A","B","D","E","A","B","C","E","A","B","C","D"))
greco <- factor(c("a","b","c","d","e",  "c","d","e","a","b",
                  "e","a","b","c","d",  "b","c","d","e","a",
                  "d","e","a","b","c"))
carga <- c(24,17,18,26,22,20,24,38,31,30,19,30,26,26,20,
           24,27,27,23,29,24,36,21,22,31)

dados <- data.frame(linha,coluna,greco,trat,carga)
dados
str(dados)

modelo = lm(carga~trat+greco+linha+coluna,dados)
modelo
anova(modelo)

#---------------------------------------------------------
rm(list = ls())
library(gridExtra)
library(ggplot2)
#---------------------------------------------------------
# Exemplo de decis�o sobre politicas de precos
#---------------------------------------------------------
# Uma companhia deseja demostrar atrav�s de um experimento
# o impacto em suas vendas considerando diferentes politicas 
# de pre�os e diferentes displays de ponto de venda, ao longo 
# dos dias de semana em diferentes bairros.
# Em estudos previos identificaram que os pre�os e uso dos 
# displays n�o tiveram impacto significativo nas vendas.
# Decidiram repetir o experimento, mudando as politicas de
# pre�os e utilizar os mesmos displays. O objetivo do estudo � decidir se
# elimina os displays e encontrar a estrategia de precio adequando.

# 5 pre�os 
# 5 tipos  de display
# 5 d�as da semana: segunda a sexta
# 5 regi�es 

Regiao <-factor( c(rep("Norte",1), rep("Sul",1), rep("Este",1), rep("Oeste",1), rep("Centro",1)))
Dia <- factor(c(rep("Segunda",5), rep("Ter�a",5), rep("Quarta",5), rep("Quinta",5), rep("Sexta",5)),
              levels = c('Segunda','Ter�a','Quarta','Quinta','Sexta'))
Preco <- factor(c("A","E","C","B","D", "C","B","A","D","E", "B","C","D","E","A", "D","A","E","C","B", "E","D","B","A","C"))
Display <- factor(c("a","b","c","d","e",  "c","d","e","a","b",
                    "e","a","b","c","d",  "b","c","d","e","a",
                    "d","e","a","b","c"))
Vendas <- c(17,26,19,16,13, 18,21,18,16,28, 20,12,16,35,13, 15,15,22,14,17, 41,34,27,27,24)*33
mydata <- data.frame(Regiao, Dia, Preco,Display, Vendas)
head(mydata)
str(mydata)

di <- ggplot(mydata, aes(x = Dia, y = Vendas, fill=Dia)) +
  geom_boxplot() + theme(legend.position = "none")
pre <- ggplot(mydata, aes(x = Preco, y = Vendas, fill=Preco)) +
  geom_boxplot() + theme(legend.position = "none")
dis <- ggplot(mydata, aes(x = Display, y = Vendas, fill=Display)) +
  geom_boxplot() + theme(legend.position = "none")
zon <- ggplot(mydata, aes(x = Regiao, y = Vendas, fill=Regiao)) +
  geom_boxplot() + theme(legend.position = "none")

grid.arrange(pre,dis,di,zon, nrow=2,ncol=2)


# Gr�ficos de intera��o
in_pre <- ggplot(data = mydata,
                 aes(x = Dia, y = Vendas, colour = Preco, group=Preco)) +
  stat_summary(fun.y=mean, geom="point")+
  stat_summary(fun.y=mean, geom="line")+
  ylab('Vendas M�dias')
in_dis <- ggplot(data = mydata,
                 aes(x = Dia, y = Vendas, colour = Display, group=Display)) +
  stat_summary(fun.y=mean, geom="point")+
  stat_summary(fun.y=mean, geom="line")+
  ylab('Vendas M�dias')
in_pre_zo <- ggplot(data = mydata,
                    aes(x = Regiao, y = Vendas, colour = Preco, group=Preco)) +
  stat_summary(fun.y=mean, geom="point")+
  stat_summary(fun.y=mean, geom="line")+
  ylab('Vendas M�dias')
in_dis_zo <- ggplot(data = mydata,
                    aes(x = Regiao, y = Vendas, colour = Display, group=Display)) +
  stat_summary(fun.y=mean, geom="point")+
  stat_summary(fun.y=mean, geom="line")+
  ylab('Vendas M�dias')

grid.arrange(in_pre,in_dis,in_pre_zo,in_dis_zo, nrow=2,ncol=2)
grid.arrange(in_pre,in_dis,nrow=1,ncol=2)
grid.arrange(in_pre_zo,in_dis_zo,nrow=1,ncol=2)


myfit <- lm(Vendas ~ Preco+Display+Dia+Regiao, mydata)
summary(myfit)

anova(myfit)