#Limpando a memória
rm(list = ls())

require(labestData)

?CostaEx5.7.3

#Carregandos os dados
dados = CostaEx5.7.3
dados

attach(dados)

names(dados)

boxplot(prod~dose)

require(ExpDes.pt)

dic(dose,prod,quali = FALSE)

#plotando o grafico com as doses versus o peso
plot(prod~dose)

#criando a curva no grafico

#modelo quadrático
f = function(x) 8.8419 + 0.0950 * x - 0.0005 * x^2
curve(f,from = 0,to = 120,add=TRUE,col='red')

#Pacote para calcular a derivada
require(Deriv)

#Pergunta, qual o máximo da curva? 
#Qual a dose que gera esse valor?

#Calculando a derivada
derivada = Deriv(f,c("x"))
derivada

#Obtendo a dose que gera  o máximo
dose_ideal = 0.095/0.001
dose_ideal

#Calculando o peso máximo
prod_maximo = f(dose_ideal)
prod_maximo

#Inserindo o ponto no gráfico
points(dose_ideal,prod_maximo,pch=8,col='blue')

