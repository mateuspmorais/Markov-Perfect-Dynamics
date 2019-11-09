# Estimation of Dynamic Models
# Mateus Proença Morais
#-------------------------------------------------------------------------------------------------------------#

rm(list = ls())
Sys.setlocale("LC_ALL","pt_BR.UTF-8")
options(encoding = "UTF-8")
# loading packages

library(plm)
library(readr)
require(graphics)
library(ggplot2)
library(Hmisc)
library(tseries)
library(ggfortify)
library(cowplot)
library(stargazer)

#reading and cleaning data

Estoques <- read_table2("http://www.economia.puc-rio.br/lrezende/OI1/Estoques.txt", 
                        col_names = c('loja','t','i','x'))
Estoques <- Estoques[-nrow(Estoques),]
Estoques <- pdata.frame(Estoques, index = c('loja','t'))
Estoques$lag_i <- lag(Estoques$i,-1)
Estoques$lag_x <- lag(Estoques$x)
Estoques$lag_i[is.na(Estoques$lag_i)] <- 0
Estoques$lag_x[is.na(Estoques$lag_x)] <- 0

#genrating the n series

Estoques$n <- pmin(Estoques$i + Estoques$x - Estoques$lag_i,Estoques$i) + pmax(Estoques$lag_x - Estoques$i,0)

#investigating the statistical properties of n

#histogram

hist(Estoques$n,main="Unidades de Produto Vendidas", xlab="Unidades de Produto Vendidas",
     ylab= "Numero de Firmas", breaks = 20)

#ols plot

ggplot(data = Estoques, aes(x = i, y = n)) + stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula= Estoques$n ~ Estoques$i) + 
  labs(x = 'Estoque', y = 'Unidades de Produto Vendidas') + 
  ggtitle('Estoque X Vendas') + theme_classic() +
  theme(plot.title = element_text(lineheight=2, face="bold", size = 14, hjust = 0.5))
  

#serial dependency

#arma <- arma(Estoques$n, order = c(1, 1))
#summary(arma)
#stargazer(arma)
p1 <- autoplot(acf(Estoques$n,lag.max = 8, plot = FALSE), conf.int.fill = '#0000FF', conf.int.value = 0.95, conf.int.type = 'ma') 
print(p1) 

p2 <- autoplot(pacf(Estoques$n,lag.max = 8, plot = FALSE), conf.int.fill = '#0000FF', conf.int.value = 0.95, conf.int.type = 'ar') 
print(p2) 

#estimation of the distribution of n

kernel <- density(Estoques$n) 
print(kernel)
plot(kernel,main="Unidades de Produto Vendidas \n (Distribuição Estimada)")

#estimation of the relationship between x and i (policy function)



