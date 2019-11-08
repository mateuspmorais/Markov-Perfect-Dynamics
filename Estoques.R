# Estimation of dynamic Models
# Mateus Proença Morais
#-------------------------------------------------------------------------------------------------------------#

rm(list = ls())

# loading packages

library(plm)
library(readr)
require(graphics)
library(ggplot2)
library(Hmisc)
library(tseries)
library(ggfortify)
library(cowplot) 

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
hist(Estoques$n)

#OLS PLOT
ggplot(data = Estoques, aes(x = i, y = n))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm',formula= Estoques$n ~ Estoques$i)

#serial dependency
arma <- arma(Estoques$n, order = c(1, 1))
summary(arma)

p1 <- autoplot(acf(Estoques$n,lag.max = 8, plot = FALSE), conf.int.fill = '#0000FF', conf.int.value = 0.95, conf.int.type = 'ma') 
print(p1) 

p2 <- autoplot(pacf(Estoques$n,lag.max = 8, plot = FALSE), conf.int.fill = '#0000FF', conf.int.value = 0.95, conf.int.type = 'ar') 
print(p2) 

# contemporaneous relation


#estimation of the distribution of n
