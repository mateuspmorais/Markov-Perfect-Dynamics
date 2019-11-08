# Estimation of dynamic Models
#Mateus Morais
#-------------------------------------------------------------------------------------------------------------#

rm(list = ls())

library(plm)
library(readr)
require(graphics)
library(ggplot2)
library(Hmisc)

Estoques <- read_table2("http://www.economia.puc-rio.br/lrezende/OI1/Estoques.txt", 
                        col_names = c('loja','t','i','x'))
Estoques <- Estoques[-nrow(Estoques),]

Estoques <- pdata.frame(Estoques, index = c('loja','t'))
Estoques$lag_i <- lag(Estoques$i,-1)
Estoques$lag_x <- lag(Estoques$x)
Estoques$lag_i[is.na(Estoques$lag_i)] <- 0
Estoques$lag_x[is.na(Estoques$lag_x)] <- 0


Estoques$n <- pmin(Estoques$i + Estoques$x - Estoques$lag_i,Estoques$i) + pmax(Estoques$lag_x - Estoques$i,0)


hist(Estoques$n)

ggplot(data = Estoques, aes(x = i, y = n))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm',formula= Estoques$n ~ Estoques$i)
