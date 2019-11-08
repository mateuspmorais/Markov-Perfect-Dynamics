# Estimation of dynamic Models
#Mateus Morais
#-------------------------------------------------------------------------------------------------------------#

rm(list = ls())

library(plm)
library(readr)


Estoques <- read_table2("http://www.economia.puc-rio.br/lrezende/OI1/Estoques.txt", 
                        col_names = c('loja','t','i','x'))
Estoques <- Estoques[-nrow(Estoques),]

Estoques <- pdata.frame(Estoques, index = c('loja','t'))
Estoques$lag_i <- lag(Estoques$i,-1)
Estoques$lag_i[is.na(Estoques$lag_i)] <- 0
Estoques$n <- pmin( - Estoques$lag_i + Estoques$i + Estoques$x,Estoques$i)
