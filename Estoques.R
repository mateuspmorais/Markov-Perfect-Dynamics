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
Estoques$n <- lag(Estoques$i, -1) - Estoques$i + lag(Estoques$x) 
