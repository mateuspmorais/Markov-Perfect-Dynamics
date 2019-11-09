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
library(extrafont)
library(extrafontdb)
library(showtext)
library(AER)

#reading and cleaning data
set.seed(123)
Estoques <- read_table2("http://www.economia.puc-rio.br/lrezende/OI1/Estoques.txt", 
                        col_names = c('loja','t','i','x'))
Estoques <- Estoques[-nrow(Estoques),]
Estoques <- pdata.frame(Estoques, index = c('loja','t'))
Estoques$lag_i <- lag(Estoques$i,1)
Estoques$forw_i <- lag(Estoques$i,-1) 
Estoques$forw_i[is.na(Estoques$forw_i)] <- 0
Estoques$lag_x <- lag(Estoques$x)
Estoques$lag_i[is.na(Estoques$lag_i)] <- 0
Estoques$lag_x[is.na(Estoques$lag_x)] <- 0



#genrating the n series

Estoques$n <- Estoques$i + Estoques$x - Estoques$forw_i
Estoques$m <- 0
for(i in 1:nrow(Estoques)) {
    row <- Estoques[i,]
   if(row$t == 10){Estoques[i,9] <- NA}else{Estoques[i,9] <- row$n}
}
Estoques[1889,9] =6
#investigating the statistical properties of n

#histogram

hist(Estoques$m,main="Unidades de Produto Vendidas", xlab="Unidades de Produto Vendidas",
     ylab= "Densidade", breaks = c(0,1,2,3,4,5,6,7,8,9), xlim= c(0, 10), freq = FALSE)


#ols plot

ggplot(data = Estoques, aes(x = m, y = i)) + stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula= Estoques$i ~ Estoques$m) + ylim(0, 20) + xlim(0, 10) +
  labs(y = 'Estoque', x = 'Unidades de Produto Vendidas') + 
  ggtitle('Estoque X Vendas') + theme_classic() +
  theme(plot.title = element_text(lineheight=2, face="bold", size = 14, hjust = 0.5))
  


#arma <- arma(Estoques$n, order = c(1, 1))
#summary(arma)
#stargazer(arma)
p1 <- autoplot(acf(Estoques$n,lag.max = 8, plot = FALSE), conf.int.fill = '#0000FF', conf.int.value = 0.95, conf.int.type = 'ma') 
print(p1) 

p2 <- autoplot(pacf(Estoques$n,lag.max = 8, plot = FALSE), conf.int.fill = '#0000FF', conf.int.value = 0.95, conf.int.type = 'ar') 
print(p2) 

#estimation of the distribution of n

kernel <- density(Estoques$m,na.rm = TRUE) 
print(kernel)
plot(kernel,main="Unidades de Produto Vendidas \n (Distribuição Estimada)")

#estimation of the relationship between x and i (policy function)
ggplot(Estoques, aes(x=i, y=x)) + geom_point() + ggtitle('Estoque X Encomendas') + theme_classic() +
  theme(plot.title = element_text(lineheight=2, face="bold", family = 'Helvetica',size = 14, hjust = 0.5)) + 
  labs(x = 'Estoque', y = 'Encomendas')

hist(Estoques$x,main="Encomendas", xlab="Encomendas",
     ylab= "Numero de Firmas", breaks = 20)

politica <- data.frame(prob0 = numeric(), prob12 = numeric(),
                       prob13 = numeric(), prob14 = numeric(), prob15 = numeric(),
                       prob16 = numeric(), prob17 = numeric(), prob18 = numeric())
for(j in 0:max(Estoques$i)) {
politica[j + 1,1] <- mean(Estoques$x[Estoques$i==j] == 0)
}
for(j in 0:max(Estoques$i)) {
politica[j + 1,2] <- mean(Estoques$x[Estoques$i==j] == 12)
}
for(j in 0:max(Estoques$i)) {
politica[j + 1,3] <- mean(Estoques$x[Estoques$i==j] == 13)
}
for(j in 0:max(Estoques$i)) {
politica[j + 1,4] <- mean(Estoques$x[Estoques$i==j] == 14)
}
for(j in 0:max(Estoques$i)) {
politica[j + 1,5] <- mean(Estoques$x[Estoques$i==j] == 15)
}
for(j in 0:max(Estoques$i)) {
politica[j + 1,6] <- mean(Estoques$x[Estoques$i==j] == 16)
}

for(j in 0:max(Estoques$i)) {
politica[j + 1,7] <- mean(Estoques$x[Estoques$i==j] == 17)
}

for(j in 0:max(Estoques$i)) {
politica[j + 1,8] <- mean(Estoques$x[Estoques$i==j] == 18)
}

ggplot(data = Estoques, aes(x = i, y = x)) + stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula= Estoques$i ~ Estoques$x) +
  ylim(0, 20) + xlim(0, 20) +
  labs(y = 'Encomendas', x = 'Estoque') + 
  ggtitle('Estoque X Vendas') + theme_classic() +
  theme(plot.title = element_text(lineheight=2, face="bold", size = 14, hjust = 0.5))
  
