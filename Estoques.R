# Estimation of Dynamic Models
# Mateus Proença Morais
#-------------------------------------------------------------------------------------------------------------#


rm(list = ls())
Sys.setlocale("LC_ALL","pt_BR.UTF-8")
options(encoding = "UTF-8")
# loading packages
require("numDeriv")
library(plm)
library(readr)
require(graphics)
library(ggplot2)
library(Hmisc)
library(tseries)
library(ggfortify)
library(cowplot)
library(stargazer)
library(AER)
library(extraDistr)
library(e1071)
library(stats)

path <- "C:\\Users\\mateus.morais\\Downloads\\Estoques.txt"
#reading and cleaning data
set.seed(123)
Estoques <- read_table2(path, 
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

for(j in 1:7){
  politica[j + 21,] <- c(1,0,0,0,0,0,0,0)
}
ggplot(data = Estoques, aes(x = i, y = x)) + stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula= Estoques$i ~ Estoques$x) +
  ylim(0, 20) + xlim(0, 20) +
  labs(y = 'Encomendas', x = 'Estoque') + 
  ggtitle('Estoque X Vendas') + theme_classic() +
  theme(plot.title = element_text(lineheight=2, face="bold", size = 14, hjust = 0.5))



#Estimation of the Value functions
start_time_w = Sys.time()

#defining variables
w <- data.frame(matrix(ncol = 4, nrow = 3000))
w[,1] <- 0
w[,2] <- 0 
w[,3] <- 0 
w[,4] <- 0


w_hat <- array(dim = c(3000,4,500))
w_hat[,1,] <- 0
w_hat[,2,] <- 0 
w_hat[,3,] <- 0 
w_hat[,4,] <- 0

probi0 <- data.frame(probi0 = numeric())

for(j in 0:max(Estoques$i)) {
  probi0[j,1] <- mean(Estoques$i[Estoques$t==1] == j)
}

i0 <- rdiscrete(3000, probs = unlist(probi0), values = c(0:19))
action <- data.frame(a1 = numeric())
action_hat <- array(dim = c(3000,500))

it <- i0
it_hat <- array(dim = c(3000,500))
for(i in 1:500){
  it_hat[,i] = i0
}
politica_hat <- array(dim = c(500))
politica_hat <- rnorm(500,8,1)


#Loop
for(i in 1:100) {
  shocks <- rdunif(3000, 1, 8)
  
  for(j in 1:length(it)) {
    action[j,1] <- rdiscrete(1, probs = unlist(politica[it[j]+ 1,]), values = c(0,12,13,14,15,16,17,18))
  }  
  
  #profit
  w[,1] <- w[,1] + ((0.95)^(i - 1))*10*pmin(it + unlist(action), unlist(shocks))
  w[,2] <- w[,2] + ((0.95)^(i - 1))*unlist(action) 
  w[,3] <- w[,3] + ((0.95)^(i - 1))*apply(action,2,function(action)ifelse((action>0),1,0))
  w[,4] <- w[,4] + ((0.95)^(i - 1))*unlist(it)^2  
  
  #transition
  it = pmax.int(it + unlist(action) - shocks,0)
  
  
  #profit of alternative policies
  
  for(j in 1:500){
    for(l in 1:3000){
      if(it_hat[l,j] < politica_hat[j]){action_hat[l,j] <- rdiscrete(1,probs = unlist(politica[it_hat[l,j]+ 1,]),
                                                                     values = c(0,12,13,14,15,16,17,18))} else {
                                                                       action_hat[l,j] <- 0
                                                                     }}
    action_hat[,j] <- ifelse(action_hat[,j] > 0 , max(action_hat[,j] + round(rnorm(1,0,0.1)),0), 0)
    
    subset <- action_hat[,j]
    dummy <- apply(array(subset),1,function(subset)ifelse((subset>0),1,0))
    #profit
    w_hat[,1,j] <- w_hat[,1,j] + ((0.95)^(i - 1))*10*pmin(it_hat[j,] + unlist(action_hat[,j]), unlist(shocks))
    w_hat[,2,j] <- w_hat[,2,j] + ((0.95)^(i - 1))*unlist(action_hat[,j]) 
    w_hat[,3,j] <- w_hat[,3,j] + ((0.95)^(i - 1))*dummy
    w_hat[,4,j] <- w_hat[,4,j] + ((0.95)^(i - 1))*unlist(it_hat[j,])^2  
    
    #transition
    it_hat[,j] = pmax.int(it_hat[,j] + unlist(action_hat[,j]) - shocks,0)
  }}
end_time_w <- Sys.time()

wh_mean<- data.frame(matrix(ncol = 4, nrow = 500))
for(i in 1:500){
  wh_mean[i,1] <- mean(w_hat[,1,i])
  wh_mean[i,2] <- - mean(w_hat[,2,i])
  wh_mean[i,3] <- - mean(w_hat[,3,i])
  wh_mean[i,4] <- - mean(w_hat[,4,i])
}

w_mean <- w[1,]
w_mean[1,1] <- mean(w[,1])
w_mean[1,2] <- - mean(w[,2])
w_mean[1,3] <- - mean(w[,3])
w_mean[1,4] <- - mean(w[,4])
g <- matrix(ncol = 4, nrow = 500)
for(k in 1:500){
  g[k,] <- as.matrix(w_mean - wh_mean[k,])
}


#minimization function
start_time_min <- Sys.time()
minfunc <- function(par) {sum(pmin(g[,1] + g[,2]*par[1] + g[,3]*par[2] + g[,4]*par[3],0)^2)
}
(estimates <- optim(par = c(1000,1000,0), fn = minfunc))

end_time_min <- Sys.time()

end_time_w - start_time_w

