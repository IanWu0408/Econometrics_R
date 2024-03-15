#HW2

install.packages("data.table")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("MASS")
library("data.table")
library("tidyverse")
library("dplyr")
library("MASS")

library(ggplot2)

set.seed(123)

###########
#generate random variable \epsilon_1 \epsilon_2
mean_vector <- c(0,0)
cov_matrix <- matrix(c(0.64,0.16,0.16,0.16),nrow = 2,byrow = TRUE)

gen_epsilons <- as.data.table(mvrnorm(n=100,000,000,mu=mean_vector,Sigma = cov_matrix))
setnames(epsilons, c("epsilon_0","epsilon_1"))

#generate random variable \epsilon
epsilon <- epsilons$epsilon_1 - epsilons$epsilon_0
epsilon_mean <- mean(epsilon)
epsilon_sd <- sd(epsilon)

#Calculate R_0
R_0 <- pnorm(-0.2/epsilon_sd)

print(epsilon_mean)
print(epsilon_sd)
print(R_0)

###########

mu_0 <- 0.8
mu_1 <- 1.0
N <- 1000000
dt <- data.table(
  ID = 1:N,
  mu_0 = mu_0,
  mu_1 = mu_1
)

mean_vector <- c(0,0)
cov_matrix <- matrix(c(0.64,0.16,0.16,0.16),nrow = 2,byrow = TRUE)

epsilons <- as.data.table(mvrnorm(n=N,mu=mean_vector,Sigma = cov_matrix))
setnames(epsilons, c("epsilon_0","epsilon_1"))

mean(epsilons$epsilon_0);var(epsilons$epsilon_0)
mean(epsilons$epsilon_1);var(epsilons$epsilon_1)
cov(epsilons$epsilon_0,epsilons$epsilon_1)


dt[, epsilons_0 := epsilons$epsilon_0][, epsilons_1 := epsilons$epsilon_1]
dt[, w_0 := mu_0 + epsilons_0][, w_1 := mu_1 + epsilons_1]


dt[, D := ifelse(w_0 >= w_1, 0 , 1)]
dt[, W := ifelse(D==0,w_0, w_1)]


dt0bs <- dt[, .(ID, D, W)]
dt0bs [, career_choice := ifelse(D==0,"Economist","Accountant")]

prop.table(table(dt0bs$career_choice))
mean(dt0bs[D == 0]$W)
mean(dt0bs[D == 1]$W)

ggplot(aes(x=dt$epsilon_1,y=dt$w_1)+geom_point()

lm

#############
# Experiment

p<-0.5

dt[, D_exp := rbinom(N,1,p)][, W_exp := ifelse( D_exp ==0, w_0, w_1)]

dtExp <- dt[, .(ID, D_exp, W_exp)]
dtExp[, career_choice := ifelse(D_exp ==0, "Economist", "Accountant")]

prop.table(table(dtExp$career_choice))
mean(dtExp[ D_exp == 0]$W_exp)
mean(dtExp[ D_exp == 1]$W_exp)



###############


lm

