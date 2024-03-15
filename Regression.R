install.packages(ggplot2)
library(ggplot2)

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

test<-ggplot(dt,aes(x=epsilons_1,y=w_1)) + geom_point()

test
       
######
#2-2
lm(w_1 ~ epsilons_1, dt)
lm(w_1 ~ 0+epsilons_1, dt)
lm(epsilons_0 ~ epsilons_1, dt)

myReg <- lm(W~D, dt)
myReg$residuals
sum(myReg$residuals)

#####
install.packages("fixest")
library(fixest)
e1<-feols(w_1~epsilons_1, dt)
e2<-feols(w_1~0+epsilons_1, dt)
#####
etable(e1,e2, tex=T)
