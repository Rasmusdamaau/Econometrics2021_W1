
library(dplyr)

### Exercise 3

x <- rnorm(n = 50, mean = 2, sd = 2)

u <- rnorm(50, mean = 0, sd = 1)

beta1 <- 1

beta2 <- 0.8

y <- beta1 + beta2 * x + u

uyt <- rnorm(n = 50, mean = 0, sd = 5)

uxt <- rnorm(n = 50, mean = 0, sd = 4)

yprik <- y + uyt

xprik <- x + uxt

lm(yprik ~ x) %>% summary

lm(y ~ xprik) %>% summary

 ### Exercise 4

library(readr)

classical <- read_csv("week2-evv-master/classical.csv")

mods <- lm(Y ~ X2 + X3, data = classical)

summary(mods)

tmp <- list()

for(i in 1:9999){
  
  dat <- classical[sample(nrow(classical), replace = T),] 
  
  mod <- lm(Y ~ X2 + X3, data = dat)
  
  tmp[[i]] <- mod$coefficients[[3]]
  
}

sim <- tmp %>% unlist

sim %>% quantile(probs = 0.95)

### Exercise 5

dat <- Ecdat::Crime

mod <- lm(crmrte ~ prbarr + prbconv + prbpris + avgsen + polpc + density, data = dat)

summary(mod)
