library(dplyr)

### Opgave 2

set.seed(69)

beta0 <- 1

beta1 <- 1

x <- rnorm(n = 30, mean = 2, sd = 2)

u <- c()

for(i in 1:30){
  u[i] <- rnorm(n = 1, mean = 0, sd = exp(0.6 * x[i]))
}

y <- beta0 + beta1 * x + u

modlm <- lm(y ~ x)

summary(modlm)

cov_hetro <- sandwich::vcovHC(modlm)

stat <- cov_hetro[2,2]

coef(modlm)[2]/stat

###

Weigths <- 1/sqrt((exp(0.6*x)))

ytilde <- Weigths * y

xtilde <- Weigths * x

gls <- lm(ytilde ~ xtilde)

summary(gls)

### fgls


