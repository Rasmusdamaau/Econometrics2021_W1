library(dplyr)

##### Exercise 3

beta1 <- 1
beta2 <- 0.8

coefs <- list()

y0 <- 0



for(j in 1:100){

noise <- rnorm(50)

y <- c(y0)

for(i in 2:length(noise)){
  y[i] <- beta1 + beta2 * y[i-1] + noise[i]
}

mod <- lm(y ~ dplyr::lag(y))

coefs[[j]] <- coef(mod)

}

est <- bind_rows(coefs) %>% sapply(mean)

beta1-est[[1]]

beta2-est[[2]]

#### Exercise 4

library(Ecdat)

dat <- Capm

# Food

y <- dat$rfood-dat$rf
x <- dat$rmrf-dat$rf

mod <- lm((rfood - rf) ~ (rmrf - rf) + jan, data = dat)

summary(mod)

# Durables

y <- dat$rdur-dat$rf
x <- dat$rmrf-dat$rf

mod <- lm(y ~ x - 1)

summary(mod)

# Construction

y <- dat$rcon-dat$rf
x <- dat$rmrf-dat$rf

mod <- lm(y ~ x - 1)

summary(mod)

##

dat$jan <- 0

for(i in seq(1,nrow(Capm), 12)){
  dat$jan[i] <- 1
}

### Exercise 5

dat <- rio::import("assets2.dat", setclass = "tibble")

dat %>% select(RMRF,
               SMB,
               HML,
               UMD,
               RF,
               R1,
               R7,
               R8,
               R10)

