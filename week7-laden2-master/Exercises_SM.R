
set.seed(69)
### Exercise 2

beta1 <- 2
beta2 <- 0.5

sivma <- matrix(c(1,-0.9,-0.9,1), nrow = 2)


x1 <- rnorm(n = 100,
            mean = 2,
            sd = 2)

x2 <- rnorm(n = 100,
            mean = 2,
            sd = 2)

u <- MASS::mvrnorm(n = 100,
                   mu = c(0,0),
                   Sigma = sivma)


y1 <- beta1 * x1 + u[,1]
y2 <- beta2 * x2 + u[,2]

lm(y1 ~ x1 -1) %>% summary
lm(y2 ~ x2 -1) %>% summary

fit1 <- y1 ~ x1 - 1
fit2 <- y2 ~ x2 - 1


systemfit(formula = list(fit1, fit2),
          method = "SUR") %>% coef


### 1000 rep experiment

beta1_est_ols <- list()
beta1_est_sur <- list()

for(i in 1:1000){

x1 <- rnorm(n = 100,
            mean = 2,
            sd = 2)

x2 <- rnorm(n = 100,
            mean = 2,
            sd = 2)

u <- MASS::mvrnorm(n = 100,
                   mu = c(0,0),
                   Sigma = sivma)

y1 <- beta1 * x1 + u[,1]
y2 <- beta2 * x2 + u[,2]

fit1 <- y1 ~ x1 - 1
fit2 <- y2 ~ x2 - 1

beta1_est_ols[[i]] <- lm(y1 ~ x1 -1) %>% coef

sur_beta1 <- systemfit(formula = list(fit1, fit2),
                       method = "SUR") %>% coef

beta1_est_sur[[i]] <- sur_beta1[1]

}

ols <- bind_rows(beta1_est_ols) %>% as.data.frame()

names(ols) <- "OLS"

sur <- bind_rows(beta1_est_sur) %>% as.data.frame()

names(sur) <- "SUR"

dat <- data.frame("ols" = ols,
              "sur" = sur) %>% as_tibble

ggplot(data = dat) +
  geom_density(aes(x = OLS)) + 
  geom_density(aes(x = SUR), color = "red")


### Exercise 3

library(Ecdat)

dat <- Ecdat::Capm

food <- lm(rmrf ~ rfood, data = dat) %>% summary
dur <- lm(rmrf ~ rdur, data = dat) %>% summary
con <- lm(rmrf ~ rcon, data = dat) %>% summary

fit1 <- rmrf ~ rfood
fit2 <- rmrf ~ rdur
fit3 <- rmrf ~ rcon

Sur_mod <- systemfit(formula = list(fit1, fit2, fit3),
          method = "SUR", data = dat) %>% summary
