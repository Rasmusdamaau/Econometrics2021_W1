library(dplyr)
library(fUnitRoots)
### Exercise 1

dat <- rio::import("renter.csv") %>% 
  rename("1YearInterest" = y ,
         "3YearInterest" = z)

par(mfrow = c(2,1))

plot.ts(dat$`1YearInterest`)
plot.ts(dat$`3YearInterest`)


adfTest(dat$`1YearInterest`, type = "c") #Not stationary
adfTest(dat$`3YearInterest`, type = "c") #Not stationary


### Testing for integration order: First testing for I(1)

adfTest(diff(dat$`1YearInterest`), type = "c")
adfTest(diff(dat$`3YearInterest`), type = "c")

### THey are I(1)

forecast::auto.arima(dat$`1YearInterest`)
forecast::auto.arima(dat$`3YearInterest`)

### Both estimates an ARIMA(0,1,2)
### Cointegration

egcm::egcm(X = dat$`1YearInterest`, Y = dat$`3YearInterest`)

ecm::ecm(y = dat$`1YearInterest`, xeq = dat %>% select(`3YearInterest`), xtr = dat %>% select(`3YearInterest`))

### Exercise 2

### Difference between AR and GARCH is that the AR models lagged values of Y only,
### Garch models lagged values of the error.

### To initially see arch/garch effects, one can plot the diff'd series and then
### spot whether there are certain points in time where the volatility spikes or 
### changes (klynger af volatility). Also non-constant volatility. Else by plotting returns
### and observing tails.

### To test for arch effects (time varying heteroskedacity in residuals), one can use
### the lagrange multipler test. First, estimate an AR(q) model for the data y
### then obtain the squared residuals from that regression, and regress them on a
### constant and q lagged values of the squared residuals. 
### null hypothesis is absence of arch components, which is that all the estimated
### coefficients are 0 for the q lagged values of the squared residuals.

dat <- rio::import("volatilitet.csv")

dat$dato <- dat$dato %>% lubridate::dmy()

ggplot2::ggplot(data = dat, mapping = ggplot2::aes(x = dato)) +
  ggplot2::geom_line(ggplot2::aes(y = c(0,diff(log(aktiekurs))))) # Plotting the data

library(rugarch)

mod <- ugarchspec(mean.model = list(armaOrder = c(5,0)))

rugarch::ugarchfit(spec = mod, data = diff(log(dat$aktiekurs)))

### Test for arch effects

ar_mod <- forecast::auto.arima(dat$aktiekurs)

y <- residuals(ar_mod)^2 %>% as.numeric

mod <- lm(y ~ dplyr::lag(y) + dplyr::lag(y, n = 2) + dplyr::lag(y, n = 3) + dplyr::lag(y, n = 4) + dplyr::lag(y, n = 5))

mod %>% summary

###################
###  Exercise 3 ###
###################

dat <- rio::import("vektorautoregresssiv.csv")

dat$NominelOliePris <- c(NA,diff(log(dat$NominelOliePris)))
dat$RealBNP <- c(NA,diff(log(dat$RealBNP)))
dat$PrisIndeks <- c(NA,diff(log(dat$PrisIndeks)))

dat <- dat %>% na.omit

dat <- dat %>% 
  mutate(`RealOlieVækst` = NominelOliePris-PrisIndeks)

rownames(dat) <- NULL

sample <- dat[5:142,]

rownames(sample) <- NULL

varmodel <- vars::VAR(y = sample %>% select(PrisIndeks, RealBNP, RealOlieVækst), p = 2)

summary(varmodel)$roots ### Shows that the var model is stable. Hence it is stationary

### Impulse response

vars::irf(varmodel,
          ortho = FALSE) %>% plot

###########################
###### Exercise 4 #########
###########################

