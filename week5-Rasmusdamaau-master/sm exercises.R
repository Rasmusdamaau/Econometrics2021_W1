
library(dplyr)
library(wooldridge)
library(plm)

data <- wooldridge::rental

data %>% names

lm(lrent ~ y90 + lpop + lavginc + pctstu, data = data)
### pooling


plm(lrent ~ y90 + lpop + lavginc + pctstu, data = data,
    model = "pooling")

### first difference

plm(lrent ~ y90 + lpop + lavginc + pctstu, data = data,
    model = "fd")

### fixed effect

plm(lrent ~ y90 + lpop + lavginc + pctstu, data = data,
    model = "within")
