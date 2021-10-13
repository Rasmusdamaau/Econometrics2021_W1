library(data.table)
library(magrittr)

data <- rio::import("self-study1-laden-vm2022-main/m_wage.csv", setclass = "data.table")



DID <- function(data,pre,post) {
  
  means <- data[,
                .(mean_pre = mean(get(pre),na.rm=T), mean_post = mean(get(post),na.rm=T)),
                by = nj
  ][]
  # means
  DID <- means[,.(mean_diff = mean_post - mean_pre), by = nj]
  DID
  
}

DID(data,"wage_st","wage_st2")

DID(data,"emptot","emptot2")

DID(data,"pmeal","pmeal2")


long_d <- melt(data,id.vars = c("bk","kfc","wendys"),
     measure.vars = c("wage_st","wage_st2","emptot","emptot2","pmeal","pmeal2"))

long_d[,post := fifelse(grepl("2",variable),1,0)]
