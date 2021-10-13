library(data.table)


data <- rio::import("self-study1-laden-vm2022-main/m_wage.csv", setclass = "data.table")



data[,
     `:=`(mean_pre = mean(wage_st,na.rm=T), mean_post = mean(wage_st2,na.rm=T)),
     by = nj
][]

means <- data[,
     .(mean_pre = mean(wage_st,na.rm=T), mean_post = mean(wage_st2,na.rm=T)),
     by = nj
][]

DID <- 
