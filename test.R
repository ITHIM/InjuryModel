test <- read.table("test.dat",header=TRUE)

library(dplyr)
by_test <- test %>% group_by(x,y,z)
ntest <- mutate(arrange(test,x,y,z), i=unlist(lapply(group_size(by_test),FUN=seq_len)))
ntest
