# 31-1-2017 MMRC-Epid JHZ

t <- read.table("table.txt",sep="\t",header=TRUE)

fit <- glm(No.casualties ~ Victim.mode*Striker.mode, data=t, family=poisson)
p <- predict(fit,se.fit=TRUE,type="response")
cbind(t,p)
#
# fit <- glm(No.casualties ~ Victim.dist+striker.dist, data=t, family=poisson)
#