# 1-2-2017 MMRC-Epid JHZ

t <- read.table("table.txt",sep="\t",header=TRUE)
K <- 1
N <- sum(t$No.casualties)
r <- rmultinom(K, N, prob = t$No.casualties)
p_hat <- r/sum(r)
p <- drop(p_hat)
m <- K*N*p
variance  <-  p*(1-p)
covariance  <- -outer(p, p)
diag(covariance) <-  variance
covariance
cbind(t,m,se=sqrt(variance))

fit <- glm(No.casualties ~ Victim.mode*Striker.mode, data=t, family=poisson)
p <- predict(fit,se.fit=TRUE,type="response")
cbind(t,m,p)
#
# fit <- glm(No.casualties ~ Victim.dist+striker.dist, data=t, family=poisson)
#