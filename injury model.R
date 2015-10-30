voi<-read.csv(file="data/voi.csv", header=T, strip.white=TRUE)
dist<-read.csv(file="data/dist.csv", header=T, strip.white=TRUE)
voi$Injuries2 <- voi$Injuries * dist$change.in.distance[match(voi$Victim, dist$mode)] ^ voi$SiNVict * dist$change.in.distance[match(voi$Other.mode,dist$mode)] ^voi$SiNStri
