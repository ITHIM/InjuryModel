voi<-read.csv(file.choose(), header=T, strip.white=TRUE)
dist
voi
match(voi$Victim, dist$V1)
voi$V2[match(voi$Victim, dist$V1)] ^ voi$SiNVict 
voi$V2[match(voi$Victim, dist$V1)]
voi$Injuries2 <- voi$Injuries * dist$V2[match(voi$Victim, dist$V1)] ^ voi$SiNVict * dist$V2[match(voi$Other.mode,dist$V1)] ^voi$SiNStri
