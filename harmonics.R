PlotOctave <- function(freq) {
  abline(v=freq*2^(-0.5:6.5/7)) ## White keys: 7 equal divisions
  blacks <- c(1,2,4,5,6)   ## 5 black keys: 0.8-1.2, 1.8-2.2, 3.8-4.2, 4.8-5.2, 5.8-6.2
  for (black in blacks){
    bs <- freq * 2 ^ c((black-0.75)/7, (black-0.25)/7)
    rect(bs[1], 0.5, bs[2], 1, col='black')
  }
}
cs <- 261.63*2^(-2:3)
harmonics <- 261.63*1:8
MakePlots <- function(text) {
  sapply(cs, PlotOctave)
  points(harmonics, rep(0.4, length(harmonics)), col='red', pch=16)
  points(harmonics[1], 0.4, col='blue', pch=16)
  title(text)
}
par(mfrow=c(2,1))
plot(1, 0, type='n', xlim=c(260,2100), ylim=c(0.2,0.9), xlab="frequency", ylab="", yaxt="n")
MakePlots("Harmonics are multiples of the fundamental frequency")
plot(1, 0, type='n', xlim=c(260,2100), ylim=c(0.2,0.9), xlab="frequency", log="x", ylab="", yaxt="n")
MakePlots("In log scale (perceptual scale), harmonics get closer and closer")


# Plot violin next to piano




