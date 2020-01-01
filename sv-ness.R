source("lib/osu.R")
source("lib/util.R")

songFolder <- "D:/Games/osu!/Songs"
mapPaths <- c(
  "Camellia - Kamah (Scythe)/Camellia - Kamah (Scythe) (IceDynamix) [sv normalized].osu",
  "1016349 Akira Complex - Ether Strike/Akira Complex - Ether Strike (IceDynamix) [celestial cry (sv)].osu",
  "08. KillerBeast/Camellia - KillerBeast (IceDynamix) [hunter (sv)].osu",
  "658681 Alon Mor - Epollo [no video]/Alon Mor - Epollo (Couil) [feel the beat.].osu",
  "586848 Camellia - Backbeat Maniac/Camellia - Backbeat Maniac (Evening) [Rewind VIP].osu",
  "551690 Camellia - paroxysm/Camellia - paroxysm (Gekido-) [Challenge].osu"
)

data <- data.frame()

for (path in mapPaths) {
  osu <- osu.parseMap(paste(songFolder, path, sep = "/"))
  title <- osu.getTitle(osu)

  svPoints <- osu.getSvPoints(osu)
  rollingAvgOverTime <- osu.getSvRollingAverage(svPoints)$rollingAvg
  avg <- mean(rollingAvgOverTime)
  stdevLog10 <- sd(log10(rollingAvgOverTime))
  
  data <- rbind(data, data.frame(title, avg, stdevLog10))
}

View(data)