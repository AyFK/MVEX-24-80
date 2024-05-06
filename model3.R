library(spatstat)     # för spacial-temporal processer och statistikor
library(MASS)         # för MLE


# funktioner
source("plots.R")
source("simulation.R")

# variabler
source("imageAndBow.R")
image <- image # kommer från imageAndBow.R
Bow <- Bow # kommer från imageAndBow.R



################################################################################
# TIDSVARIERANDE INTENSITET
################################################################################

# välj personen som ska modelleras
targetSubjectId <- 1
testSubject <- subset(Bow, id == targetSubjectId & inpic == 1 & dur40 == 1)
realData <- ppp(testSubject$locX, testSubject$locY, window)

# välj resterande personer som modeller anpassas till
trainingData <- subset(Bow, id != targetSubjectId & inpic == 1 & dur40 == 1)

# MLE för att hitta form- och skal- paramterar till fördelningen av sackad- och
# fixerings-tid
gammaFixation <- fitdistr(trainingData$duration, "gamma")
gammaSaccade <- fitdistr(trainingData$presac, "gamma")



stateSample <- function(trainingData, lowerTime, upperTime, window,
                        numTestSubjects){
  # subset med timestamp-separation
  stateDf <- subset(trainingData, trainingData$timestamp >= lowerTime &
                    trainingData$timestamp <= upperTime)
  
  # gör 'stateDf' till ett ppp obejekt
  pppState <- ppp(stateDf$locX, stateDf$locY, window)
  
  # definera en bandwidth, låt spatstat hitta den optimala
  bandwidth <- bw.diggle(pppState)
  
  # skatta λ
  lambdaEst <- density(pppState, bandwidth, window=window,
                       positive=TRUE) / numTestSubjects
  
  # få ett sample
  pppSubSample <- rpoispp(lambda = lambdaEst, nsim=1)
  
  return (pppSubSample)
}


nSim = 40
stateProcess <- list()

for (n in 1:nSim) {
  cat ("state process", n, "/", nSim, "done \n")
  
  processTimestamp1 <- stateSample(trainingData, 0, 12000, window, 19)
  processTimestamp2 <- stateSample(trainingData, 12000, 24000, window, 19)
  processTimestamp3 <- stateSample(trainingData, 24000, 36000, window, 19)
  processTimestamp4 <- stateSample(trainingData, 36000, 48000, window, 19)
  processTimestamp5 <- stateSample(trainingData, 48000, 60000, window, 19)
  finalProcess <- superimpose(processTimestamp1, processTimestamp2,
                              processTimestamp3, processTimestamp4,
                              processTimestamp5)
  
  stateProcess[[paste("Simulation", n, sep = " ")]] <- finalProcess
}


stateSummary <- runSimulation(stateProcess, realData,
                              gammaFixation, gammaSaccade)


plotCanvas(realData, stateProcess$`Simulation 2`,
           "State Process", image, window)
  

plotConvexHullArea(stateSummary, nSim, "Konvext hölje: Modell 3")

plotScanpathDist(stateSummary, nSim, "Reslängd: Modell 3")

plotIntersectionRatio(stateSummary, nSim, "Skärningskvot: Modell 3")

plotRatioAvgHistogram(stateSummary, nSim, "Histogram: Modell 3")

