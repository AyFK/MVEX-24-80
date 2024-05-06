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
# ICKE-HOMOGEN POISSONPROCESS
################################################################################

# välj personen som ska modelleras
targetSubjectId <- 1
testSubject <- subset(Bow, id == targetSubjectId & inpic == 1 & dur40 == 1)
realData <- ppp(testSubject$locX, testSubject$locY, window)

# välj resterande personer som modeller anpassas till
trainingData <- subset(Bow, id != targetSubjectId & inpic == 1 & dur40 == 1)

# gör det till ett point pattern obejekt
trainingEyeData <- ppp(trainingData$locX, trainingData$locY, window)


# MLE för att hitta form- och skal- paramterar till fördelningen av sackad- och
# fixerings-tid
gammaFixation <- fitdistr(trainingData$duration, "gamma")
gammaSaccade <- fitdistr(trainingData$presac, "gamma")


# definera en bandwidth, låt spatstat hitta den optimala
bandwidth <- bw.diggle(trainingEyeData)

# skatta λ, med hjälp av 'trainingEyeData' + 'bandwidth', och ta deras
# snitt genom att dela med antalet test 'numTestSubjects'
numTestSubjects <- 19

lambdaEst <- density(trainingEyeData, bandwidth,
                      window=window) / numTestSubjects


# simulera process
nSim <- 40
nonHomogeneousProcess <- rpoispp(lambda = lambdaEst, nsim = nSim)



nonHomogeneousSummary <- runSimulation(nonHomogeneousProcess, realData,
                                       gammaFixation, gammaSaccade)



plotCanvas(realData, nonHomogeneousProcess$`Simulation 1`,
           "Icke-homogen Poisson", image, window)


# plotta
plotConvexHullArea(nonHomogeneousProcess, nSim, "Konvext hölje: Modell 2")

plotScanpathDist(nonHomogeneousProcess, nSim, "Reslängd: Modell 2")

plotIntersectionRatio(nonHomogeneousProcess, nSim, "Skärningskvot: Modell 2")

plotRatioAvgHistogram(nonHomogeneousProcess, nSim, "Histogram: Modell 2")
