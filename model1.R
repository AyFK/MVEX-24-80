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
# HOMOGEN POISSONPROCESS
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

# skatta λ, välj samma intensitet per 1x1 pixel
lambdaEst1 <- length(testSubject$locX) / (height * width)


# simulera process
nSim <- 40
homogeneousProcess <- rpoispp(lambda = lambdaEst1, nsim = nSim, win = window)

# gör ppp obeject av riktiga datan
realData <- ppp(testSubject$locX, testSubject$locY, window)

homogeneousSummary <- runSimulation(homogeneousProcess, realData, gammaFixation,
                                    gammaSaccade)

plotCanvas(realData, homogeneousProcess$`Simulation 1`,
           "Homogen Poisson", image, window)

plotCanvas(realData, homogeneousProcess$`Simulation 1`,
           "Homogen Poisson", image, window)

plotConvexHullArea(homogeneousSummary, nSim, "Konvext hölje: Modell 1")

plotScanpathDist(homogeneousSummary, nSim, "Reslängd: Modell 1")

plotIntersectionRatio(homogeneousSummary, nSim, "Skärningskvot: Modell 1")

plotRatioAvgHistogram(homogeneousSummary, nSim, "Histogram: Modell 1")
# icke-FSS
#plotEnvelopes(realData)

# FSS
#plotEnvelopes(homogeneousProcess$`Simulation 1`)

