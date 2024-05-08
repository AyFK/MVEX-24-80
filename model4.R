library(spatstat)     # för spatio-temporala processer och statistikor
library(MASS)         # för MLE

# funktioner
source("plots.R")
source("simulation.R")

# variabler
source("imageAndBow.R")
image <- image # kommer från imageAndBow.R
Bow <- Bow # kommer från imageAndBow.R



################################################################################
# SEKVENTIELL PUNKTSIMULERING
################################################################################
# välj personen som ska modelleras
targetSubjectId <- 1
testSubject <- subset(Bow, id == targetSubjectId & inpic == 1 & dur40 == 1)
realData <- ppp(testSubject$locX, testSubject$locY, window)

# används för intensitetsskattning
trainingData <- subset(Bow, id != targetSubjectId & inpic == 1 & dur40 == 1
                       & timestamp < 60001)

trainingEyeData <- ppp(trainingData$locX, trainingData$locY, window)
bandwidth <- bw.diggle(trainingEyeData)
numTestSubjects <- 19
lambdaEst <- density(trainingEyeData, bandwidth,
                     window=window, positive=TRUE) / numTestSubjects

testSubject <- subset(Bow, id == targetSubjectId & inpic == 1 & dur40 == 1
                      & timestamp < 60001)

# hitta avståndet r
r <- 334

nSim <- 40
ppos <- rpoispp(50, nsim = nSim)
simulations <- hyperframe(ppos=ppos) # att lagra kommande punktmönster i

savePoint <- function(finalPattern, nextPoint, fixation, saccade,
                      cumulativeTime, gammaFixation, gammaSaccade){
  
  finalPattern <- superimpose(finalPattern, nextPoint)
  
  # simulera fixerings- och sackadtider till punkten
  nextFixation <- rgamma(1, shape = gammaFixation$estimate[1],
                         rate = gammaFixation$estimate[2])
  nextSaccade <- rgamma(1, shape = gammaSaccade$estimate[1],
                        rate = gammaSaccade$estimate[2])
  
  fixation <- c(fixation, nextFixation)
  saccade <- c(saccade, nextSaccade)
  
  cumulativeTime <- cumulativeTime + nextFixation + nextSaccade
  
  return(list(finalPattern, fixation, saccade, cumulativeTime))
}

accProb <- 0.1 # välj sannolikhet att acceptera nya sackadlängder med

cat ("Reglera extrema sackadlängder...")
for (i in 1:nSim) {
  
  point <- rpoint(1, lambdaEst, positive=TRUE) # första punkten
  fixation <- rgamma(1, shape = gammaFixation$estimate[1],
                     rate = gammaFixation$estimate[2])
  saccade <- 0 
  cumulativeTime <- fixation
  finalPattern <- point
  
  while (cumulativeTime < 60000) {
    nextPoint <- rpoint(1, lambdaEst, positive=TRUE)
    
    if (crossdist(point, nextPoint) > r) { 
      jumplengthProb <- sample(c(0, 1), prob = c(accProb, (1-accProb))) # slumpa siffran 0 med extremeProb, annars 1
      
      if (jumplengthProb[1] == 0) { # om 0, acceptera punkten
        savedPoint <- savePoint(finalPattern, nextPoint, fixation, saccade,
                                cumulativeTime, gammaFixation, gammaSaccade)
        finalPattern <- savedPoint[[1]]
        fixation <- savedPoint[[2]]
        saccade <- savedPoint[[3]]
        cumulativeTime <- savedPoint[[4]]
        
        point <- nextPoint
      }
      
      else { # om 1, acceptera inte punkten
        next
      }
    }
    
    else { # acceptera punkten om euklidiska avståndet är mindre än r
      savedPoint <- savePoint(finalPattern, nextPoint, fixation, saccade,
                              cumulativeTime, gammaFixation, gammaSaccade)
      finalPattern <- savedPoint[[1]]
      fixation <- savedPoint[[2]]
      saccade <- savedPoint[[3]]
      cumulativeTime <- savedPoint[[4]]
      
      point <- nextPoint
    }
  }
  simulations$ppos[[i]] <- finalPattern
}
cat("Klar!")

nonHomogeneousProcess4 <- simulations$ppos
nonHomogeneousSummary4 <- runSimulation(nonHomogeneousProcess4, realData,
                                        gammaFixation, gammaSaccade)
index = 1
plot(nonHomogeneousProcess4[[index]], main=paste("Punktmönster", index, "från Modell 4"))
plotCanvas(realData, nonHomogeneousProcess4[[index]], "Modell 4", image, window)
plotConvexHullArea(nonHomogeneousSummary4, nSim, "Konvext hölje: Modell 4")
plotScanpathDist(nonHomogeneousSummary4, nSim, "Reslängd: Modell 4")
plotIntersectionRatio(nonHomogeneousSummary4, nSim, "Skärningskvot: Modell 4")
plotRatioAvgHistogram(nonHomogeneousSummary4, nSim, "Histogram: Modell 4")

