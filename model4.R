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
                     window=window) / numTestSubjects

testSubject <- subset(Bow, id == targetSubjectId & inpic == 1 & dur40 == 1
                      & timestamp < 60001)

# MLE för att hitta form- och skal- parametrar till fördelningen av sackad- och
# fixeringstidslängder
gammaFixation <- fitdistr(testSubject$duration, "gamma")
gammaSaccade <- fitdistr(testSubject$presac, "gamma")


# anpassa en gammafördelning på alla personers sackadlängder för att välja avståndet r
allSubjects <- subset(Bow, inpic == 1 & dur40 == 1
                       & timestamp < 60001)
gammaFit <- fitdistr(allSubjects$jumplength, "gamma")
x <- seq(0, max(allSubjects$jumplength), length.out = 100)
hist(allSubjects$jumplength, prob = TRUE, 
     main = "Histogram av sackadlängd", 
     ylim = c(0,0.005),
     xlab="Sackadlängd (pixlar)", ylab="Täthet")
legend("topright", legend = c(paste("Form = ", round(gammaFit$estimate["shape"], digits=2), sep = ""), 
                              legend = paste("Skala = ", round(1 / gammaFit$estimate["rate"], digits=2), sep = "")),  
       fill = "#56B4E9")
lines(x, dgamma(x, shape = gammaFit$estimate["shape"], 
                rate = gammaFit$estimate["rate"]), col = "#56B4E9", lwd = 2)

x1 <- seq(0, 2000, length.out = 1000) # 2000 valt för att nå till 1 på cdfValues nedan
shape <- gammaFit$estimate[1]
rate <- gammaFit$estimate[2]
cdfValues <- pgamma(x1, shape, rate)
plot(x1, cdfValues, type = "l", ylim = c(0, 1), 
     xlab = "Sackadlängd (pixlar)", ylab = "Sannolikhet",
     main = "Fördelningsfunktion av \n gammafördelningen av sackadlängd")

q = 0.9 # välj kvantil för 'extrema' sackadlängder

# hitta avståndet r
allR <- data.frame(x=x1,y=cdfValues)
allR$y <- round(allR$y, digits=2)
r <- allR$x[which(allR$y==q)[1]]

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

for (i in 1:nSim) {
  
  point <- rpoint(1, lambdaEst) # första punkten
  fixation <- rgamma(1, shape = gammaFixation$estimate[1],
                     rate = gammaFixation$estimate[2])
  saccade <- 0 
  cumulativeTime <- fixation
  finalPattern <- point
  
  while (cumulativeTime < 60000) {
    
    nextPoint <- rpoint(1, lambdaEst)
    
    if (crossdist(point, nextPoint) > r) { 
      jumplengthProb <- sample(c(0,1), prob = c(accProb,(1-accProb))) # slumpa siffran 0 med extremeProb, annars 1
      
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

