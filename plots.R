plotConvexHullArea <- function(summaryStatistics, nSim, title) {
  ylimit <- 600000

  # axelar med heltal istället för förkortningar 3e+05, etc
  options(scipen = 10)
  
  plot(summaryStatistics$time1, summaryStatistics$dataHull1, type = "l", col =
       "#D55E00", lwd = 2, xlab = "Tid (ms)", ylab = "Area", main =
       title, ylim = c(0, ylimit))
  
  for (n in 1:nSim) {
    hullTag <- paste0("processHull", n)
    lines(summaryStatistics$time1, summaryStatistics[[hullTag]], col =
          "#56B4E9", lwd = 2)
  }
  
  legend("bottomright", legend = c("Data", "Simulering"),
         col = c("#D55E00", "#56B4E9"), lwd = 3, bty = "n", y.intersp = 1.5)
  
  lines(summaryStatistics$time1, summaryStatistics$dataHull1, col =
         "#D55E00", lwd = 2)

  par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5) # större fonts för plot
}


plotScanpathDist <- function(summaryStatistics, nSim, title) {
  ylimit <- 140000
  
  # axelar med heltal istället för förkortningar 3e+05, etc 
  options(scipen = 10)
  
  plot(summaryStatistics$time1, summaryStatistics$dataPath1, type = "l",
       col ="#D55E00", lwd = 3, xlab = "Tid (ms)", ylab = "Reslängd", main =
       title, ylim = c(0, ylimit))
  
  for (n in 1:nSim) {
    hullTag <- paste0("processPath", n)
    lines(summaryStatistics$time1, summaryStatistics[[hullTag]], col =
            "#56B4E9", lwd = 2)
  }
  
  lines(summaryStatistics$time1, summaryStatistics$dataPath1, col =
         "#D55E00", lwd = 2)
  
  legend("bottomright", legend = c("Data", "Simulering"),
         col = c("#D55E00", "#56B4E9"), lwd = 3, bty = "n", y.intersp = 1.5)

  par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5) # större fonts för plot
}


plotIntersectionRatio <- function(summaryStatistics, nSim, title) {
  ylimit <- 1.1
  
  # axelar med heltal istället för förkortningar 3e+05, etc 
  options(scipen = 10)
  
  plot(summaryStatistics$time1, summaryStatistics$ratio1, type = "n",
       lwd = 3, xlab = "Tid (ms)", ylab = "Kvot", main = title,
       ylim = c(0, ylimit))
  
  for (n in 1:nSim) {
    ratioTag <- paste0("ratio", n)
    lines(summaryStatistics$time1, summaryStatistics[[ratioTag]], col =
            "#56B4E9", lwd = 2)
  }
  

  avgRatio <- rowMeans(summaryStatistics[, grepl("^ratio",
                                                 names(summaryStatistics))])
  
  lines(summaryStatistics$time1, avgRatio, col = "#D55E00", lwd = 3)

  par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5) # större fonts för plot
}


plotRatioAvgHistogram <- function(summaryStatistics, nSim, title) {
  averages <- rep(0, nSim)
  
  for (n in 1:nSim) {
    ratioTag <- paste0("ratio", n)
    averages[n] <- mean(summaryStatistics[[ratioTag]])  
  }
  
  numBins <- 7
  range <- range(averages)
  breaks <- seq(range[1], range[2], length.out = numBins + 1)
  
  hist(averages, main = title, xlab = "Medelvärde", ylab = "Frekvens",
       breaks = breaks, col = "#56B4E9")

  par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5) # större fonts för plot
}


plotEnvelopes <- function(pppObeject) {
  envelopesResult <- envelope(pppObeject, fun=Lest, nsim=99, global=TRUE)
  plot(envelopesResult, main = "Global L-function")
  par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5) # större fonts för plot
}


plotCanvas <- function(realData, processData, title, image, window) {
  plot(window, main = title, lty = 2)
  plot(image, add = TRUE)
  plot(realData, col="#D55E00", pch = 19, lwd = 1, add = TRUE)
  plot(processData, col="#56B4E9", pch = 1, lwd = 2, add = TRUE)
  
  legend("topright", legend = c("Data", "Simulering"), col = c("#D55E00", "#56B4E9"),
         pch = c(19, 19), pt.cex = 2, bty = "n", y.intersp = 1.5)
}
