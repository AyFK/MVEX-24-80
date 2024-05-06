library(spatstat)

newTimeSamples <- function(processResults, realData, gammaFixation,
                           gammaSaccade, timeIncrement) {
  
  numProcessSamples <- processResults$n
  numDataSamples <- realData$n
  
  # dra stickprov av fördelningen från fixering och sackad 
  fixationSamples <- rgamma(numProcessSamples, shape = gammaFixation$estimate[1],
                            rate = gammaFixation$estimate[2])
  
  saccadeSamples <- rgamma(numProcessSamples, shape = gammaSaccade$estimate[1],
                           rate = gammaSaccade$estimate[2])
  
  # övergångstid från punkt Pm = (Xm, Ym) till punkt Pn = (Xn, Yn) behöver både
  # sackad-tiden från Pm till Pn, men också fixerings-tiden under Pm.
  processIncrements <- c(0, fixationSamples) + c(saccadeSamples, 0)
  transitionTimeProcess <- cumsum(processIncrements)
  
  dataIncrements <- c(0, testSubject$duration) + c(testSubject$presac, 0)
  transitionTimeData <- cumsum(dataIncrements)
  
  # räkna ut när tiden ska ta slut
  stoppingTime <- 60000
  
  # överskatta tiden för att säkerställer att alla nödvändiga intervall täcks
  nearestMultiple <- ceiling(stoppingTime / timeIncrement) * timeIncrement
  
  # kumulativa summan av tids-intervallen
  transitionTimes <- seq(timeIncrement, nearestMultiple, by = timeIncrement)
  
  return(list(partition = transitionTimes, data = transitionTimeData, 
              process = transitionTimeProcess))
}


timeSearchAlgorithm <- function(currentTime, numSamples, transitionTimes,
                                startSearch = 1) {
  idx <- 0

  # sök efter korrekt tids-index
  for (i in startSearch:numSamples) {
    if (currentTime < transitionTimes[i]) {
      idx <- i - 1
      break
    }
  }
  
  if (idx == 0) { # om processen har nått sitt slut, välj sista värdet
    idx <- numSamples
  }
  return(idx)
}


addToDataFrame <- function(summaryStatistics, matrix, numRows, measures, idx,
                           timeIncrement) {
  # räkna ut antalet rader som saknas
  notMissingRows <- numRows - nrow(matrix)
  
  # fyll värden som saknas med det föregånde värdet och öka tid kumulativt
  if (notMissingRows > 0) {
    # skapa tomma rader
    emptyRows <- matrix(NA, nrow = notMissingRows, ncol = ncol(matrix))
    
    # lägg till raden i matrisen
    matrix <- rbind(matrix, emptyRows)
    
    # fyll tomma värden med det sista värdet som förekom
    for (col in 1:ncol(matrix)) {
      matrix[notMissingRows:numRows, col] <- zoo::na.locf(
                                          matrix[notMissingRows:numRows, col])
    }
    
    # öka tidskolumnen kumulativt
    for (row in notMissingRows:numRows) {
      matrix[row, 1] <- matrix[row-1, 1] + timeIncrement 
    }
  }
  
  # ge namn till kolumner
  colnames(matrix) <- paste0(measures, idx)
  
  # bind ihop kolumnerna
  summaryStatistics <- cbind(summaryStatistics, matrix)
  return(summaryStatistics)
}


runSimulation <- function(processResults, realData, gammaFixation, gammaSaccade,
                          timeIncrement = 200) {
  
  # ta fram antalet simulationer från process
  numSimulations <- length(processResults)
  
  # skapa en vektor där vi sparar simulations resultaten samt antalet rader (tid-
  # intervall) för respektive simulation.
  simulationSummaries <- vector("list", numSimulations)
  numRows <- rep(0, numSimulations)

  # mått från simulationen vi är intereserade av
  measures <- c("time", "xData", "yData", "xProcess", "yProcess",
                "dataHull", "processHull", "ratio", "dataPath", "processPath")
  
  # antalet mått som kommer sparas per simulation
  numSummaries <- length(measures)

  # påbörja simulationen, lägg resultaten i 'simulationSummaries'
  for (n in 1:numSimulations) {
    cat ("\n simulation: ", n, "/", numSimulations)
    
    # skapa en matris som sparar alla nödvändiga mått (tid, ratio, etc)
    summary <- matrix(nrow = 0, ncol = numSummaries)
  
    # välj aktuell process
    simulationTag <- paste("Simulation", n, sep = " ")
    processData <- processResults[[n]]
    
    # dra ett nytt urval för tidsintervallen
    times <- newTimeSamples(processData, realData, gammaFixation, gammaSaccade,
                            timeIncrement)
    
    
    # för varje tidspunkt, kalkylera valda statistikor
    for (t in 1:length(times$partition) ) {
      
      # för varje tids-index, lägg till en ny rad i 'summary'
      summary <- rbind(summary, rep(0, numSummaries))
      
      # placera 'currentTime' i 'summary' på index = 1
      currentTime <- times$partition[t]
      summary[t, 1] <- currentTime
      
      
      # hitta motsvarande tids-index för punkten på 'times$partition'
      dataIdx <- timeSearchAlgorithm(currentTime, realData$n,
                                     times$data)
      
      processIdx <- timeSearchAlgorithm(currentTime, processData$n,
                                        times$process)
  
      # med korrekt tids-index för punkten, ta fram koordinaterna och spara dessa
      # i 'summary' på index = 2, 3, för 'realData'
      summary[t, 2] <- realData$x[dataIdx]
      summary[t, 3] <- realData$y[dataIdx]
  
      # och index = 4, 5 för 'processData'
      summary[t, 4] <- processData$x[processIdx]
      summary[t, 5] <- processData$y[processIdx]
      
  
      # convex hull för data och process med korrekt tids-index, placera dem i
      # 'summary' på index = 6, 7
      dataHull <- convexhull(realData[0:dataIdx])
      processHull <- convexhull(processData[0:processIdx])
      
      areaDataHull <- area.owin(dataHull)
      summary[t, 6] <- areaDataHull
      
      areaProcessHull <- area.owin(processHull)
      summary[t, 7] <- areaProcessHull
      
      
      # intersection ratio har basfall värde av 0
      ratio <- 0
      
      if (areaProcessHull != 0) {
        # när det finns en convex hull area, räkna ut kvoten
        intersectArea <- area.owin(intersect.owin(processHull, dataHull,
                                                  fatal=FALSE))
        ratio <- intersectArea / areaProcessHull
      }
      
      # placera den i 'summary' på index = 8
      summary[t, 8] <- ratio
      
      
      # scanpath
      if (t > 1) {
        dataDist <- crossdist(realData[dataIdx-1], realData[dataIdx])[1]
        processDist <- crossdist(processData[processIdx-1],
                                 processData[processIdx])[1]
        
        # om avstånd saknas (t.ex samma blick på tavlan), sätt till 0
        if (is.na(dataDist)) {
          dataDist <- 0
        }
        
        if (is.na(processDist)) {
          processDist <- 0
        }
  
        summary[t, 9] <- summary[t-1, 9] + dataDist
        summary[t, 10] <- summary[t-1, 10] + processDist
      }
    }
    
    # när statistikorna för hela 'times$partition' är uträknade, spara 'summary'
    # matrisen och antalet rader matrisen har i var sin lista / vektor
    numRows[n] <- dim(summary)[1]
    simulationSummaries[[n]] <- summary
  }

  
  # hitta max antalet rader
  maxRows <- numRows[which.max(numRows)]
  
  
  # skapar en data frame för sammanfattande statistik med 'maxRows' rader
  summaryStatistics <- data.frame(matrix(nrow = maxRows, ncol = 0))
  
  
  # fyll 'summaryStatistics' med matriserna från simulationen
  for (n in 1:numSimulations) {
    summaryStatistics <- addToDataFrame(summaryStatistics, simulationSummaries[[n]],
                                        maxRows, measures, n, timeIncrement)
  }

  cat ("\nklar!\n")
  return(summaryStatistics)
}
