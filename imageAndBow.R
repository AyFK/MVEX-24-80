library(jpeg)         # för att hämta och modifiera bild
library(spatstat) 

load("Bow.RData")
painting <- readJPEG("P4.jpg")


# data detaljer
missingValsIdxNA <- which(is.na(Bow$presacinpic))
goodDataIdx <- which(!is.na(Bow$presacinpic))
missingValsIdx <- missingValsIdxNA[-1] ### la till den här raden
### missingVals till for-loopen fick inte innehålla rad 1 för då funkade inte max

for (i in missingValsIdx) {
  # Hittar största värdet innan NaN
  prevValidIdx <- max(goodDataIdx[goodDataIdx < i])
  
  # Adderar det värdet med det "ite" missingValsIdx
  Bow$duration[prevValidIdx] <- Bow$duration[i] + Bow$duration[prevValidIdx]
  Bow$presac[prevValidIdx] <- Bow$presac[i] + Bow$presac[prevValidIdx]
}
Bow <- Bow[-missingValsIdxNA, ] ## tar bort avsaknade värden ### la till NA här

Bow <- Bow[-1, ] # ta bort 1a element, saknar fixering



Bow <- subset(Bow, Bow$timestamp <= 60000)
Bow <-na.omit(Bow)


# bild detaljer
window <- owin(c(114, 908), c(0, 767))
width <- 908 - 114
height <- 767

imageMatrix <- rgb(painting[,,1], painting[,,2], painting[,,3])
dim(imageMatrix) <- dim(painting)[1:2]
image <- im(transmat(imageMatrix, "European", "spatstat"))



# variabler som behövs i andra filer
assign("image", image, envir = .GlobalEnv)
assign("Bow", Bow, envir = .GlobalEnv)
