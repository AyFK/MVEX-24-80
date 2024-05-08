library(jpeg)         # för att hämta och modifiera bild
library(spatstat) 

load("Bow.RData")
painting <- readJPEG("P4.jpg")

Bow <- subset(Bow, Bow$timestamp <= 60000)

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
