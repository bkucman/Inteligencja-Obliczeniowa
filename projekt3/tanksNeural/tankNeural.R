# Projekt 3 
# Błażej Kucman

install.packages("neuralnet")
library(neuralnet)

# read the data from game
data <- read.csv(file="D:\\Inforamtyka\\Inteligencja-Obliczeniowa\\projekt3\\strzaly",header=TRUE,sep=",")
#data$myTank.x[1]

normalize <- function(x) {
  
    (x-min(x))/(max(x)-min(x))
}

data.truncated <- data.frame(data[1:2], data[4], data[9],data[14:16], data[29:30])

data.truncated.normalize <- normalize(data.truncated)

outputNames <- names(data.truncated[5:7])
inputNames <- names(data.truncated[-5:-7])

namesCount <- length(data.truncated)
namesOutput.count <- length(outputNames)
namesInput.count <- length(inputNames)

formForNeural <- paste(paste(outputNames,collapse="+"),paste(inputNames,collapse = "+"),
                        sep="~")

tank.neuralnet111 <- neuralnet(formForNeural,data.truncated.normalize,
                            hidden = namesCount, threshold = 0.1)

plot(tank.neuralnet111)

tank.neuralnet[10]
# $`weights`
# $`weights`[[1]]
# $`weights`[[1]][[1]]

bias1 <- tank.neuralnet$weights[[1]][[1]][1,]
bias2 <- tank.neuralnet$weights[[1]][[2]][1,]

weights1 <- tank.neuralnet$weights[[1]][[1]][2:(namesInput.count+1),]
weights2 <- tank.neuralnet$weights[[1]][[2]][2:(namesCount+1),]

bias1a <- tank.neuralnet111$weights[[1]][[1]][1,]
bias2a <- tank.neuralnet111$weights[[1]][[2]][1,]

weights1a <- tank.neuralnet111$weights[[1]][[1]][2:(namesInput.count+1),]
weights2a <- tank.neuralnet111$weights[[1]][[2]][2:(namesCount+1),]

length(bias1) == namesCount
length(bias2) == namesOutput.count
length(weights1) == namesInput.count * namesCount
length(weights2) == namesCount * namesOutput.count

roundOnly <- function(x) {
  return(toString(round(x, 3)))
}

roundOnly(bias1a)
roundOnly(bias2a)
roundOnly(t(weights1a))
roundOnly(t(weights2a))

round(bias1,3)
round(bias2,3)
round(t(weights1),3)
round(t(weights2),3)
