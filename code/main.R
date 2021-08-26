# Generate Time Course Matrix

TCdata = c()
AV = c(0,20,0,0,0,0)
IV = c(30,45,60,40,40,40)
DO = c(15,20,25,15,20,25)

len = 0
for (c in 1:length(AV)){
  i = 1
  while (i <= 240){
    if (i < AV[c]){
      TCdata[len+1] = 0
      len = len+1
      i = i+1
    }
    else{
      j = 1
      while (j <= IV[c] && i <= 240){
        if (j <= DO[c]){
          TCdata[len+1] = 1
          len = len+1
          i = i+1
        }
        else{
          TCdata[len+1] = 0
          len = len+1
          i = i+1
        }
        j = j+1
      }
    }
  }
}

TC = matrix(data = TCdata, nrow = 240, ncol = 6)


# Standardisation

for (c in 1:6){
  m = mean(TC[,c])
  std = sd(TC[,c])
  TC[,c] = TC[,c] - m
  TC[,c] = TC[,c] / std
}


# Plotting

png(filename = "plots/TC1plot.png")
plot(x = 1:240, y = TC[,1], type = "l")
dev.off()
png(filename = "plots/TC2plot.png")
plot(x = 1:240, y = TC[,2], type = "l")
dev.off()
png(filename = "plots/TC3plot.png")
plot(x = 1:240, y = TC[,3], type = "l")
dev.off()
png(filename = "plots/TC4plot.png")
plot(x = 1:240, y = TC[,4], type = "l")
dev.off()
png(filename = "plots/TC5plot.png")
plot(x = 1:240, y = TC[,5], type = "l")
dev.off()
png(filename = "plots/TC6plot.png")
plot(x = 1:240, y = TC[,6], type = "l")
dev.off()


# TC Correlation Plot
library("corrplot")

png(filename = "plots/TCCorrelationPlot.png")
corrplot(cor(TC))
dev.off()


# Spatial Matrix

SMdata = c()
xmin = c(2, 2, 8, 8, 15, 15)
xmax = c(6, 6, 13, 13, 19, 19)
ymin = c(2, 15, 2, 15, 2, 15)
ymax = c(6, 19, 6, 19, 6, 19)
for (i in 1:6){
  M = matrix(0, 21, 21)
  for (x in xmin[i]:xmax[i]){
    for (y in ymin[i]:ymax[i]){
      M[x, y] = 1
    }
  }
  SMdata = append(SMdata, unlist(as.list(M)))
}

SM = matrix(data = SMdata, nrow = 441, ncol = 6)
SM = t(SM)

# SM Correlation Plot

png(filename = "plots/SMCorrelationPlot.png")
corrplot(cor(t(SM)))
dev.off()





















