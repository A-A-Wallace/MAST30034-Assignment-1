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


# Adding Noise

gammaTdata = c()

for (i in 1:1440){
  gammaTdata[i] = rnorm(1, sd = sqrt(0.25))
}

gammaT = matrix(gammaTdata, nrow = 240, ncol = 6)

gammaSdata = c()

for (i in 1:2646){
  gammaSdata[i] = rnorm(1, sd = sqrt(0.015))
}

gammaS = matrix(gammaSdata, nrow = 6, ncol = 441)

png(filename = "plots/gammaTcorrelationPlot.png")
corrplot(cor(gammaT))
dev.off()

png(filename = "plots/gammaScorrelationPlot.png")
corrplot(cor(t(gammaS)))
dev.off()

png(filename = "plots/gammaThistogram.png")
hist(gammaTdata)
dev.off()

png(filename = "plots/gammaShistogram.png")
hist(gammaSdata)
dev.off()

gammaProd = gammaT %*% gammaS

png(filename = "plots/gammaProductCorrelationPlot.png")
corrplot(cor(gammaProd))
dev.off()

# Building X

X = (TC + gammaT) %*% (SM + gammaS)

Xsample = X[, sample.int(441, 100)]

Xsample = data.frame(Xsample)

png(filename = "plots/XsamplesPlot.png")
plot(1:240, Xsample[,1], type = "l")
for (i in 2:100){
  points(1:240, Xsample[,i], type = "l")
}
dev.off()

Xvars = c()
for (i in 1:441){
  Xvars[i] = var(X[,i])
}
png(filename = "plots/XPlot.png")
plot(1:441, Xvars)
dev.off()

for (c in 1:441){
  m = mean(X[,c])
  std = sd(X[,c])
  X[,c] = X[,c] - m
  X[,c] = X[,c] / std
}

# Finding A and D using least squares estimation

DtDinv = solve(t(TC) %*% TC)
Alsr = DtDinv %*% t(TC) %*% X

Dlsr = X %*% t(Alsr)

png(filename = "plots/retrievedTCs.png")
plot(x = 1:240, y = Dlsr[,1], type = "l")
points(x = 1:240, y = Dlsr[,2], type = "l")
points(x = 1:240, y = Dlsr[,3], type = "l")
points(x = 1:240, y = Dlsr[,4], type = "l")
points(x = 1:240, y = Dlsr[,5], type = "l")
points(x = 1:240, y = Dlsr[,6], type = "l")
dev.off()

png(filename = "plots/Dlsr3X30scatterplot.png")
plot(x = Dlsr[,3], y = X[,30])
dev.off()

png(filename = "plots/Dlsr4X30scatterplot.png")
plot(x = Dlsr[,4], y = X[,30])
dev.off()


# Finding A and D using ridge regression

lambda = 0.5
DtDrrinv = solve(t(TC) %*% TC + lambda * 441 * diag(nrow = 6, ncol = 6))
Arr = DtDrrinv %*% t(TC) %*% X

Drr = X %*% t(Arr)

ctlsr = c()
for (i in 1:6){
  ctlsr[i] = abs(cor(TC[,i], Dlsr[,i]))
}

ctrr = c()
for (i in 1:6){
  ctrr[i] = abs(cor(TC[,i], Drr[,i]))
}
print("sum of Ctlsr: ")
print(sum(ctlsr))
print("sum of Ctrr: ")
print(sum(ctrr))

newLambda = 1000
newDtDrrinv = solve(t(TC) %*% TC + newLambda * 441 * diag(nrow = 6, ncol = 6))
newArr = newDtDrrinv %*% t(TC) %*% X

newDrr = X %*% t(newArr)

png(filename = "plots/ArrAlsrPlot.png")
plot(Alsr[1,], newArr[1,])
dev.off()

# Lasso regression

# Function taken from the assignment description
lassoRegression = function(rho){
  N = 441
  step <- 1/(norm(TC %*% t(TC)) * 1.1)
  thr <- rho*N*step
  Ao <- matrix(0, nsrcs, 1)
  A <- matrix(0, nsrcs, 1)
  Alr <- matrix(0, nsrcs, x1*x2)
  
  for (k in 1:(x1*x2)) {
    A <- Ao+step*(t(TC) %*% (X[,k]-(TC%*%Ao)))
    A <- (1/(1+thr)) * (sign(A)*pmax(replicate(nsrcs, 0), abs(A)-thr))
    
    for (i in 1:10) {
      Ao <- A
      A <- Ao+step * (t(TC)%*%(X[,k]-(TC%*%Ao)))
      A <- (1/(1+thr)) * (sign(A)*pmax(replicate(nsrcs, 0), abs(A)-thr))
    }
    Alr[,k] <- A
  }
  return (Alr)
}

#lassoRegression(0)

# Principal Component Regression

library("MASS")

PCTC = svd(TC)



