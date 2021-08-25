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
jpeg(filename = "TC plots.jpeg", quality = 100)
par(mfrow = c(3,2))




































