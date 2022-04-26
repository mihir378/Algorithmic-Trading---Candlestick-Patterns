############# STRATEGIES w TECHNICAL PATTERNS #############
library(xts)
library(PerformanceAnalytics)
library(ggplot2)
library(quantmod)
library(dplyr)

############ ADDITIONAL PACKAGE ###########
library(devtools)
install_github("kochiuyu/CandleStickPattern")
library(CandleStickPattern)

########### CHOOSE INDEX/STOCK ############
getSymbols("AAPL")
price <- Cl(AAPL)
Close <- Cl(AAPL)
Open <- Op(AAPL)
Vol <- (AAPL$AAPL.Volume)

######### Bullish/ Buying Patterns ##########
bull_eng <- bullish.engulf(AAPL)
bull_har <- bullish.harami(AAPL)
hammer   <- hammer(AAPL, US.delta = 0.1, LS.delta = 0.7)
pear_line <- piercing.line(AAPL)
morn_star <- morning.star(AAPL, n =20, L.delta = 0.8, S.delta = 0.2)
three_ws <- three.white.soldiers(AAPL,n=20,delta=0.8)
  
######### Bearish/ Selling Patterns ##########
bear_eng <- bearish.engulf(AAPL)
bear_har <- bearish.harami(AAPL)
inv_hammer <- inverted.hammer(AAPL, US.delta=0.7, LS.delta=0.1) 
dark_clud <- dark.cloud.cover(AAPL)
eve_star <-  evening.star(AAPL, n=20, L.delta = 0.8, S.delta = 0.2)
three_bc <- three.black.crows(AAPL, n=20, delta=0.8)
  
  
###### Additional Confirmation signals ########
gp_up <- gap.up(AAPL)                                         # Gap Up Signal
gp_dn <- gap.down(AAPL)                                       # Gap Down signal
vol20 <- SMA(AAPL$AAPL.Volume, n=20)                          # 20d SMA - Volume 

##### MATRIX CREATION ########
strat <- as.data.frame(matrix(1:36, nrow = 6, 
                              dimnames = list(c("Bull Eng.","Bull Har.","Hammer", "Pier. Line","Morn. Star", "3 White Soldiers"), 
                                              c("Bear Eng.","Bear Har.","Inv. Hammer", "Dark Cloud","Eve. Star", "3 Black Crows"))))
print(strat)

################################# 1. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (bull_eng[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (bear_eng[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
  }
}
signal_1<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_1[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret1<-reclass(return,price)

cum.ret1 <- Return.cumulative(ret1,geometric = TRUE)
ann.ret1 <- Return.annualized(ret1, geometric = TRUE)
semisd.ret1 <- SemiSD(ret1)


################################# 2. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (bull_har[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (bear_eng[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_2<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_2[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret2<-reclass(return,price)

cum.ret2 <- Return.cumulative(ret2,geometric = TRUE)
ann.ret2 <- Return.annualized(ret2, geometric = TRUE)
semisd.ret2 <- SemiSD(ret2)

################################# 3. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (hammer[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (bear_eng[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_3<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_3[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret3<-reclass(return,price)

cum.ret3 <- Return.cumulative(ret3,geometric = TRUE)
ann.ret3 <- Return.annualized(ret3, geometric = TRUE)
semisd.ret3 <- SemiSD(ret3)

################################# 4. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (pear_line[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (bear_eng[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_4<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_4[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret4<-reclass(return,price)

cum.ret4 <- Return.cumulative(ret4,geometric = TRUE)
ann.ret4 <- Return.annualized(ret4, geometric = TRUE)
semisd.ret4 <- SemiSD(ret4)

################################# 5. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (morn_star[i] && Vol[i] > vol20[i]){  
    signal[i] <- 1
  } else if (bear_eng[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_5<-reclass(signal,price)
trade <- Lag(signal_5)

for (i in (day+2):length(price)){
  profit[i] <- qty * trade[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret5<-reclass(return,price)

cum.ret5 <- Return.cumulative(ret5,geometric = TRUE)
ann.ret5 <- Return.annualized(ret5, geometric = TRUE)
semisd.ret5 <- SemiSD(ret5)

################################# 6. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (three_ws[i] && Vol[i] > vol20[i]){  
    signal[i] <- 1
  } else if (bear_eng[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_6<-reclass(signal,price)
trade <- Lag(signal_6)

for (i in (day+2):length(price)){
  profit[i] <- qty * trade[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret6<-reclass(return,price)

cum.ret6 <- Return.cumulative(ret6,geometric = TRUE)
ann.ret6 <- Return.annualized(ret6, geometric = TRUE)
semisd.ret6 <- SemiSD(ret6)

################################# 7. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (bull_eng[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (bear_har[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
  }
}
signal_7<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_7[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret7<-reclass(return,price)

cum.ret7 <- Return.cumulative(ret7,geometric = TRUE)
ann.ret7 <- Return.annualized(ret7, geometric = TRUE)
semisd.ret7 <- SemiSD(ret7)

################################# 8. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (bull_har[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (bear_har[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_8<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_8[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret8<-reclass(return,price)

cum.ret8 <- Return.cumulative(ret8,geometric = TRUE)
ann.ret8 <- Return.annualized(ret8, geometric = TRUE)
semisd.ret8 <- SemiSD(ret8)

################################# 9. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (hammer[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (bear_har[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_9<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_9[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret9<-reclass(return,price)

cum.ret9 <- Return.cumulative(ret9,geometric = TRUE)
ann.ret9 <- Return.annualized(ret9, geometric = TRUE)
semisd.ret9 <- SemiSD(ret9)

################################# 10. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (pear_line[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (bear_har[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_10<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_10[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret10<-reclass(return,price)

cum.ret10 <- Return.cumulative(ret10,geometric = TRUE)
ann.ret10 <- Return.annualized(ret10, geometric = TRUE)
semisd.ret10 <- SemiSD(ret10)

################################# 11. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (morn_star[i] && Vol[i] > vol20[i]){  
    signal[i] <- 1
  } else if (bear_har[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_11<-reclass(signal,price)
trade <- Lag(signal_11)

for (i in (day+2):length(price)){
  profit[i] <- qty * trade[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret11<-reclass(return,price)

cum.ret11 <- Return.cumulative(ret11,geometric = TRUE)
ann.ret11 <- Return.annualized(ret11, geometric = TRUE)
semisd.ret11 <- SemiSD(ret11)

################################# 12. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (three_ws[i] && Vol[i] > vol20[i]){  
    signal[i] <- 1
  } else if (bear_har[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_12<-reclass(signal,price)
trade <- Lag(signal_12)

for (i in (day+2):length(price)){
  profit[i] <- qty * trade[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret12<-reclass(return,price)

cum.ret12 <- Return.cumulative(ret12,geometric = TRUE)
ann.ret12 <- Return.annualized(ret12, geometric = TRUE)
semisd.ret12 <- SemiSD(ret12)

################################# 13. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (bull_eng[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (inv_hammer[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
  }
}
signal_13<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_13[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret13<-reclass(return,price)

cum.ret13 <- Return.cumulative(ret13,geometric = TRUE)
ann.ret13 <- Return.annualized(ret13, geometric = TRUE)
semisd.ret13 <- SemiSD(ret13)

################################# 14. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (bull_har[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (inv_hammer[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_14<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_14[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret14<-reclass(return,price)

cum.ret14 <- Return.cumulative(ret14,geometric = TRUE)
ann.ret14 <- Return.annualized(ret14, geometric = TRUE)
semisd.ret14 <- SemiSD(ret14)

################################# 15. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (hammer[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (inv_hammer[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_15<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_15[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret15<-reclass(return,price)

cum.ret15 <- Return.cumulative(ret15,geometric = TRUE)
ann.ret15 <- Return.annualized(ret15, geometric = TRUE)
semisd.ret15 <- SemiSD(ret15)

################################# 16. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (pear_line[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (inv_hammer[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_16<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_16[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret16<-reclass(return,price)

cum.ret16 <- Return.cumulative(ret16,geometric = TRUE)
ann.ret16 <- Return.annualized(ret16, geometric = TRUE)
semisd.ret16 <- SemiSD(ret16)

################################# 17. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (morn_star[i] && Vol[i] > vol20[i]){  
    signal[i] <- 1
  } else if (inv_hammer[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_17<-reclass(signal,price)
trade <- Lag(signal_17)

for (i in (day+2):length(price)){
  profit[i] <- qty * trade[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret17<-reclass(return,price)

cum.ret17 <- Return.cumulative(ret17,geometric = TRUE)
ann.ret17 <- Return.annualized(ret17, geometric = TRUE)
semisd.ret17 <- SemiSD(ret17)

################################# 18. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (three_ws[i] && Vol[i] > vol20[i]){  
    signal[i] <- 1
  } else if (inv_hammer[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_18<-reclass(signal,price)
trade <- Lag(signal_18)

for (i in (day+2):length(price)){
  profit[i] <- qty * trade[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret18<-reclass(return,price)

cum.ret18 <- Return.cumulative(ret18,geometric = TRUE)
ann.ret18 <- Return.annualized(ret18, geometric = TRUE)
semisd.ret18 <- SemiSD(ret18)

################################# 19. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (bull_eng[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (dark_clud[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
  }
}
signal_19<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_19[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret19<-reclass(return,price)

cum.ret19 <- Return.cumulative(ret19,geometric = TRUE)
ann.ret19 <- Return.annualized(ret19, geometric = TRUE)
semisd.ret19 <- SemiSD(ret19)

################################# 20. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (bull_har[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (dark_clud[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_20<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_20[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret20<-reclass(return,price)

cum.ret20 <- Return.cumulative(ret20,geometric = TRUE)
ann.ret20 <- Return.annualized(ret20, geometric = TRUE)
semisd.ret20 <- SemiSD(ret20)

################################# 21. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (hammer[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (dark_clud[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_21<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_21[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret21<-reclass(return,price)

cum.ret21 <- Return.cumulative(ret21,geometric = TRUE)
ann.ret21 <- Return.annualized(ret21, geometric = TRUE)
semisd.ret21 <- SemiSD(ret21)

################################# 22. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (pear_line[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (dark_clud[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_22<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_22[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret22<-reclass(return,price)

cum.ret22 <- Return.cumulative(ret22,geometric = TRUE)
ann.ret22 <- Return.annualized(ret22, geometric = TRUE)
semisd.ret22 <- SemiSD(ret22)

################################# 23. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (morn_star[i] && Vol[i] > vol20[i]){  
    signal[i] <- 1
  } else if (dark_clud[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_23<-reclass(signal,price)
trade <- Lag(signal_23)

for (i in (day+2):length(price)){
  profit[i] <- qty * trade[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret23<-reclass(return,price)

cum.ret23 <- Return.cumulative(ret23,geometric = TRUE)
ann.ret23 <- Return.annualized(ret23, geometric = TRUE)
semisd.ret23 <- SemiSD(ret23)

################################# 24. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (three_ws[i] && Vol[i] > vol20[i]){  
    signal[i] <- 1
  } else if (dark_clud[i-1] && Vol[i-1] > vol20[i-1] && gp_dn[i]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_24<-reclass(signal,price)
trade <- Lag(signal_24)

for (i in (day+2):length(price)){
  profit[i] <- qty * trade[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret24<-reclass(return,price)

cum.ret24 <- Return.cumulative(ret24,geometric = TRUE)
ann.ret24 <- Return.annualized(ret24, geometric = TRUE)
semisd.ret24 <- SemiSD(ret24)

################################# 25. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (bull_eng[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (eve_star[i] && Vol[i-1] > vol20[i-1]){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
  }
}
signal_25<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_25[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret25<-reclass(return,price)

cum.ret25 <- Return.cumulative(ret25,geometric = TRUE)
ann.ret25 <- Return.annualized(ret25, geometric = TRUE)
semisd.ret25 <- SemiSD(ret25)

################################# 26. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (bull_har[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (eve_star[i] && Vol[i-1] > vol20[i-1]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_26<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_26[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret26<-reclass(return,price)

cum.ret26 <- Return.cumulative(ret26,geometric = TRUE)
ann.ret26 <- Return.annualized(ret26, geometric = TRUE)
semisd.ret26 <- SemiSD(ret26)

################################# 27. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (hammer[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (eve_star[i] && Vol[i-1] > vol20[i-1]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_27<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_27[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret27<-reclass(return,price)

cum.ret27 <- Return.cumulative(ret27,geometric = TRUE)
ann.ret27 <- Return.annualized(ret27, geometric = TRUE)
semisd.ret27 <- SemiSD(ret27)

################################# 28. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (pear_line[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (eve_star[i] && Vol[i-1] > vol20[i-1]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_28<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_28[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret28<-reclass(return,price)

cum.ret28 <- Return.cumulative(ret28,geometric = TRUE)
ann.ret28 <- Return.annualized(ret28, geometric = TRUE)
semisd.ret28 <- SemiSD(ret28)

################################# 29. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (morn_star[i] && Vol[i] > vol20[i]){  
    signal[i] <- 1
  } else if (eve_star[i] && Vol[i-1] > vol20[i-1]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_29<-reclass(signal,price)
trade <- Lag(signal_29)

for (i in (day+2):length(price)){
  profit[i] <- qty * trade[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret29<-reclass(return,price)

cum.ret29 <- Return.cumulative(ret29,geometric = TRUE)
ann.ret29 <- Return.annualized(ret29, geometric = TRUE)
semisd.ret29 <- SemiSD(ret29)

################################# 30. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (three_ws[i] && Vol[i] > vol20[i]){  
    signal[i] <- 1
  } else if (eve_star[i] && Vol[i-1] > vol20[i-1]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_30<-reclass(signal,price)
trade <- Lag(signal_30)

for (i in (day+2):length(price)){
  profit[i] <- qty * trade[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret30<-reclass(return,price)

cum.ret30 <- Return.cumulative(ret30,geometric = TRUE)
ann.ret30 <- Return.annualized(ret30, geometric = TRUE)
semisd.ret30 <- SemiSD(ret30)

################################# 31. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (bull_eng[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (three_bc[i] && Vol[i-1] > vol20[i-1]){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
  }
}
signal_31<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_31[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret31<-reclass(return,price)

cum.ret31 <- Return.cumulative(ret31,geometric = TRUE)
ann.ret31 <- Return.annualized(ret31, geometric = TRUE)
semisd.ret31 <- SemiSD(ret31)

################################# 32. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (bull_har[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (three_bc[i] && Vol[i-1] > vol20[i-1]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_32<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_32[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret32<-reclass(return,price)

cum.ret32 <- Return.cumulative(ret32,geometric = TRUE)
ann.ret32 <- Return.annualized(ret32, geometric = TRUE)
semisd.ret32 <- SemiSD(ret32)

################################# 33. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (hammer[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (three_bc[i] && Vol[i-1] > vol20[i-1]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_33<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_33[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret33<-reclass(return,price)

cum.ret33 <- Return.cumulative(ret33,geometric = TRUE)
ann.ret33 <- Return.annualized(ret33, geometric = TRUE)
semisd.ret33 <- SemiSD(ret33)

################################# 34. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (pear_line[i-1] && Vol[i-1] > vol20[i-1] && gp_up[i]){  
    signal[i] <- 1
  } else if (three_bc[i] && Vol[i-1] > vol20[i-1]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_34<-reclass(signal,price)
#trade <- Lag(signal)

for (i in (day+2):length(price)){
  profit[i] <- qty * signal_34[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret34<-reclass(return,price)

cum.ret34 <- Return.cumulative(ret34,geometric = TRUE)
ann.ret34 <- Return.annualized(ret34, geometric = TRUE)
semisd.ret34 <- SemiSD(ret34)

################################# 35. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (morn_star[i] && Vol[i] > vol20[i]){  
    signal[i] <- 1
  } else if (three_bc[i] && Vol[i-1] > vol20[i-1]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_35<-reclass(signal,price)
trade <- Lag(signal_35)

for (i in (day+2):length(price)){
  profit[i] <- qty * trade[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret35<-reclass(return,price)

cum.ret35 <- Return.cumulative(ret35,geometric = TRUE)
ann.ret35 <- Return.annualized(ret35, geometric = TRUE)
semisd.ret35 <- SemiSD(ret35)

################################# 36. #################################
qty <-1
day <-20

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (three_ws[i] && Vol[i] > vol20[i]){  
    signal[i] <- 1
  } else if (three_bc[i] && Vol[i-1] > vol20[i-1]){  
    signal[i] <- 0
  } else {          
    signal[i] <- signal[i-1]
  }
}
signal_36<-reclass(signal,price)
trade <- Lag(signal_36)

for (i in (day+2):length(price)){
  profit[i] <- qty * trade[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret36<-reclass(return,price)

cum.ret36 <- Return.cumulative(ret36,geometric = TRUE)
ann.ret36 <- Return.annualized(ret36, geometric = TRUE)
semisd.ret36 <- SemiSD(ret36)

###### Collate Results - Buy/Sell Stratgy Matrices #####

# Creating Cumulative Return Matrix 

col1 <- rbind(cum.ret1,cum.ret2,cum.ret3,cum.ret4,cum.ret5,cum.ret6)
col2 <- rbind(cum.ret7,cum.ret8,cum.ret9,cum.ret10,cum.ret11,cum.ret12)
col3 <- rbind(cum.ret13,cum.ret14,cum.ret15,cum.ret16,cum.ret17,cum.ret18)
col4 <- rbind(cum.ret19,cum.ret20,cum.ret21,cum.ret22,cum.ret23,cum.ret24)
col5 <- rbind(cum.ret25,cum.ret26,cum.ret27,cum.ret28,cum.ret29,cum.ret30)
col6 <- rbind(cum.ret31,cum.ret32,cum.ret33,cum.ret34,cum.ret35,cum.ret36)

cum.ret.matrix <- cbind.data.frame(col1,col2,col3,col4,col5,col6)
colnames(cum.ret.matrix) <- c("Bear Eng.","Bear Har.","Inv. Hammer", "Dark Cloud","Eve. Star", "3 Black Crows")
rownames(cum.ret.matrix) <- c("Bull Eng.","Bull Har.","Hammer", "Pier. Line","Morn. Star", "3 White Soldiers")
View(cum.ret.matrix)

# Creating Annualised Return Matrix

col1 <- rbind(ann.ret1,ann.ret2,ann.ret3,ann.ret4,ann.ret5,ann.ret6)
col2 <- rbind(ann.ret7,ann.ret8,ann.ret9,ann.ret10,ann.ret11,ann.ret12)
col3 <- rbind(ann.ret13,ann.ret14,ann.ret15,ann.ret16,ann.ret17,ann.ret18)
col4 <- rbind(ann.ret19,ann.ret20,ann.ret21,ann.ret22,ann.ret23,ann.ret24)
col5 <- rbind(ann.ret25,ann.ret26,ann.ret27,ann.ret28,ann.ret29,ann.ret30)
col6 <- rbind(ann.ret31,ann.ret32,ann.ret33,ann.ret34,ann.ret35,ann.ret36)

ann.ret.matrix <- cbind.data.frame(col1,col2,col3,col4,col5,col6)
colnames(ann.ret.matrix) <- c("Bear Eng.","Bear Har.","Inv. Hammer", "Dark Cloud","Eve. Star", "3 Black Crows")
rownames(ann.ret.matrix) <- c("Bull Eng.","Bull Har.","Hammer", "Pier. Line","Morn. Star", "3 White Soldiers")
View(ann.ret.matrix)

# Creating SemiSD Matrix

col1 <- rbind(semisd.ret1,semisd.ret2,semisd.ret3,semisd.ret4,semisd.ret5,semisd.ret6)
col2 <- rbind(semisd.ret7,semisd.ret8,semisd.ret9,semisd.ret10,semisd.ret11,semisd.ret12)
col3 <- rbind(semisd.ret13,semisd.ret14,semisd.ret15,semisd.ret16,semisd.ret17,semisd.ret18)
col4 <- rbind(semisd.ret19,semisd.ret20,semisd.ret21,semisd.ret22,semisd.ret23,semisd.ret24)
col5 <- rbind(semisd.ret25,semisd.ret26,semisd.ret27,semisd.ret28,semisd.ret29,semisd.ret30)
col6 <- rbind(semisd.ret31,semisd.ret32,semisd.ret33,semisd.ret34,semisd.ret35,semisd.ret36)

semisd.ret.matrix <- cbind.data.frame(col1,col2,col3,col4,col5,col6)
colnames(semisd.ret.matrix) <- c("Bear Eng.","Bear Har.","Inv. Hammer", "Dark Cloud","Eve. Star", "3 Black Crows")
rownames(semisd.ret.matrix) <- c("Bull Eng.","Bull Har.","Hammer", "Pier. Line","Morn. Star", "3 White Soldiers")
View(semisd.ret.matrix)

#Combining single Signal Data Frame
signals <- cbind.data.frame(signal_1,signal_2,signal_3,signal_4,signal_5,signal_6,
                             signal_7,signal_8,signal_9,signal_10,signal_11,signal_12,
                             signal_13,signal_14,signal_15,signal_16,signal_17,signal_18,
                             signal_19,signal_20,signal_21,signal_22,signal_23,signal_24,
                             signal_25,signal_26,signal_27,signal_28,signal_29,signal_30,
                             signal_31,signal_32,signal_33,signal_34,signal_35,signal_36)
#(signals)

sig_patt_total <- signals%>%
  mutate("Total Sum - Signals"=rowSums(.))
View(sig_patt_total)


#### Choose the best Trading Strategy and re-run the specific code to reset signal 
chartSeries(AAPL,
            subset="2021-01::2022-04",
            theme=chartTheme('white'))
addTA(signal,type='S',col='red')

