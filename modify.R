library(quantmod)
library(forecast)
# World market indices: https://finance.yahoo.com/world-indices/
indexnames=c("^KS11","^JKSE","^N225","^TWII","^HSI","^STI","^DJI",
             "^BVSP","^FTSE","^FCHI","^GDAXI","^GSPC","^BFX","^MXX")
getSymbols(indexnames,from="2010-01-01", adjust=TRUE)
indexdata = cbind(KS11$KS11.Close,JKSE$JKSE.Close,N225$N225.Close,
                  TWII$TWII.Close,HSI$HSI.Close,DJI$DJI.Close,
                  BVSP$BVSP.Close,FTSE$FTSE.Close,FCHI$FCHI.Close,
                  GDAXI$GDAXI.Close,GSPC$GSPC.Close,BFX$BFX.Close,MXX$MXX.Close
                  )
write.csv(indexdata,"C:/Users/user/Desktop/國際財管作業/data/index.CSV")

#EXCHANGE RATE IN QUANTILE
curr_names=c("KRWHKD=X","IDRHKD=X","JPYHKD=X",
             "TWDHKD=X","SGDHKD=X","HKD=X","BRLHKD=X",
             "GBPHKD=X","EURHKD=X","MXNHKD=X")
getSymbols(curr_names,from="2010-01-01", adjust=TRUE)
KRW = `KRWHKD=X`[,4]
IDR = `IDRHKD=X`[,4]
JPY = `JPYHKD=X`[,4]
TWD = `TWDHKD=X`[,4]
SGD = `SGDHKD=X`[,4]
USD = `HKD=X`[,4]
BRL = `BRLHKD=X`[,4]
GBP = `GBPHKD=X`[,4]
EUR = `EURHKD=X`[,4]
MXN = `MXNHKD=X`[,4]
erdata = cbind(KRW, IDR, JPY, TWD, SGD, USD, BRL, GBP, EUR, EUR, USD, EUR, MXN)
write.csv(erdata,"C:/Users/user/Desktop/國際財管作業/data/exchangerate.csv")

#Sharpe Ratio for each Country
sharpe1 = function(a,b){
  ret = diff(log(a)) + diff(log(b))
  ret = ret[complete.cases(ret)]#刪除遺失值
  return(ret)
}
sharpe = function(a,b){
  ret = diff(log(a)) + diff(log(b))
  ret = ret[complete.cases(ret)]#刪除遺失值
  sr = mean(ret)/sd(ret)
  return(sr)
}
hsi_d = diff(log(HSI$HSI.Close))
hk_sr = mean(hsi_d[complete.cases(hsi_d)])/sd(hsi_d[complete.cases(hsi_d)])
fc_sr = NULL
corre = NULL
for (i in 1:13) {
  fc_sr[i] = sharpe(indexdata[,i], erdata[,i])
  #相關係數算不出來
  #corre[i] = cor(sharpe1(indexdata[,i], erdata[,i]), hsi_d[complete.cases(hsi_d)])
}
fc_sr/hk_sr#相關係數小於這數字都可以列入資產裡面

#因為各國休市問題，要比較的話要另外開(計算相關係數)
korhk = 1

