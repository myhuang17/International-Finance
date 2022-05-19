library(quantmod)
library(forecast)

#PART-0 COLLECT DATA===============================================================
# World market indices: https://finance.yahoo.com/world-indices/
indexnames=c("^KS11","^JKSE","^N225","^TWII","^HSI","^STI","^DJI",
             "^BVSP","^FTSE","^FCHI","^GDAXI","^GSPC","^BFX","^MXX")
getSymbols(indexnames,from="2010-01-01", to="2022-05-13", adjust=TRUE)
indexdata = cbind(KS11$KS11.Close,JKSE$JKSE.Close,N225$N225.Close,
                  TWII$TWII.Close,STI$STI.Close,DJI$DJI.Close,
                  BVSP$BVSP.Close,FTSE$FTSE.Close,FCHI$FCHI.Close,
                  GDAXI$GDAXI.Close,GSPC$GSPC.Close,BFX$BFX.Close,MXX$MXX.Close
                  )
write.csv(indexdata,"C:/Users/user/Desktop/國際財管作業/data/index.CSV")

#EXCHANGE RATE IN QUANTMOD
curr_names=c("KRWHKD=X","IDRHKD=X","JPYHKD=X",
             "TWDHKD=X","SGDHKD=X","HKD=X","BRLHKD=X",
             "GBPHKD=X","EURHKD=X","MXNHKD=X")
getSymbols(curr_names,from="2010-01-01", to="2022-05-13", adjust=TRUE)
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
for(i in 1:2347){
  KRW[i] = KRW[i]/100
}
for(i in 1:91){
  IDR[i] = IDR[i]/100
}
erdata = cbind(KRW, IDR, JPY, TWD, SGD, USD, BRL, GBP, EUR, EUR, USD, EUR, MXN)
write.csv(erdata,"C:/Users/user/Desktop/國際財管作業/data/exchangerate.csv")


#PART1-1 SHARPE RATIO===============================================================
#Sharpe Ratio for each Country
sharpe1 = function(a,b){
  ret = diff(log(a)) + diff(log(b))
  ret = ret[complete.cases(ret)]#刪除遺失值
  return(ret)
}#對數取差分而已
sharpe = function(a,b){
  ret = diff(log(a)) + diff(log(b))
  ret = ret[complete.cases(ret)]#刪除遺失值
  sr = mean(ret)/sd(ret)
  return(sr)
}#夏普值
#SHARPE HK
hsi_d = diff(log(HSI$HSI.Close))
hk_sr = mean(hsi_d[complete.cases(hsi_d)])/sd(hsi_d[complete.cases(hsi_d)])

fc_sr = NULL
for (i in 1:13) {
  fc_sr[i] = sharpe(indexdata[,i], erdata[,i])
}
one = rbind(hk_sr, fc_sr)
two = fc_sr/hk_sr#相關係數小於這數字都可以列入資產裡面

#PART1-2 SHARPE INEQUATION===============================================================
#因為各國休市問題，要比較的話要另外開(計算相關係數)
corre = NULL# Correlation for each market
fc_sr2 = NULL#Sharpe Ratio
res = NULL
result = NULL#Sharpe Inequation
for(i in 1:13){
  comp = cbind(indexdata[, i], erdata[, i], HSI$HSI.Close)
  comp = comp[complete.cases(comp)]
  hsi_d2 = diff(log(comp[,3]))
  hsi_d2 = hsi_d2[complete.cases(hsi_d2)]
  fore = sharpe1(comp[,1], comp[,2])
  corre[i] = cor(fore, hsi_d2)
  fc_sr2[i] = sharpe(comp[,1], comp[,2])
  hk_sr2 = mean(hsi_d2)/sd(hsi_d2)
  res = cbind(fc_sr2[i], corre[i] * hk_sr2, fc_sr2[i] > corre[i] * hk_sr2)
  result = rbind(result, res)
}
head(comp)#原始比較資料
head(hsi_d2)#恒生完整比較資料
corre#相關係數
hk_sr 
fc_sr2#各國閜普值
hk_sr2#香港夏普值
result#結果[外國夏普值, 相關係數*恒生夏普值, 是否加入資產配置]
write.csv(result,"C:/Users/user/Desktop/國際財管作業/result.csv")

#PART 2 組合演算法===============================================================
p2 = result[,1] / result[,2]#sort and the biggest one should be the first one
sortmarket = order(p2, decreasing = TRUE)

USES = NULL
x=0
for(i in sortmarket){
  x = x + 1
  comp = cbind(indexdata[, i], erdata[, i], HSI$HSI.Close)
  comp = comp[complete.cases(comp)]
  hsi_d3 = diff(log(comp[,3]))
  hsi_d3 = hsi_d3[complete.cases(hsi_d3)]
  if(x==1){
    oldfore = hsi_d3
    newratio = hk_sr2
  }
  if(fc_sr2[i] > corre[i] * newratio){
    fore = sharpe1(comp[,1], comp[,2])
    newratio = sharpe(fore, oldfore)
    oldfore = sharpe1(fore, oldfore)
    USES[i] = 1
  }else{
    USES[i] = 0
  }
}
