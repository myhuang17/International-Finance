library(quantmod)
library(tseries)
library(fPortfolio)
library(lubridate)
library(zoo)
#預設的資料
input_p = 500000
year_t = 5
num_stock = 10
# TIME
dt = c(as.Date(paste0(year(Sys.Date())-year_t,"-1-1")), today())
#取得資料
target = c("006208.TW","0051.TW","0057.TW","00661.TW","00639.TW","00646.TW",
           "00639.TW","00652.TW","00660.TW","00662.TW","00668.TW","00709.TW",
           "00830.TW","00885.TW","2373.TW","2002.TW","0056.TW","1101.TW")
finame = c("`006208.TW`","`0051.TW`","`0057.TW`","`00661.TW`","`00639.TW`","`00646.TW`",
           "`00639.TW`","`00652.TW`","`00660.TW`","`00662.TW`","`00668.TW`","`00709.TW`",
           "`00830.TW`","`00885.TW`","`2373.TW`","`2002.TW`","`0056.TW`","`1101.TW`")
getSymbols(target, from=dt[1], to=dt[2], adjust=TRUE)
#組合RET
RET = NULL
for(i in 1:length(finame)) {
  temp=eval(parse(text=finame[i]))#parse用於將字符類對象轉換為表達式類對象
  RET=cbind(RET,diff(log(temp[,4]))*100)
  print(finame[i])
}
dim(na.omit(RET))#dim獲取或設置指定矩陣;na.omit返回刪除NA後的向量RET
colnames(RET)=target
shp = sort(apply(na.omit(RET), 2, sharpe), decreasing = TRUE)
fin = names(shp[1:num_stock])
datas = NULL
for(i in 1:length(fin)) {
  temp=eval(parse(text=paste0("`",fin[i],"`")))#parse用於將字符類對象轉換為表達式類對象
  datas=cbind(datas,diff(log(temp[,4]))*100)
}
datas = as.timeSeries(na.omit(datas))
#效率曲線-tangency
cons = c('LongOnly')
spect = portfolioSpec()
setNFrontierPoints(spect) = 250
setSolver(spect) = "solveRquadprog"
frontier = portfolioFrontier(datas)
tailoredFrontierPlot(frontier)#前緣曲線圖
weightsPlot(frontier)#比例圖
weightedReturnsPlot(frontier)
x = equalWeightsPoints(frontier)#EWP
#frontierPoints(frontier)
result = efficientPortfolio(datas)#riskless point
tangencyPortfolio(datas)#tangency point
#QP1
spec1 = portfolioSpec()
setTargetReturn(spec1) = mean(apply(datas, 1, mean))
qp1 = efficientPortfolio(datas, spec = spec1)#****
weightedReturnsPlot(frontier)
#取得權重 計算資金與股數 QP1
qp1_pf = as.data.frame.list(qp1@portfolio@portfolio$weights)
colnames(qp1_pf) = fin
capital = qp1_pf*input_p#各股投入資金
shares = NULL
for(i in 1:length(fin)) {
  prices = 0
  temp=eval(parse(text=paste0("`",fin[i],"`")))#parse用於將字符類對象轉換為表達式類對象
  prices=temp[length(temp[,1]),4]
  shares[i] = as.double(capital[i]) / prices
}
shares = as.data.frame.list(shares)
colnames(shares) = fin
