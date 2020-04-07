
library(XML)
library(htmltab)
library(zoo)
library(data.table)
library(gridExtra)
library(jsonlite)
library(lubridate)




GetTideData <- function(tideUrl){
  tideData <- htmltab(tideUrl,rm_nodata_cols = FALSE, rm_nodata_rows = FALSE)
  tideData <- transpose(tideData)
  colnames(tideData) <- as.character(unlist(tideData[1,]))
  tideData <- tideData[-1,]
  tideData[,3] <- gsub("morn-ing","morning",tideData[,3])
  tideData[,3] <- gsub("after-noon","afternoon",tideData[,3])
  tideData[is.na(tideData)] <- "-"
  tideData$Time <- paste(tideData[,2],tideData[,3])
  tideData <- tideData[,-c(1:4)]
  if(ncol(tideData) < 16){
    tideData <- tideData[,-c(3:10)]
    tideData <- tideData[,c(5,1,2,3,4)]
    
    return(tideData)
    
  }else{
    tideData <- tideData[,-c(6:13)]
    tideData <- tideData[,c(8,1,2,6,7,3,4,5)]
    tideData[,7] <- as.numeric(tideData[,7])
    tideData[,8] <- as.numeric(tideData[,8])
    tideData[,6] <- as.factor(tideData[,6])
    tideData$Number <- as.numeric(c(1:nrow(tideData)))
    
    return(tideData)
  }
}

PredTaitung <- function(){
  TaitungTide = 'https://www.tide-forecast.com/locations/Taitung-City/forecasts/latest/six_day'
  TaitungTideData <- GetTideData(TaitungTide)
  # 台東潮 <- TaitungTideData
  dfjson <- toJSON(TaitungTideData)
  write(dfjson,file="./tideData/Taitung.JSON")
  # save(台東潮,file="台東潮.Rda")
}

PredHualien <- function(){
  HualienTide = 'https://www.tide-forecast.com/locations/Hualien-City/forecasts/latest/six_day'
  HualienTideData <- GetTideData(HualienTide)
  # 花蓮潮<- HualienTideData
  dfjson <- toJSON(HualienTideData)
  write(dfjson,file="./tideData/Hualien.JSON")
  # save(花蓮潮,file="花蓮潮.Rda")
}

PredYilan <- function(){
  ChiLungTide = 'https://www.tide-forecast.com/locations/Chi-Lung-Taiwan/forecasts/latest/six_day'
  YilanTideData <- GetTideData(ChiLungTide)
  # 宜蘭潮 <- YilanTideData
  dfjson <- toJSON(YilanTideData)
  write(dfjson,file="./tideData/Yilan.JSON")
  # save(宜蘭潮,file="宜蘭潮.Rda")
}

PredSuAo <- function(){
  ChiLungTide = 'https://www.tide-forecast.com/locations/Chi-Lung-Taiwan/forecasts/latest/six_day'
  SuAoTideData <- GetTideData(ChiLungTide)
  # 蘇澳潮 <- SuAoTideData
  dfjson <- toJSON(SuAoTideData)
  write(dfjson,file="./tideData/SuAo.JSON")
  # save(蘇澳潮,file="蘇澳潮.Rda")
}

PredXiaoLiuQiu <- function(){
  HengChunTide = 'https://www.tide-forecast.com/locations/Heng-ch-un/forecasts/latest/six_day'
  XiaoLiuQiuTideData <- GetTideData(HengChunTide)
  # 小琉球潮 <- XiaoLiuQiuTideData
  dfjson <- toJSON(XiaoLiuQiuTideData)
  write(dfjson,file="./tideData/XiaoLiuQiu.JSON")
  # save(小琉球潮,file="小琉球潮.Rda")
}

print(as.character(now("UTC")))

PredHualien()
PredSuAo()
PredTaitung()
PredXiaoLiuQiu()
PredYilan()