# To run this script:
# update R to version 3.6
# install docker libxml2-dev libopenssl-dev curl licurl-openssl-dev and maybe some other dependencies for RSelenium
# sudo apt install docker.io
# then run sudo docker pull selenium/standalone-chrome to pull the selenium docker image
# finally run sudo docker run -d -p 4445:4444 selenium/standalone-chrome to start the container
# run docker as a service 

# docker container must be restarted before running this script!!!

library(XML)
library(htmltab)
library(zoo)
library(data.table)
library(gridExtra)
library(jsonlite)
library(lubridate)
#library(devtools)
library(RSelenium)

# activate the selenium driver and connect it to the running docker image
remote_driver <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
remote_driver$open()

getCurrentTime <- function(){
  timesys <- as.character(now("UTC"))
  timesys <- gsub(":.*","",timesys)
  return(timesys)
}

removeDuplicateDirections <- function(string){
  if(nchar(string) != 1){
    return(substr(string, 0, nchar(string)/2))
  } else {
    return(string)
  }
}

loadJavascriptExtractTableToDF <- function(url) {
  remote_driver$navigate(url)
  # scroll to the bottom
  webElem <- remote_driver$findElement("css", "body")
  webElem$sendKeysToElement(list(key = "end"))
  #webElem$screenshot(display = TRUE)
  page <- remote_driver$getPageSource()
  dataset = htmltab(page[[1]])
  return(dataset)
}

GetBuoyData<- function(buoyUrl,tideUrl){
  #TaitungUrl = 'https://www.cwb.gov.tw/V8/E/M/OBS_Marine_30day.html?MID=WRA007'
  #FugangBuoyURL = 'https://www.cwb.gov.tw/V8/E/M/OBS_Marine_30day.html?MID=1586'
  #dataset <- loadJavascriptExtractTableToDF(TaitungUrl)
  #buoydata <- loadJavascriptExtractTableToDF(FugangBuoyURL)
  dataset <- loadJavascriptExtractTableToDF(buoyUrl)
  buoydata <- loadJavascriptExtractTableToDF(tideUrl)
  #dataset = htmltab(buoyPage)
  #buoydata = htmltab(tidePage)
  #print(head(dataset))
  #print(head(buoydata))
  # have to fix the processing pipeline 
  # th table columns have changed a bit
  # change the names of the columns
  dataset$DateTime = dataset[,c("Time")]
  #dataset$Time = dataset[,2]
  dataset$WaveHeight = dataset[,c("Wave Height(m)")]
  dataset$WaveDir = dataset[,c("Wave Dir(Max WH)")]
  dataset$WavePeriod = dataset[,"WP(sec)"]
  dataset$WindSpeed = dataset[,"Mean Wind Speed                   (m/s)       (BS)"]
  dataset$WindDirection = dataset[,"Wind Dir"]
  dataset$Tide = buoydata[,c("Tidal Height(m)")]
  
  # exclude the long names
  # dataset = dataset[c(-1:-17)]
  returnDataSet <- dataset[,c("DateTime","WaveHeight","WaveDir","WavePeriod","WindSpeed","WindDirection","Tide")]
  
  # Format the Date and time
  # Join the two columns for date and time
  #dataset$DateTime <- paste(dataset$Date, dataset$Time, sep=" ")
  # remove the words in parentheses
  returnDataSet$DateTime <- gsub("\\s*\\([^\\)]+\\)","",as.character(returnDataSet$DateTime))
  # format as POSIXct
  returnDataSet$DateTime <- as.POSIXct(returnDataSet$DateTime,format="%m/%d%H:%M")
  # get rid of the old date and time
  #dataset = dataset [c(-1,-2)]
  # convert characters to numeric type
  # clean up NAs
  cols_to_change = c("WaveHeight", "WavePeriod", "WindSpeed","Tide")
  for(i in cols_to_change){
    #Change to type numeric
    returnDataSet[,i] = as.numeric(returnDataSet[,i])
    #remove first NAs
    returnDataSet[,i] <- as.numeric(na.fill(returnDataSet[,i], c('extend',NA)))
    # aproximate all other NAs
    returnDataSet[,i] <- as.numeric(na.approx(returnDataSet[,i]))
  }
  
  returnDataSet[,c("WaveDir","WindDirection")] <- lapply(returnDataSet[,c("WaveDir","WindDirection")], removeDuplicateDirections)
  # subsetting dataset to last 3 days
  
  returnDataSet <- returnDataSet[1:72,]
  
  return(returnDataSet)
}


GetTaitung <- function(){
  TaitungUrl = 'https://www.cwb.gov.tw/V8/E/M/OBS_Marine_30day.html?MID=WRA007'
  FugangBuoyURL = 'https://www.cwb.gov.tw/V8/E/M/OBS_Marine_30day.html?MID=1586'
  TaitungData <- (GetBuoyData(TaitungUrl,FugangBuoyURL))
  #  台東浮標 <-TaitungData
  dfjson <- toJSON(TaitungData)
  write(dfjson,file="./buoyData/Taitung.JSON")
  #save(台東浮標,file="台東浮標.Rda")
  #return(TaitungData)
}

GetHualien <- function(){
  HualienUrl = 'https://www.cwb.gov.tw/V8/E/M/OBS_Marine_30day.html?MID=46699A'
  HualienBuoyURL = 'https://www.cwb.gov.tw/V8/E/M/OBS_Marine_30day.html?MID=C4T01'
  HualienData <- (GetBuoyData(HualienUrl,HualienBuoyURL))
  # 花蓮浮標 <-HualienData
  #save(HualienData,file="HualienBuoyData.Rda")
  dfjson <- toJSON(HualienData)
  write(dfjson,file="./buoyData/Hualien.JSON")
  #save(花蓮浮標,file="花蓮浮標.Rda")
}

GetYilan <- function(){
  YilanUrl = 'https://www.cwb.gov.tw/V8/E/M/OBS_Marine_30day.html?MID=46708A'
  YilanBuoy = 'https://www.cwb.gov.tw/V8/E/M/OBS_Marine_30day.html?MID=C4U02'
  YilanData <- (GetBuoyData(YilanUrl,YilanBuoy))
  # 宜蘭浮標 <- YilanData
  #save(YilanData,file="YilanBuoyData.Rda")
  dfjson <- toJSON(YilanData)
  write(dfjson,file="./buoyData/Yilan.JSON")
  # save(宜蘭浮標,file="宜蘭浮標.Rda")
}

GetSuAo <- function(){
  SuAoUrl = 'https://www.cwb.gov.tw/V8/E/M/OBS_Marine_30day.html?MID=46706A'
  SuAoBuoy = 'https://www.cwb.gov.tw/V8/E/M/OBS_Marine_30day.html?MID=C4U01'
  SuAoData <- (GetBuoyData(SuAoUrl,SuAoBuoy))
  # 蘇澳浮標 <- SuAoData
  #save(SuAoData,file="SuAoBuoyData.Rda")
  dfjson <- toJSON(SuAoData)
  write(dfjson,file="./buoyData/SuAo.JSON")
  # save(蘇澳浮標,file="蘇澳浮標.Rda")
}

GetXiaoLiuQiu <- function(){
  XiaoLiuQiuUrl = 'https://www.cwb.gov.tw/V8/E/M/OBS_Marine_30day.html?MID=46714D'
  XiaoLiuQiuBuoy = 'https://www.cwb.gov.tw/V8/E/M/OBS_Marine_30day.html?MID=1496'
  XiaoLiuQiuData <- (GetBuoyData(XiaoLiuQiuUrl,XiaoLiuQiuBuoy))
  # 小琉球浮標 <- XiaoLiuQiuData
  #save(XiaoLiuQiuData,file="XiaoLiuQiuBuoyData.Rda")
  dfjson <- toJSON(XiaoLiuQiuData)
  write(dfjson,file="./buoyData/XiaoLiuQiu.JSON")
  # save(小琉球浮標,file="小琉球浮標.Rda")
}

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

GetXiaoLiuQiu()
GetSuAo()
GetYilan()
GetHualien()
GetTaitung()
PredHualien()
PredSuAo()
PredTaitung()
PredXiaoLiuQiu()
PredYilan()