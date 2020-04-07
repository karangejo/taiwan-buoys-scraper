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


print(as.character(now("UTC")))


GetHualien()
