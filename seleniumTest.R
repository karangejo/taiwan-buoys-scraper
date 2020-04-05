# To run this script:
# update R to version 3.6
# install docker libxml2-dev libopenssl-dev curl licurl-openssl-dev and maybe some other dependencies for RSelenium
# sudo apt install docker.io
# then run sudo docker pull selenium/standalone-chrome to pull the selenium docker image
# finally run sudo docker run -d -p 4445:4444 selenium/standalone-chrome to start the container
# run docker as a service 
library(RSelenium)
library(devtools)
library(htmltab)

# activate the selenium driver and connect it to the running docker image
remote_driver <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
remote_driver$open()
remote_driver$navigate("https://www.cwb.gov.tw/V8/E/M/OBS_Marine_30day.html?MID=WRA007")
page <- remote_driver$getPageSource()
datasetNew = htmltab(page[[1]])

# function to extract a table of data that is loaded dynamically 
loadJavascriptExtractTableToDF <- function(url) {
  remote_driver$navigate(url)
  # scroll to the bottom
  webElem <- remote_driver$findElement("css", "body")
  webElem$sendKeysToElement(list(key = "end"))
  page <- remote_driver$getPageSource()
  dataset = htmltab(page[[1]])
  return(dataset)
}

#df <- loadJavascriptExtractTableToDF("https://www.cwb.gov.tw/V8/E/M/OBS_Marine_30day.html?MID=WRA007")


