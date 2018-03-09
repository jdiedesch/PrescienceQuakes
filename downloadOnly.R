library(jsonlite)
library(geojsonio)
library(RCurl)
library(stringr)
library(dplyr)
library(tidyr)
library(tscount)
library(scales)
library(ggplot2)

pSim <- function(nPeriods, mdl){
  modelSim <- tsglm.sim(n = nPeriods, fit = mdl)
  sum(modelSim$ts)
  
}

sampleTotal <- function(endPoint, fullData, dayCount) {
  s <- sum(fullData[(endPoint - dayCount):endPoint])
}


# Download and Clean Data -------------------------------------------------

currDate <- Sys.Date()

pathBase <- "https://earthquake.usgs.gov/fdsnws/event/1/query?"
retFormat <- paste0("format=geojson")
startTime <- "starttime=1960-01-01"
endTime <- paste0("endtime=", Sys.Date())
minMag <- "minmagnitude=6"

eqPath <- paste0(pathBase, retFormat, "&", startTime, "&", endTime, "&", minMag)

largeQuakes <- fromJSON(eqPath)
quakeFrame <- largeQuakes$features$properties

# USGS limits downloads, so need to download in sequence to get mag 5 quakes
magSeq <- seq(from = 5.0, to = 6.0, by = 0.05)

for (i in 1:(length(magSeq) - 1)){
  
  minMag <- paste0("minmagnitude=", magSeq[i])
  maxMag <- paste0("maxmagnitude=", magSeq[i + 1])
  eqPath <- paste0(pathBase, retFormat, "&", startTime, "&", endTime, "&", 
                   minMag, "&", maxMag)
  quakeQ <- fromJSON(eqPath)
  qFrame <- quakeQ$features$properties
  quakeFrame <- rbind(quakeFrame, qFrame)
}

# USGS uses UTC timecode
baseDate <- as.POSIXct("1970-01-01T00:00:00.000Z", tz = "GMT")
quakes <- distinct(quakeFrame, ids, .keep_all = TRUE) %>%
  mutate(date = baseDate + time / 1000, # time is milliseconds since the epoch
         cleanDate = make_date(year(date), month(date), day(date))) %>%
  group_by(cleanDate) %>%
  summarize(qCount = n())

# need to fill in days that have 0 quakes
dateSeq <- data.frame(date = seq.Date(from = min(quakes$cleanDate), 
                                      to = max(quakes$cleanDate),
                                      by = "days"))

# combine the data with the full set of dates
quakes <- left_join(dateSeq, quakes, by = c("date" = "cleanDate"))

# convert NAs from USGS data (the days that have no quakes) to 0
quakes$qCount[is.na(quakes$qCount)] <- 0

# make a time series (convenient for looking at things like Pacf)
quakeTS <- ts(quakes$qCount)