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

baseDate <- as.POSIXct("1970-01-01T00:00:00.000Z", tz = "GMT")
quakes <- distinct(quakeFrame, ids, .keep_all = TRUE) %>%
  mutate(date = baseDate + time / 1000, # time is milliseconds since the epoch
         cleanDate = make_date(year(date), month(date), day(date))) %>%
  group_by(cleanDate) %>%
  summarize(qCount = n())

dateSeq <- data.frame(date = seq.Date(from = min(quakes$cleanDate), 
                    to = max(quakes$cleanDate),
                    by = "days"))

quakes <- left_join(dateSeq, quakes, by = c("date" = "cleanDate"))
quakes$qCount[is.na(quakes$qCount)] <- 0
quakeTS <- ts(quakes$qCount)

# THIS IS THE END OF THE DATA DOWNLOAD IF YOU ONLY WANT THE DATA

# Build a Time Series Model -----------------------------------------------

# poisson time series model
pModel <- tsglm(quakeTS, model = list(past_obs = 1:10),
                                      link = "identity",
                                      distr = "poisson")

# current total
currentTotal <- filter(quakes, year(date) == 2018, month(date) == 3) %>%
  summarize(tot = sum(qCount))

# simulate remaining days
simDays <- as.numeric(as.Date("2018-03-31") - max(quakes$date))

simVec <- as.matrix(rep(simDays, 10000))
modelSim <- apply(simVec, 1, pSim, mdl = pModel) + as.numeric(currentTotal)

# break points for distribution
brks <- c(0, 101, 140, 174, 213, 1000)
modelDens <- hist(modelSim, breaks = brks, plot = FALSE)
modelProbs <- modelDens$count / 10000

# Historical Periods to Compare Distribution -----------------------

historicalSum <- mutate(quakes, m = month(date), y = year(date)) %>%
  group_by(m, y) %>%
  summarize(totalQ = sum(qCount))
historicalDens <- hist(historicalSum$totalQ, breaks = brks, plot = FALSE)
historicalProbs <- historicalDens$count / nrow(historicalSum)


# Arbitrary Periods -------------------------------------------------------

endPoints <- sample((simDays + 1):nrow(quakes), 100000, replace = TRUE)
sampleTotals <- apply(as.matrix(endPoints), 1, sampleTotal, 
                      fullData = quakes$qCount, dayCount = simDays)

sampleTotals <- sampleTotals + as.numeric(currentTotal)

sampleDens <- hist(sampleTotals, breaks = brks, plot = FALSE)
sampleProbs <- sampleDens$count / length(sampleTotals)


# Comparison --------------------------------------------------------------

compFrame <- data.frame(Threshold = c("Less than 101", "101 to 140", "140 to 174",
                                      "174 to 213", "More than 213"),
                        Historical = percent(historicalProbs),
                        Model = percent(modelProbs),
                        RandomSample = percent(sampleProbs))


# Distribution By Day -----------------------------------------------------
quakes <- ungroup(quakes) %>%
  mutate(d = day(date), m = month(date), y = year(date)) %>%
  group_by(m, y) %>%
  mutate(runSum = order_by(d, cumsum(qCount)))
  
dayStats <- ungroup(quakes) %>%
  group_by(d) %>%
  summarize(med = median(runSum),
            avg = mean(runSum),
            q25 = quantile(runSum, probs = 0.25),
            q75 = quantile(runSum, probs = 0.75))

pt <- ggplot(quakes, aes(d, runSum)) +
  geom_point() +
  geom_line(data = dayStats, aes(d, med), color = "red")




  
                        
