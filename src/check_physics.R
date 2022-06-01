setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(timeSeries)
library(statcomp)
library(zoo)
library(rLakeAnalyzer)
library(lubridate)
library(pracma)
library(broom)
library(ggpmisc)
library(patchwork)
library(stringr)
# Package ID: knb-lter-ntl.130.29 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: High Frequency Water Temperature Data - Lake  Mendota Buoy 2006 - current.
# Data set creator:  NTL Lead PI - University of Wisconsin
# Data set creator:  John Magnuson - University of Wisconsin
# Data set creator:  Stephen Carpenter - University of Wisconsin
# Data set creator:  Emily Stanley - University of Wisconsin
# Metadata Provider:  NTL Information Manager - University of Wisconsin
# Contact:  NTL Information Manager -  University of Wisconsin  - ntl.infomgr@gmail.com
# Contact:  NTL Lead PI -  University of Wisconsin  - ntl.leadpi@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/130/29/a5919fc36f07fcf99765f084d18f5174"
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F
               ,skip=1
               ,sep=","
               ,quot='"'
               , col.names=c(
                 "sampledate",
                 "year4",
                 "month",
                 "daynum",
                 "depth",
                 "wtemp",
                 "flag_wtemp"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}
rm(tmpDateFormat,tmp1sampledate)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)
if (class(dt1$month)=="factor") dt1$month <-as.numeric(levels(dt1$month))[as.integer(dt1$month) ]
if (class(dt1$month)=="character") dt1$month <-as.numeric(dt1$month)
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)
if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$wtemp)=="factor") dt1$wtemp <-as.numeric(levels(dt1$wtemp))[as.integer(dt1$wtemp) ]
if (class(dt1$wtemp)=="character") dt1$wtemp <-as.numeric(dt1$wtemp)
if (class(dt1$flag_wtemp)!="factor") dt1$flag_wtemp<- as.factor(dt1$flag_wtemp)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt1)
attach(dt1)
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.

summary(sampledate)
summary(year4)
summary(month)
summary(daynum)
summary(depth)
summary(wtemp)
summary(flag_wtemp)
# Get more details on character variables

summary(as.factor(dt1$flag_wtemp))
detach(dt1)
dt2= dt1
head(dt2)

# dt2$datetime <- (paste0(dt2$sampledate,' ',dt2$hour,':00:00'))
df = dt2 %>%
  rename(datetime = sampledate) %>%
  select(datetime, depth, wtemp)

data_reshape1 <- reshape(df,                                 # Applying reshape function
                         idvar = "datetime",
                         timevar = "depth",
                         direction = "wide")

colnames(data_reshape1) = c('datetime',paste0("wtr_",str_extract(colnames(data_reshape1)[2:ncol(data_reshape1)], "\\d+([.,]\\d+)?")))

hypso <- read_csv('../data/LakeEnsemblR_bathymetry_standard.csv')
H <- hypso$Depth_meter
A <- hypso$Area_meterSquared

areas <- approx(H, A, unique(df$depth))$y
depths = unique(df$depth)
bath = data.frame('depths' = depths, 'areas' = areas)

df.ph <- data.frame(
  'time' = data_reshape1$datetime,
  'ssi' = ts.schmidt.stability(wtr = data_reshape1,
                               bathy = bath, na.rm = T),
  'tempgrad' = data_reshape1$wtr_0 - data_reshape1$wtr_20,
  'year' = year( data_reshape1$datetime),
  'doy' = yday( data_reshape1$datetime))

df.duration <- data.frame('year' = NULL,
                          'dur' = NULL)
for (i in unique(df.ph$year)[-1]){
  df.year <- df.ph %>%
    dplyr::filter(year == i
                  )
  schm <- ifelse(df.year$ssi.schmidt.stability > 0, 1, NA)
  tgrad <- ifelse(df.year$tempgrad > 1, 1, NA)

  length(na.contiguous(schm))
  length(na.contiguous(tgrad))

  df.duration <- rbind(df.duration,
                       data.frame('year' = i,
                                  'dur' = (length(na.contiguous(schm)) + length(na.contiguous(tgrad)))/2))
}

ggplot(df.ph) +
  geom_line(aes(time, ssi.schmidt.stability)) +
ggplot(df.ph) +
  geom_line(aes(time, tempgrad)) +
ggplot(df.duration) +
  geom_line(aes(year, dur))

