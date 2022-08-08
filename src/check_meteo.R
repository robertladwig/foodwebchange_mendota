setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)

# Package ID: knb-lter-ntl.20.34 Cataloging System:https://pasta.edirepository.org.
# Data set title: Madison Wisconsin Daily Meteorological Data 1869 - current.
# Data set creator:  Lyle Anderson -  
# Data set creator:  Dale Robertson -  
# Data set creator:  NOAA National Weather Service -  
# Contact:  NTL Information Manager -  University of Wisconsin  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/20/34/3c7ddd692d3ac8e90bf2954a16b39e89" 
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
                 "max_air_temp_adjusted",     
                 "max_air_temp_raw",     
                 "min_air_temp_adjusted",     
                 "min_air_temp_raw",     
                 "ave_air_temp_adjusted",     
                 "ave_air_temp_raw",     
                 "range_air_temp_adjusted",     
                 "precip_raw_mm",     
                 "snow_raw_cm",     
                 "snow_depth_cm",     
                 "data_status"    ), check.names=TRUE)

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
if (class(dt1$max_air_temp_adjusted)=="factor") dt1$max_air_temp_adjusted <-as.numeric(levels(dt1$max_air_temp_adjusted))[as.integer(dt1$max_air_temp_adjusted) ]               
if (class(dt1$max_air_temp_adjusted)=="character") dt1$max_air_temp_adjusted <-as.numeric(dt1$max_air_temp_adjusted)
if (class(dt1$max_air_temp_raw)=="factor") dt1$max_air_temp_raw <-as.numeric(levels(dt1$max_air_temp_raw))[as.integer(dt1$max_air_temp_raw) ]               
if (class(dt1$max_air_temp_raw)=="character") dt1$max_air_temp_raw <-as.numeric(dt1$max_air_temp_raw)
if (class(dt1$min_air_temp_adjusted)=="factor") dt1$min_air_temp_adjusted <-as.numeric(levels(dt1$min_air_temp_adjusted))[as.integer(dt1$min_air_temp_adjusted) ]               
if (class(dt1$min_air_temp_adjusted)=="character") dt1$min_air_temp_adjusted <-as.numeric(dt1$min_air_temp_adjusted)
if (class(dt1$min_air_temp_raw)=="factor") dt1$min_air_temp_raw <-as.numeric(levels(dt1$min_air_temp_raw))[as.integer(dt1$min_air_temp_raw) ]               
if (class(dt1$min_air_temp_raw)=="character") dt1$min_air_temp_raw <-as.numeric(dt1$min_air_temp_raw)
if (class(dt1$ave_air_temp_adjusted)=="factor") dt1$ave_air_temp_adjusted <-as.numeric(levels(dt1$ave_air_temp_adjusted))[as.integer(dt1$ave_air_temp_adjusted) ]               
if (class(dt1$ave_air_temp_adjusted)=="character") dt1$ave_air_temp_adjusted <-as.numeric(dt1$ave_air_temp_adjusted)
if (class(dt1$ave_air_temp_raw)=="factor") dt1$ave_air_temp_raw <-as.numeric(levels(dt1$ave_air_temp_raw))[as.integer(dt1$ave_air_temp_raw) ]               
if (class(dt1$ave_air_temp_raw)=="character") dt1$ave_air_temp_raw <-as.numeric(dt1$ave_air_temp_raw)
if (class(dt1$range_air_temp_adjusted)=="factor") dt1$range_air_temp_adjusted <-as.numeric(levels(dt1$range_air_temp_adjusted))[as.integer(dt1$range_air_temp_adjusted) ]               
if (class(dt1$range_air_temp_adjusted)=="character") dt1$range_air_temp_adjusted <-as.numeric(dt1$range_air_temp_adjusted)
if (class(dt1$precip_raw_mm)=="factor") dt1$precip_raw_mm <-as.numeric(levels(dt1$precip_raw_mm))[as.integer(dt1$precip_raw_mm) ]               
if (class(dt1$precip_raw_mm)=="character") dt1$precip_raw_mm <-as.numeric(dt1$precip_raw_mm)
if (class(dt1$snow_raw_cm)=="factor") dt1$snow_raw_cm <-as.numeric(levels(dt1$snow_raw_cm))[as.integer(dt1$snow_raw_cm) ]               
if (class(dt1$snow_raw_cm)=="character") dt1$snow_raw_cm <-as.numeric(dt1$snow_raw_cm)
if (class(dt1$snow_depth_cm)=="factor") dt1$snow_depth_cm <-as.numeric(levels(dt1$snow_depth_cm))[as.integer(dt1$snow_depth_cm) ]               
if (class(dt1$snow_depth_cm)=="character") dt1$snow_depth_cm <-as.numeric(dt1$snow_depth_cm)
if (class(dt1$data_status)!="factor") dt1$data_status<- as.factor(dt1$data_status)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(sampledate)
summary(year4)
summary(month)
summary(daynum)
summary(max_air_temp_adjusted)
summary(max_air_temp_raw)
summary(min_air_temp_adjusted)
summary(min_air_temp_raw)
summary(ave_air_temp_adjusted)
summary(ave_air_temp_raw)
summary(range_air_temp_adjusted)
summary(precip_raw_mm)
summary(snow_raw_cm)
summary(snow_depth_cm)
summary(data_status) 
# Get more details on character variables

summary(as.factor(dt1$data_status))
detach(dt1)               

ggplot(dt1) + 
  geom_line(aes(sampledate, ave_air_temp_adjusted)) +
  geom_point(aes(sampledate, ave_air_temp_adjusted))



