setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(timeSeries)
library(statcomp)
library(zoo)
library(rLakeAnalyzer)
library(lubridate)
library(pracma)
library(broom)

# Package ID: knb-lter-ntl.29.30 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Physical Limnology of Primary Study Lakes 1981 - current.
# Data set creator:  John Magnuson - University of Wisconsin
# Data set creator:  Stephen Carpenter - University of Wisconsin
# Data set creator:  Emily Stanley - University of Wisconsin
# Contact:  NTL Information Manager -  University of Wisconsin  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/30/03e232a1b362900e0f059859abe8eb97"
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F
               ,skip=1
               ,sep=","
               ,quot='"'
               , col.names=c(
                 "lakeid",
                 "year4",
                 "daynum",
                 "sampledate",
                 "depth",
                 "rep",
                 "sta",
                 "event",
                 "wtemp",
                 "o2",
                 "o2sat",
                 "deck",
                 "light",
                 "frlight",
                 "flagdepth",
                 "flagwtemp",
                 "flago2",
                 "flago2sat",
                 "flagdeck",
                 "flaglight",
                 "flagfrlight"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}
rm(tmpDateFormat,tmp1sampledate)
if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$rep)!="factor") dt1$rep<- as.factor(dt1$rep)
if (class(dt1$sta)!="factor") dt1$sta<- as.factor(dt1$sta)
if (class(dt1$event)!="factor") dt1$event<- as.factor(dt1$event)
if (class(dt1$wtemp)=="factor") dt1$wtemp <-as.numeric(levels(dt1$wtemp))[as.integer(dt1$wtemp) ]
if (class(dt1$wtemp)=="character") dt1$wtemp <-as.numeric(dt1$wtemp)
if (class(dt1$o2)=="factor") dt1$o2 <-as.numeric(levels(dt1$o2))[as.integer(dt1$o2) ]
if (class(dt1$o2)=="character") dt1$o2 <-as.numeric(dt1$o2)
if (class(dt1$o2sat)=="factor") dt1$o2sat <-as.numeric(levels(dt1$o2sat))[as.integer(dt1$o2sat) ]
if (class(dt1$o2sat)=="character") dt1$o2sat <-as.numeric(dt1$o2sat)
if (class(dt1$deck)=="factor") dt1$deck <-as.numeric(levels(dt1$deck))[as.integer(dt1$deck) ]
if (class(dt1$deck)=="character") dt1$deck <-as.numeric(dt1$deck)
if (class(dt1$light)=="factor") dt1$light <-as.numeric(levels(dt1$light))[as.integer(dt1$light) ]
if (class(dt1$light)=="character") dt1$light <-as.numeric(dt1$light)
if (class(dt1$frlight)!="factor") dt1$frlight<- as.factor(dt1$frlight)
if (class(dt1$flagdepth)!="factor") dt1$flagdepth<- as.factor(dt1$flagdepth)
if (class(dt1$flagwtemp)!="factor") dt1$flagwtemp<- as.factor(dt1$flagwtemp)
if (class(dt1$flago2)!="factor") dt1$flago2<- as.factor(dt1$flago2)
if (class(dt1$flago2sat)!="factor") dt1$flago2sat<- as.factor(dt1$flago2sat)
if (class(dt1$flagdeck)!="factor") dt1$flagdeck<- as.factor(dt1$flagdeck)
if (class(dt1$flaglight)!="factor") dt1$flaglight<- as.factor(dt1$flaglight)
if (class(dt1$flagfrlight)!="factor") dt1$flagfrlight<- as.factor(dt1$flagfrlight)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt1)
attach(dt1)
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.

summary(lakeid)
summary(year4)
summary(daynum)
summary(sampledate)
summary(depth)
summary(rep)
summary(sta)
summary(event)
summary(wtemp)
summary(o2)
summary(o2sat)
summary(deck)
summary(light)
summary(frlight)
summary(flagdepth)
summary(flagwtemp)
summary(flago2)
summary(flago2sat)
summary(flagdeck)
summary(flaglight)
summary(flagfrlight)
# Get more details on character variables

summary(as.factor(dt1$lakeid))
summary(as.factor(dt1$rep))
summary(as.factor(dt1$sta))
summary(as.factor(dt1$event))
summary(as.factor(dt1$frlight))
summary(as.factor(dt1$flagdepth))
summary(as.factor(dt1$flagwtemp))
summary(as.factor(dt1$flago2))
summary(as.factor(dt1$flago2sat))
summary(as.factor(dt1$flagdeck))
summary(as.factor(dt1$flaglight))
summary(as.factor(dt1$flagfrlight))
detach(dt1)

df.livingstone = data.frame('year' = NULL,
                            'depth' = NULL,
                            'jz' = NULL,
                            'alphaz' = NULL,
                            'id' = NULL)
coeff = data.frame('year' = NULL,
                   'Jz' = NULL,
                   'Jv' = NULL,
                   'Ja' = NULL,
                   'id' = NULL)

hypso <- read_csv('../data/LakeEnsemblR_bathymetry_standard.csv')
H <- hypso$Depth_meter
A <- hypso$Area_meterSquared

df <- dt1 %>%
  dplyr::filter(lakeid == 'ME') %>%
  select(year4, sampledate, depth, wtemp, o2, flago2)

summary(as.factor(df$depth))

if (23 > max(H)){
  H <- c(23, H)
  A <- c(min(A), A)
}

# calculate the q factor for alpha
if (length(A) <= 2){
  areas <- approx(H, A, seq(max(H), min(A),-1))$y
  depths <- seq(max(H), min(H),-1)
} else {
  areas = A
  depths = H
}


fit_q <- function(x, areas, depths){

  pred_areas <- max(areas) * (1 - depths/max(depths))^x
  fit <- sqrt(sum((areas - pred_areas)^2)/length(areas))
  return(fit)
}
# use Brent method to fit q
q <- optim(par = 0.1, fn = fit_q, areas = areas, depths = depths, method = 'Brent', lower = 0.5, upper = 2.0)

# visual check
plot(depths, areas)
lines(depths, max(areas) * (1 - depths/max(depths))^q$par, col = 'red')

for (id.year in unique(df$year4)){
  obs <- df %>%
    dplyr::filter(year4 == id.year)

  dat1 <- matrix(NA, nrow = length(seq(0, 23, 0.5)), ncol = length(unique(obs$sampledate)))
  ph1 <- matrix(NA, nrow = 3, ncol = length(unique(obs$sampledate)))
  for (i in unique(obs$sampledate)){

    obs.dt <- obs %>%
      dplyr::filter(sampledate == i)


    if(length(na.omit(obs.dt$o2)) < 2){
      next
    }

    dat1[, match(i, unique(obs$sampledate))] <- approx(obs.dt$depth,
                                                       obs.dt$o2,
                                                       seq(0, 23, 0.5), rule = 2)$y
    ph1[1, match(i, unique(obs$sampledate))] <- thermo.depth(approx(obs.dt$depth,
                                                                    obs.dt$wtemp,
                                                                    seq(0, 23, 0.5), rule = 2)$y, seq(0, 23, 0.5))
    ph1[2, match(i, unique(obs$sampledate))] <- obs.dt$wtemp[which.min(obs.dt$depth)] - obs.dt$wtemp[which.max(obs.dt$depth)]
    ph1[3, match(i, unique(obs$sampledate))] <- center.buoyancy(approx(obs.dt$depth,
                                                                    obs.dt$wtemp,
                                                                    seq(0, 23, 0.5), rule = 2)$y, seq(0, 23, 0.5))
  }
  if(yday(unique(obs$sampledate))[which.max(colSums(dat1))] < 250){
    max.date <- which.max(colSums(dat1))
  } else {
    max.date <- which.max(colSums(dat1[, which(yday(unique(obs$sampledate)) < 250)]))
  }

  therm.dep <- ceiling(mean(ph1[1,], na.rm = T))

  dat2 <- dat1[which(seq(0, 23, 0.5) == therm.dep) : nrow(dat1), max.date:ncol(dat1)]
  deps <- seq(0,23,0.5)[which(seq(0, 23, 0.5) == therm.dep) : nrow(dat1)]
  times <- yday(unique(obs$sampledate)[max.date:ncol(dat1)])

  for (j in 1:nrow(dat2)){

    start = dat2[j,1]
    end = dat2[j, which(dat2[1,] < 1)[1]]

    start.time = times[1]
    end.time = times[which(dat2[1,] < 1)[1]]

    if (all(dat2[j,] > 1)){
      next
    }

    dat3 <- interp1(times[1:which(dat2[j,] <= 1)[1]],
                    dat2[j, 1:which(dat2[j,] <= 1)[1]],
                    xi = seq(times[1], times[which(dat2[j,] <= 1)[1]], 1),
                    method = 'linear')

    times.in <- seq(times[1], times[which(dat2[j,] <= 1)[1]], 1)

    df.livingstone <- rbind(df.livingstone, data.frame('year' = id.year,
               'depth' = deps[j],
               'jz' = (dat3[1] - dat3[which(dat3 <= 1)[1]]) / (times.in[1] - times.in[which(dat3 <= 1)[1]]),
               'alphaz' = q$par/(max(deps)+1 - deps[j]),
               'id' = 'ME'))

    ggplot(subset(df.livingstone, year == id.year)) +
      geom_point(aes(alphaz, jz)) +
      scale_x_continuous(trans='log10') +
      theme_bw()
  }
}

for (l in unique(df.livingstone$year)){
  dit.l <- df.livingstone %>%
    dplyr::filter(year == l)

  sum.mod <- lm(abs(jz) ~ alphaz, data = dit.l)
  p  <-summary(sum.mod)$coefficients[,"Pr(>|t|)"][2]

  if (!is.na(p) && p <= 0.05){
    coeff = rbind(coeff, data.frame('year' = l,
                                    'Jz' = abs(mean(dit.l$jz, na.rm = T)),
                                    'Jv' = sum.mod$coefficients[1],
                                    'Ja' = sum.mod$coefficients[2],
                                    'id' = "ME"))
  } else {
    coeff = rbind(coeff, data.frame('year' = l,
                                    'Jz' = abs(mean(dit.l$jz, na.rm = T)),
                                    'Jv' = NA,
                                    'Ja' = NA,
                                    'id' = "ME"))
  }

}

ggplot(subset(coeff, year != 2007 & year != 2018), aes(year, Jz, col = 'Volume')) +
  geom_line(aes(year, Jv, col = 'Volume')) +
  geom_line(aes(year, Ja, col = 'Sediment')) +
  geom_line(aes(year, Jz, col = 'Total Sink')) +
  geom_point(aes(year, Jv, col = 'Volume')) +
  geom_point(aes(year, Ja, col = 'Sediment')) +
  geom_point(aes(year, Jz, col = 'Total Sink')) +
  geom_smooth(method = "loess", size = 1.5) +
  ylab("Oxygen flux in g/m3/d") + xlab('') +
  theme_bw()
ggsave('../figs/livingstone_fluxes.png', dpi = 300, units = 'in', width = 5, height = 3)
