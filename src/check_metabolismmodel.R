setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(timeSeries)
library(statcomp)
library(zoo)

df <- read_csv('../data/mendota_fluxes.csv')

ggplot(df) +
  geom_boxplot(aes(x = year, y = Fnep, group = year))

ggplot(df) +
  geom_boxplot(aes(x = year, y = Fsed, group = year))

ggplot(df) +
  geom_boxplot(aes(x = year, y = Fmineral, group = year))

ggplot(df) +
  geom_boxplot(aes(x = year, y = Fmineral - Fsed, group = year))

ggplot(df) +
  geom_boxplot(aes(x = year, y = Fmineral - Fsed + Fnep, group = year))

ggplot(df,aes(x = datetime, y = Fmineral - Fsed)) +
  geom_line() +
  geom_smooth()

ggplot(df) +
  geom_line(aes(x = datetime, y = fsed_first_mean) )

ggplot(df) +
  geom_line(aes(x = datetime, y = o2_hyp_middle / 1000 )) +
  geom_line(aes(x = datetime, y = o2_hyp_upper / 1000 ), linetype = 'dashed') +
  geom_line(aes(x = datetime, y = o2_hyp_lower / 1000 ), linetype = 'dashed')

ggplot(df) +
  geom_line(aes(x = datetime, y = o2_hyp_upper / 1000 ))

ggplot(subset(df, year > 2010 & year < 2016)) +
  geom_line(aes(x = datetime, y = o2_hyp_middle / 1000 ))

df.ts <-  ts(df$Fmineral - df$Fsed, frequency = 365, start=c(1995, 5, 9))
df.ts <-  ts(df$Fmineral, frequency = 365, start=c(1995, 5, 9))
df.ts <-  ts(- df$Fsed, frequency = 365, start=c(1995, 5, 9))

df.ts.mavg <- rollapply(df.ts, 7, mean)

plot(df.ts)
dec <- decompose(df.ts)
plot(dec)

plot(df.ts.mavg)
dec.mavg <- decompose(df.ts.mavg, type = "additive")
plot(dec.mavg)

ts.adj <- df.ts - dec$seasonal
plot(ts.adj)

ts.smooth <- smoothLowess((timeSeries(df.ts)), f=0.05)
tP <- turns(ts.smooth[, 2])
plot(tP)

turnsStats(ts.smooth[, 2])

ggplot(df) +
  geom_line(aes(x = seq(1,nrow(df)), y = Fmineral - Fsed)) +
  geom_vline(xintercept = 6676)

Turning_point(df.ts)
