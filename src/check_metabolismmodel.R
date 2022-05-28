setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(timeSeries)
library(statcomp)

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

ggplot(df) +
  geom_line(aes(x = datetime, y = Fmineral - Fsed))

ggplot(df) +
  geom_line(aes(x = datetime, y = o2_hyp_middle / 1000 ))

df.ts <-  ts(df$Fmineral - df$Fsed, frequency = 365, start=c(1995, 5, 9))

plot(df.ts)
dec <- decompose(df.ts)
plot(dec)

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
