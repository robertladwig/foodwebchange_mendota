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

strat <- read_csv('../output/stratification.csv')

anoxic <- read_csv("../output/anoxicfactor.csv")

fluxes <- read_csv('../output/dosinks.csv')

df.strat <- strat %>%
  group_by(year) %>%
  mutate(med = mean(linear, constant.low, constant.high, spline)) %>%
  select(year, med)

df.anoxic <- anoxic %>%
  dplyr::filter(year != 1995) %>%
  select(year, AF)

df.flux <- fluxes %>%
  dplyr::filter(year != 1995) %>%
  select(year, Jz, Jv, Ja)

df <- merge(df.strat, df.anoxic, by = 'year')
df <- merge(df, df.flux, by = 'year')
                

mod <- lm(AF ~ med + Jv , data = df)
summary(mod)

my.formula <- y ~ (x)


g1 <- ggplot(df,aes(med, AF)) + 
  geom_point() +
  xlab('Mean stratification duration (d)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ log(x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_minimal()
g2 <- ggplot(df,aes(Jv, AF)) + 
  geom_point() +
  xlab('Volumetric sink (g/m3/d))') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ log(x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_minimal()
g3 <- ggplot(df,aes(Ja, AF)) + 
  geom_point() +
  xlab('Areal sink (g/m2/d)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ log(x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_minimal()
g4 <- ggplot(df,aes(Jz, AF)) + 
  geom_point() +
  xlab('Total sink (g/m3/d)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ log(x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_minimal()

g <- (g1 + g2) / (g3 + g4); g
ggsave(plot = g, '../figs/comparison.png', dpi = 300, units = 'in', width = 7, height = 7)
