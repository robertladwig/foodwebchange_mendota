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
  select(year, med, linear, constant.low, constant.high, spline)

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



g5 <- ggplot(df.anoxic) +
  geom_line(aes(year, AF)) +
  geom_point(aes(year, AF)) +
  ylab('Anoxic Factor (days per season)') + xlab('') +
  theme_bw()
g6 <- ggplot(df.strat) +
  geom_ribbon(aes(x = year, ymin = constant.low, ymax = constant.high), fill = 'grey80') +
  geom_line(aes(x = year, y = linear)) +
  geom_point(aes(x = year, y = linear)) +
  geom_line(aes(x = year, y = spline), linetype = 2, color = 'red3') +
  ylab('Stratification Duration (days)') + xlab('') +
  theme_bw()
g7 <- ggplot(df.flux, aes(year, Jz, col = 'Volumetric')) +
  geom_line(aes(year, Jv, col = 'Volumetric')) +
  # geom_line(aes(year, Jz, col = 'Median Flux')) +
  geom_point(aes(year, Jv, col = 'Volumetric')) +
  # geom_point(aes(year, Jz, col = 'Median Flux')) +
  geom_smooth(method = "loess", size = 1.5) +
  geom_line(aes(year, Ja , col = 'Areal')) +
  geom_point(aes(year, Ja , col = 'Areal')) +
  geom_smooth(aes(year, Ja , col = 'Areal'), method = "loess", size = 1.5) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = expression("Areal flux ["*g~m^{-2}*d^{-1}*"]"))) +
  ylab(expression("Volumetric flux ["*g~m^{-3}*d^{-1}*"]")) + xlab('') +
  theme_bw()+
  theme(axis.line.y.right = element_line(color = "red"),
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"),
        axis.line.y.left = element_line(color = "darkcyan"),
        axis.ticks.y.left = element_line(color = "darkcyan"),
        axis.text.y.left = element_text(color = "darkcyan"),
        axis.title.y.left = element_text(color = "darkcyan"),
        legend.position = "none"
  ); g5 / g6 / g7
# ggplot(coeff, aes(year, Ja, col = 'Volume')) +
#   # geom_line(aes(year, Jv, col = 'Volume')) +
#   geom_line(aes(year, Ja , col = 'Sediment')) +
#   # geom_line(aes(year, Jz, col = 'Median Sink')) +
#   # geom_point(aes(year, Jv, col = 'Volume')) +
#   geom_point(aes(year, Ja , col = 'Sediment')) +
#   # geom_point(aes(year, Jz, col = 'Median Sink')) +
#   # geom_smooth(method = "loess", size = 1.5) +
#   geom_smooth(aes(year, Ja , col = 'Sediment'), method = "loess", size = 1.5) +
#   ylab("Oxygen flux in g/m2/d") + xlab('') +
#   theme_bw()
ggsave(plot = g5 / g6 / g7, '../figs/timeseries_comparison.png', dpi = 300, units = 'in', width = 7, height = 9)
