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
library(Boruta)
library(caret)
library(relaimpo)
library(corrplot)
library(RColorBrewer)

# PLOT: correlation plot for importrant predictors
png(file = "results/Fig_Correlation.png",res = 300,width = 216,height = 216, units = 'mm')
c.plot <- corrplot(res, type = "upper", order = "alphabet", addCoef.col = "black",
                   tl.col = "black", tl.srt = 45, method = 'color', sig.level = 0.001, insig = "blank",
                   col=(brewer.pal(n=8, name="RdYlBu")));c.plot
dev.off()


strat <- read_csv('../output/stratification.csv')

anoxic <- read_csv("../output/anoxicfactor.csv")

fluxes <- read_csv('../output/dosinks.csv')

biomass <- read_csv('../output/biomass_duration.csv')

discharge <- read_csv('../output/discharge.csv')

cw <- readRDS('../data/yearly_clearwater_stats.rds')

df.strat <- strat %>%
  group_by(year) %>%
  mutate(med = mean(linear, constant.low, constant.high, spline)) %>%
  select(year, med, linear, constant.low, constant.high, spline)

df.anoxic <- anoxic %>%
  dplyr::filter(year != 1995 & year != 2021) %>%
  select(year, AF)

df.flux <- fluxes %>%
  dplyr::filter(year != 1995 & year != 2021) %>%
  select(year, Jz, Jv, Ja)

df.biomass <- biomass %>%
  dplyr::filter(Year != 1995 & Year != 2021) %>%
  rename(year = Year)

df.discharge <- discharge %>%
  dplyr::filter(year != 1995 & year != 2021) 

df.cw <- cw %>%
  dplyr::filter(Year > 1995) %>%
  rename(year = Year)

df <- merge(df.strat, df.anoxic, by = 'year')
df <- merge(df, df.flux, by = 'year')
df <- merge(df, df.biomass, by = 'year')
df <- merge(df, df.discharge, by = 'year')
df <- merge(df, df.cw, by = 'year')


mod <- lm(AF ~ med + Days.0.5.mg.L + Jz, data = df)
summary(mod)

boruta_output <- Boruta(AF ~ med + Jz + Jv + Ja + 
                          Days.0.5.mg.L + Days.1.mg.L + Days.1.5.mg.L +
                          Days.2.mg.L + Days.3.mg.L +
                          discharge + max.discharge + min.discharge +
                          Clearwater.Duration + Prev.Winter.Duration + Max.Clearwater.Depth.m, 
                        data = df, doTrace=2,
                        maxRuns = 1e4)  # perform Boruta search
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables
plot(boruta_output, cex.axis=1.5, las=3, xlab="", main="")  # plot variable importance

final.boruta <- TentativeRoughFix(boruta_output)
print(final.boruta)
plot(final.boruta)
boruta.df <- attStats(final.boruta)
boruta_signif =getSelectedAttributes(final.boruta, withTentative = F)
print(boruta.df)
print(boruta_signif)

idx = which( colnames(df) %in% boruta_signif)
hyp.data = df[,idx]
hyp.data$AF = df$AF
sc.info <- scale(hyp.data)
hyp.data <- as.data.frame(scale(hyp.data))


hypo1 <- lm(AF ~ ., data = hyp.data)
summary(hypo1)
sum.hypo1 <-summary(hypo1)
step(hypo1)

hypo1 <- lm(AF ~ med + Days.0.5.mg.L + Jz + Clearwater.Duration, data = hyp.data)

AIC(hypo1)
BIC(hypo1)

drop1(hypo1, test = 'F')

relImportance <-calc.relimp(hypo1, type = "lmg", rela = TRUE)
sort(relImportance$lmg, decreasing=TRUE)
boot <- boot.relimp(hypo1, b = 1000, type = c("lmg",
                                              "last", "first", "pratt"), rank = TRUE,
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot, lev =0.9, nodiff=TRUE) # print result

varImp(hypo1, scale = TRUE)

hyp.data2 <- hyp.data[, c(1,2,3,6,7)]

res=cor(hyp.data2, method = c("pearson"))

# PLOT: correlation plot for importrant predictors
png(file = "../figs/model.png",res = 300,width = 216,height = 216, units = 'mm')
c.plot <- corrplot(res, type = "upper", order = "alphabet", addCoef.col = "black",
                   tl.col = "black", tl.srt = 45, method = 'color', sig.level = 0.001, insig = "blank",
                   col=(brewer.pal(n=8, name="RdYlBu")));c.plot
dev.off()


my.formula <- y ~ (x)


g1 <- ggplot(df,aes(med, AF)) + 
  geom_point() +
  xlab('Mean stratification duration (d)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_minimal()
g2 <- ggplot(df,aes(Jv, AF)) + 
  geom_point() +
  xlab('Volumetric sink (g/m3/d))') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_minimal()
g3 <- ggplot(df,aes(Ja, AF)) + 
  geom_point() +
  xlab('Areal sink (g/m2/d)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_minimal()
g4 <- ggplot(df,aes(Jz, AF)) + 
  geom_point() +
  xlab('Total sink (g/m3/d)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_minimal()
g5 <- ggplot(df,aes(Days.0.5.mg.L, AF)) + 
  geom_point() +
  xlab('Biomass over 0.5 mg/L (d)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_minimal()
g6 <- ggplot(df,aes(Days.1.5.mg.L, AF)) + 
  geom_point() +
  xlab('Biomass over 1 mg/L (d)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_minimal()
g7 <- ggplot(df,aes(discharge, AF)) + 
  geom_point() +
  xlab('Yahara Q (cfs)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_minimal()
g8 <- ggplot(df,aes(Clearwater.Duration, AF)) + 
  geom_point() +
  xlab('Yahara Q (cfs)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_minimal()

g <- (g1 + g2) / (g3 + g4)  / (g5 + g6) / (g7 + g8); g
ggsave(plot = g, '../figs/comparison.png', dpi = 300, units = 'in', width = 7, height = 7)



g5 <- ggplot(df) +
  geom_line(aes(year, AF)) +
  geom_point(aes(year, AF)) +
  ylab('Anoxic Factor (days per season)') + xlab('') +
  theme_bw()
g6 <- ggplot(df) +
  geom_ribbon(aes(x = year, ymin = constant.low, ymax = constant.high), fill = 'grey80') +
  geom_line(aes(x = year, y = linear)) +
  geom_point(aes(x = year, y = linear)) +
  geom_line(aes(x = year, y = spline), linetype = 2, color = 'red3') +
  ylab('Stratification Duration (days)') + xlab('') +
  theme_bw()
g7 <- ggplot(df, aes(year, Jz, col = 'Volumetric')) +
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
  )
g8 <- ggplot(df) +
  geom_line(aes(year, Days.0.5.mg.L, col = '0.5 mg/L')) +
  geom_point(aes(year, Days.0.5.mg.L, col = '0.5 mg/L')) +
  geom_line(aes(year, Days.1.mg.L, col = '1 mg/L')) +
  geom_point(aes(year, Days.1.mg.L, col = '1 mg/L')) +
  geom_line(aes(year, Days.1.5.mg.L, col = '1.5 mg/L')) +
  geom_point(aes(year, Days.1.5.mg.L, col = '1.5 mg/L')) +
  geom_line(aes(year, Days.2.mg.L, col = '2 mg/L')) +
  geom_point(aes(year, Days.2.mg.L, col = '2 mg/L')) +
  geom_line(aes(year, Days.3.mg.L, col = '3 mg/L')) +
  geom_point(aes(year, Days.3.mg.L, col = '3 mg/L')) +
  ylab('Biomass over threshold (days per year)') + xlab('') +
  theme_bw()+
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())
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
g9 <- ggplot(df) +
  geom_ribbon(aes(x = year, ymin = min.discharge, ymax = discharge), fill = 'grey80') +
  geom_line(aes(year, discharge)) +
  geom_point(aes(year, discharge)) +
  ylab('Yahara Q (cfs)') + xlab('') +
  theme_bw(); 
g10 <- ggplot(df) +
  geom_line(aes(year, Clearwater.Duration)) +
  geom_point(aes(year, Clearwater.Duration)) +
  ylab('Clearwater dur. (days)') + xlab('') +
  theme_bw(); g5 / g6 / g7 / g9 /g10 /g8
ggsave(plot = g5 / g6 / g7 / g9 / g10 /g8, '../figs/timeseries_comparison.png', dpi = 300, units = 'in', width = 7, height = 17)
