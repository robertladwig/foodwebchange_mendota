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

nutrients <- read_csv('../output/nutrients.csv')


df.strat <- strat %>%
  group_by(year) %>%
  mutate(med = mean(linear, constant.low, constant.high, spline)) %>%
  dplyr::select(year, med, linear, constant.low, constant.high, spline)

df.anoxic <- anoxic %>%
  dplyr::filter(year != 1995 & year != 2021) %>%
  dplyr::select(year, AF)

df.flux <- fluxes %>%
  dplyr::filter(year != 1995 & year != 2021) %>%
  dplyr::select(year, Jz, Jv, Ja)

df.biomass <- biomass %>%
  dplyr::filter(Year != 1995 & Year != 2021) %>%
  rename(year = Year)

df.nutrients <- nutrients %>%
  dplyr::filter(year != 1995 & year != 2021) %>%
  rename(year = year)

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
df <- merge(df, df.nutrients, by = 'year')


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

sum.hypo1 <-summary(hypo1)

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


# 2. Regression line + confidence intervals
modeleq <- paste0('y = ', round(sum.hypo1$coefficients[1,1],2),
                  ' + ',round(sum.hypo1$coefficients[2,1],2),' Strat.Dur',
                  ' + ',round(sum.hypo1$coefficients[3,1],2),' Days>0.5',
                  ' + ',round(sum.hypo1$coefficients[4,1],2),' DO.Depl',
                  ' + ',round(sum.hypo1$coefficients[5,1],2),' Clear.Dur',
                  ' + e, where e ~ N(0,',round(sum.hypo1$sigma,2),")")
library(latex2exp)

pred.int <- predict(hypo1, interval = "confidence")
pred.int <- pred.int * attr(sc.info, 'scaled:scale')[7] + attr(sc.info, 'scaled:center')[7]

mydata <- cbind(data.frame('AF'  = df$AF), pred.int)

# PLOT: linear model
p <- ggplot(mydata, aes(fit, AF)) +
  stat_smooth(method = lm) +
  geom_point(size = 2) +
  xlab('Predicted Anoxic Factor [d per season]')+
  ylab('Anoxic Factor [d per season]')+
  theme_minimal()+
  annotate("text", x = 61, y = 82.5, label = modeleq, size = 3)+
  annotate("text", x = 50, y = 80, label = (paste0('R2 = ',round(sum.hypo1$r.squared,2))), size =3) +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1));p
p.linear <- p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed"); p.linear

ggsave(file=paste0('../figs/linearModel.png'), p.linear, dpi = 300,width = 216,height = 216, units = 'mm')



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
  xlab('Clearwater duration') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_minimal()

g <- (g1 + g2) / (g3 + g4)  / (g5 + g6) / (g7 + g8); g
g <- (g1 + g4) / (g5 + g8)  / (p.linear) + plot_annotation(tag_levels = 'A') ; g
ggsave(plot = g, '../figs/linear_comparison.png', dpi = 300, units = 'in', width = 7, height = 10)




g5 <- ggplot(df) +
  geom_line(aes(year, AF)) +
  geom_point(aes(year, AF)) +
  ylab('Anoxic Factor (days per season)') + xlab('') +
  geom_vline(xintercept=2010, linetype = 'dashed') +
  theme_bw()
g6 <- ggplot(df) +
  geom_ribbon(aes(x = year, ymin = constant.low, ymax = constant.high), fill = 'grey80') +
  geom_line(aes(x = year, y = linear)) +
  geom_point(aes(x = year, y = linear)) +
  geom_line(aes(x = year, y = spline), linetype = 2, color = 'red3') +
  ylab('Stratification Duration (days)') + xlab('') +
  geom_vline(xintercept=2010, linetype = 'dashed') +
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
  geom_vline(xintercept=2010, linetype = 'dashed') +
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
  geom_vline(xintercept=2010, linetype = 'dashed') +
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
  geom_vline(xintercept=2010, linetype = 'dashed') +
  theme_bw(); 
g10 <- ggplot(df) +
  geom_line(aes(year, Clearwater.Duration)) +
  geom_point(aes(year, Clearwater.Duration)) +
  ylab('Clearwater dur. (days)') + xlab('') +
  geom_vline(xintercept=2010, linetype = 'dashed') +
  theme_bw(); 
g11 <- ggplot(df) +
  geom_line(aes(year, pH)) +
  geom_point(aes(year, pH)) +
  ylab('pH (-)') + xlab('') +
  geom_vline(xintercept=2010, linetype = 'dashed') +
  theme_bw(); 
g12 <- ggplot(df) +
  geom_line(aes(year, PO4.P_surf , col = 'surface')) +
  geom_point(aes(year, PO4.P_surf , col = 'surface')) +
  geom_line(aes(year, PO4.P_bot , col = 'bottom')) +
  geom_point(aes(year, PO4.P_bot, col  = 'bottom')) +
  ylab('Phosphate (mg/L)') + xlab('') +
  geom_vline(xintercept=2010, linetype = 'dashed') +
  theme_bw(); 
g13 <- ggplot(df) +
  geom_line(aes(year, NO3.NO2.N_surf, col = 'surface' )) +
  geom_point(aes(year, NO3.NO2.N_surf , col = 'surface')) +
  geom_line(aes(year, NO3.NO2.N_bot, col = 'bottom' )) +
  geom_point(aes(year, NO3.NO2.N_bot , col = 'bottom')) +
  ylab('Nitrate (mg/L)') + xlab('') +
  geom_vline(xintercept=2010, linetype = 'dashed') +
  theme_bw(); 
g14 <- ggplot(df) +
  geom_line(aes(year, RSi)) +
  geom_point(aes(year, RSi)) +
  ylab('React. Silica (mg/L)') + xlab('') +
  geom_vline(xintercept=2010, linetype = 'dashed') +
  theme_bw(); 


g5 / g6 / g7 / g9 /g10 /g8 / g11 /g12 /g13/ g14





library(ggpubr)
df.prior = df %>%
  mutate('class' = ifelse(year < 2010, 'prior 2010','post 2010')) %>%
  dplyr::select(class, AF, med, Jz, Days.0.5.mg.L, discharge, Clearwater.Duration, pH, PO4.P_surf, NO3.NO2.N_surf, RSi)
m.df.prior <- reshape2::melt(df.prior, id = 'class')

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'AF'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'AF'), method ="kruskal.test")

p1 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'AF'), x = "class", y = "value",
                palette = "jco", xlab = '', ylab = 'Anoxic Factor',
                add = "jitter")
#  Add p-value
p1 = p1 + stat_compare_means()
# Change method
# p1 + stat_compare_means(method = "t.test")

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'med'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'med'), method ="kruskal.test")

p2 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'med'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'Stratification duration',
                 add = "jitter")
#  Add p-value
p2 = p2 + stat_compare_means()

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'Jz'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'Jz'), method ="kruskal.test")

p3 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'Jz'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'Total oxygen sink',
                 add = "jitter")
#  Add p-value
p3 = p3 + stat_compare_means()

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'Days.0.5.mg.L'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'Days.0.5.mg.L'), method ="kruskal.test")

p4 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'Days.0.5.mg.L'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'Biomass over 0.5 mg/L',
                 add = "jitter")
#  Add p-value
p4 = p4 + stat_compare_means()

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'discharge'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'discharge'), method ="kruskal.test")

p5 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'discharge'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'Discharge',
                 add = "jitter")
#  Add p-value
p5 = p5 + stat_compare_means()

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'Clearwater.Duration'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'Clearwater.Duration'), method ="kruskal.test")

p6 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'Clearwater.Duration'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'Clearwater duration',
                 add = "jitter")
#  Add p-value
p6 = p6 + stat_compare_means()

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'pH'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'pH'), method ="kruskal.test")

p7 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'pH'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'pH',
                 add = "jitter")
#  Add p-value
p7 = p7 + stat_compare_means()

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'PO4.P_surf'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'PO4.P_surf'), method ="kruskal.test")

p8 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'PO4.P_surf'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'PO4-P surf',
                 add = "jitter")
#  Add p-value
p8 = p8 + stat_compare_means()

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'NO3.NO2.N_surf'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'NO3.NO2.N_surf'), method ="kruskal.test")

p9 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'NO3.NO2.N_surf'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'NO3-NO2-N surf',
                 add = "jitter")
#  Add p-value
p9 = p9 + stat_compare_means()

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'RSi'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'RSi'), method ="kruskal.test")

p10 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'RSi'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'RSi',
                 add = "jitter")
#  Add p-value
p10 = p10 + stat_compare_means()

(g5 / g6 / g7 / g9 / g10 /g8 / g11 /g12 /g13/ g14 ) | (p1 / p2 /p3 /p5/p6 /p4 /p7 / p8  / p9 /p10)


ggsave(plot = (g5 / g6 / g7 / g9 / g10 /g8) | (p1 / p2 /p3 /p5/p6 /p4), '../figs/timeseries_comparison.png', dpi = 300, units = 'in', width = 20, height = 17)

ggsave(plot = (g5 / g6 / g7 / g9 / g10 /g8 / g11 /g12 /g13/ g14 ) | (p1 / p2 /p3 /p5/p6 /p4 /p7 / p8  / p9 /p10) + plot_layout(guides = 'collect'), '../figs/timeseries_comparison.png', dpi = 300, units = 'in', width = 20, height = 30)


# find breaking point
library(bfast)
library(zoo)
library(strucchange)
library(xts)

ts.af =  ts(df$AF, start= 1996, frequency = 1)

plot(ts.af)

plot(merge(
       AF = as.zoo(ts.af),
       zoo(mean(AF), time(AF)),
       CUSUM = cumsum(AF - mean(AF)),
       zoo(0, time(AF)),
       MOSUM = rollapply(AF - mean(AF), 4, sum),
       zoo(0, time(AF))
     ), screen = c(1, 1, 2, 2, 3, 3), main = "", xlab = "Time",
  col = c(1, 4, 1, 4, 1, 4) )

plot(merge(
        AF = as.zoo(ts.af),
        zoo(c(NA, cumsum(head(AF, -1))/1:99), time(AF)),
        CUSUM = cumsum(c(0, recresid(lm(AF ~ 1)))),
        zoo(0, time(AF))
      ), screen = c(1, 1, 2, 2), main = "", xlab = "Time",
   col = c(1, 4, 1, 4) )

AF = as.zoo(ts.af)
plot(1996 + 4:25, sapply(4:25, function(i) {
  before <- 1:i
  after <- (i+1):4
  res <- c(AF[before] - mean(AF[before]), AF[after] - mean(AF[after]))
   sum(res^2)
   }), type = "b", xlab = "Time", ylab = "RSS")

bp.nile <- breakpoints(AF ~ 1)
nile.fac <- breakfactor(bp.nile, breaks = 1 )
fm1.nile <- lm(AF ~ nile.fac - 1)
plot(bp.nile)

ocus.nile <- efp(AF ~ 1, type = "OLS-CUSUM")

png(file = "../figs/Fig_Breakpoint.png",res = 300,width = 216,height = 216, units = 'mm')
opar <- par(mfrow=c(2,1), mar=c(2,2,0,2))
plot(ocus.nile, alt.boundary=F,main="")
abline(v= 2010, lty=2, col='red')
plot(AF, ylab="Annual Flow of the river Nile") > abline(h= mean(AF),col='blue')
abline(v= 2010, lty=2, col='red')
lines(ts(predict(fm1.nile),start=1996,freq=1), col='darkgreen',lwd=2)
par(opar)
dev.off()
