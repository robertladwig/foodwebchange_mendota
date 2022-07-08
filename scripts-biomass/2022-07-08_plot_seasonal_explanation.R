# RRR
# make figure 3- before and after biomass by season
# about the data being used:
# it is the LTER biomass (mg/L) data totaled across all phytoplankton counted
# then this was linearly interpolated to have a biomass value for every day
# then seasons were assigned to each day:
#   - ice-on based on LTER data
#   - stratified based on Roberts calculated intervals
#   - spring is between ice-off and stratification
#   - fall is between stratification and ice on
# then for each year, the cumulative, average, and total days in each season was calculated

# Note, for L&O letters, small figs are 80 mm (~3 in) wide, and large figures are 180 mm (~7 in) wide

biomass <- readRDS("robin-data/2022-06-23_biomass_metrics/biomass_metrics_by_season-interp_values.rds")
names(biomass)
names(biomass$linear)
head(biomass$linear$spring)

plot.folder <- "plots/2022-07-08_biomass_by_season_boxplots/"


# ---- black and white, side by side ----

m <- "mean"
exclude.if.few.samples <- TRUE
include.jitter <- FALSE
for (m in names(biomass)){
  ice <- biomass[[m]]$ice
  spring <- biomass[[m]]$spring
  strat <- biomass[[m]]$stratified
  fall <- biomass[[m]]$fall
  
  if (exclude.if.few.samples){
    ice.cutoff <- 2
    spring.cutoff <- 2
    strat.cutoff <- 10
    fall.cutoff <- 2
    ice <- ice[!ice$Measured.Days < ice.cutoff, ]
    spring <- spring[!spring$Measured.Days < spring.cutoff, ]
    strat <- strat[!strat$Measured.Days < strat.cutoff, ]
    fall <- fall[!fall$Measured.Days < fall.cutoff, ]
  }
  
  spring.pre <- spring[spring$Year <= 2009, ]
  spring.post <- spring[spring$Year >= 2010, ]
  strat.pre <- strat[strat$Year <= 2009, ]
  strat.post <- strat[strat$Year >= 2010, ]
  fall.pre <- fall[fall$Year <= 2009, ]
  fall.post <- fall[fall$Year >= 2010, ]
  
  av.bio <- list(spring.pre$Ave.Daily.mg.L, spring.post$Ave.Daily.mg.L, 
                  strat.pre$Ave.Daily.mg.L, strat.post$Ave.Daily.mg.L, 
                  fall.pre$Ave.Daily.mg.L, fall.post$Ave.Daily.mg.L)
  names(av.bio) <- c("spring.pre","spring.post","strat.pre","strat.post","fall.pre","fall.post")
  
  cum.bio <- list(spring.pre$Cumulative.mg.L, spring.post$Cumulative.mg.L, 
                  strat.pre$Cumulative.mg.L, strat.post$Cumulative.mg.L, 
                  fall.pre$Cumulative.mg.L, fall.post$Cumulative.mg.L)
  names(cum.bio) <- c("spring.pre","spring.post","strat.pre","strat.post","fall.pre","fall.post")
  
  pdf(file = paste0(plot.folder,"/quicklook_", m, "__jitter-", include.jitter, "__remove_low_n-", exclude.if.few.samples, ".pdf"), 
      width = 6.5, height = 3)
  
  par(mar = c(2,3.5,2,1.5), oma = c(.1,.1,.1,.1),mfrow = c(1,2))
  
  boxplot(x = av.bio, notch = F, range = 0, lty = 1, axes = F, at = c(1,2,4,5,7,8))
  axis(side = 2, las = 2)
  axis(side = 1, at = c(1.5, 4.5, 7.5), labels = F)
  box()
  mtext(text = c("Spring","Stratified","Fall"), side = 1, line = .75, at = c(1.5, 4.5, 7.5), xpd = T)
  mtext(text = "Average Biomass (mg/L)", side = 2, outer = F, line = 2.5)
  
  if (include.jitter){
    points(x = jitter(rep(1, length(av.bio$spring.pre)), factor = 15), y = av.bio$spring.pre, pch = 21, bg = adjustcolor("black",.3), cex = .7)
    points(x = 1 + jitter(rep(1, length(av.bio$spring.post)), factor = 15), y = av.bio$spring.post, pch = 21, bg = adjustcolor("black",.3), cex = .7)
    points(x = 3 + jitter(rep(1, length(av.bio$strat.pre)), factor = 15), y = av.bio$strat.pre, pch = 21, bg = adjustcolor("black",.3), cex = .7)
    points(x = 4 + jitter(rep(1, length(av.bio$strat.post)), factor = 15), y = av.bio$strat.post, pch = 21, bg = adjustcolor("black",.3), cex = .7)
    points(x = 6 + jitter(rep(1, length(av.bio$fall.pre)), factor = 15), y = av.bio$fall.pre, pch = 21, bg = adjustcolor("black",.3), cex = .7)
    points(x = 7 + jitter(rep(1, length(av.bio$fall.post)), factor = 15), y = av.bio$fall.post, pch = 21, bg = adjustcolor("black",.3), cex = .7)
  }
  
  boxplot(x = cum.bio, notch = F, range = 0, lty = 1, axes = F, at = c(1,2,4,5,7,8))
  axis(side = 2, las = 2)
  axis(side = 1, at = c(1.5, 4.5, 7.5), labels = F)
  box()
  mtext(text = c("Spring","Stratified","Fall"), side = 1, line = .75, at = c(1.5, 4.5, 7.5), xpd = T)
  mtext(text = "Cumulative Biomass (mg/L)", side = 2, outer = F, line = 3.5)
  
  if (include.jitter){
    points(x = jitter(rep(1, length(cum.bio$spring.pre)), factor = 15), y = cum.bio$spring.pre, pch = 21, bg = adjustcolor("black",.3), cex = .7)
    points(x = 1 + jitter(rep(1, length(cum.bio$spring.post)), factor = 15), y = cum.bio$spring.post, pch = 21, bg = adjustcolor("black",.3), cex = .7)
    points(x = 3 + jitter(rep(1, length(cum.bio$strat.pre)), factor = 15), y = cum.bio$strat.pre, pch = 21, bg = adjustcolor("black",.3), cex = .7)
    points(x = 4 + jitter(rep(1, length(cum.bio$strat.post)), factor = 15), y = cum.bio$strat.post, pch = 21, bg = adjustcolor("black",.3), cex = .7)
    points(x = 6 + jitter(rep(1, length(cum.bio$fall.pre)), factor = 15), y = cum.bio$fall.pre, pch = 21, bg = adjustcolor("black",.3), cex = .7)
    points(x = 7 + jitter(rep(1, length(cum.bio$fall.post)), factor = 15), y = cum.bio$fall.post, pch = 21, bg = adjustcolor("black",.3), cex = .7) 
  }
  
  mtext(text = paste(m, "stratification model"), side = 3, outer = T, line = -1.25)
  
  dev.off()
}


