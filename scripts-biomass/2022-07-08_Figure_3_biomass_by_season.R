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


# ---- prep data ----

m <- "mean"

ice <- biomass[[m]]$ice
spring <- biomass[[m]]$spring
strat <- biomass[[m]]$stratified
fall <- biomass[[m]]$fall

ice.cutoff <- 2
spring.cutoff <- 2
strat.cutoff <- 10
fall.cutoff <- 2
ice <- ice[!ice$Measured.Days < ice.cutoff, ]
spring <- spring[!spring$Measured.Days < spring.cutoff, ]
strat <- strat[!strat$Measured.Days < strat.cutoff, ]
fall <- fall[!fall$Measured.Days < fall.cutoff, ]

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

col.pre <- "steelblue"
col.post <- "orange3"

# ---- make plot ----

# pdf(file = paste0(plot.folder,"/Figure_3-biomass_by_season.pdf"), 
#     width = 7, height = 3)
pdf(file = paste0("figs/Figure_3-biomass_by_season.pdf"), 
    width = 7, height = 3)

par(mar = c(1.75,3,.5,2), oma = c(.1,.1,.1,.1))
par(fig = c(0,.42,0,1))

boxplot(x = av.bio, notch = F, range = 0, lty = 1, axes = F, at = c(1,2,4,5,7,8), col = rep(c(col.pre,col.post), 3))
axis(side = 2, las = 2)
axis(side = 1, at = c(1.5, 4.5, 7.5), labels = F)
box()
mtext(text = c("Spring","Stratified","Fall"), side = 1, line = .75, at = c(1.2, 4.5, 7.5), xpd = T)
mtext(text = "Average Biomass (mg/L)", side = 2, outer = F, line = 2)


par(fig = c(.42,.84,0,1), new = TRUE)
boxplot(x = cum.bio, notch = F, range = 0, lty = 1, axes = F, at = c(1,2,4,5,7,8), col = rep(c(col.pre,col.post), 3))
axis(side = 2, las = 2)
axis(side = 1, at = c(1.5, 4.5, 7.5), labels = F)
box()
mtext(text = c("Spring","Stratified","Fall"), side = 1, line = .75, at = c(1.2, 4.5, 7.5), xpd = T)
mtext(text = "Cumulative Biomass (mg/L)", side = 2, outer = F, line = 3.25)

par(fig = c(.8,1,0,1), new = TRUE, mar = c(.1,.1,.1,.1))
plot(1:10,1:10, type = "n", axes = F, ann = F)
lab.1.ht <- 8
lab.2.ht <- 6.75
rect.size <- .25

rect(xleft = 1, xright = 2, ybottom = lab.1.ht - rect.size, ytop = lab.1.ht + rect.size, col = col.pre, xpd = NA)
rect(xleft = 1, xright = 2, ybottom = lab.2.ht - rect.size, ytop = lab.2.ht + rect.size, col = col.post, xpd = NA)
text(x = 3, y = lab.1.ht, label = "Pre-invasion", adj = 0)
text(x = 3, y = lab.2.ht, label = "Post-invasion", adj = 0)
dev.off()

