library(vegan)
library(lubridate)

phyto.list <- readRDS("robin-data/2022-06-08_phyto_list.rds")
key <- readRDS("robin-data/2022-07-25_season_dates/seasons_by_sample.rds")

phyto.list$div[is.na(phyto.list$div)] <- 0
phyto.list$gen[is.na(phyto.list$gen)] <- 0
phyto.list$tax[is.na(phyto.list$tax)] <- 0

diversity.table <- data.frame(key)

# division-level
diversity.table$division.shannon <- diversity(x = phyto.list$div, index = "shannon", MARGIN = 2)

diversity.table$division.simpson <- diversity(x = phyto.list$div, index = "simpson", MARGIN = 2)

diversity.table$division.invsimpson <- diversity(x = phyto.list$div, index = "invsimpson", MARGIN = 2)

pres.abs <- phyto.list$div > 0
diversity.table$division.richness <- colSums(pres.abs)

# genera-level
diversity.table$genera.shannon <- diversity(x = phyto.list$gen, index = "shannon", MARGIN = 2)

diversity.table$genera.simpson <- diversity(x = phyto.list$gen, index = "simpson", MARGIN = 2)

diversity.table$genera.invsimpson <- diversity(x = phyto.list$gen, index = "invsimpson", MARGIN = 2)

pres.abs <- phyto.list$gen > 0
diversity.table$genera.richness <- colSums(pres.abs)

# taxa-level
diversity.table$taxon.shannon <- diversity(x = phyto.list$tax, index = "shannon", MARGIN = 2)

diversity.table$taxon.simpson <- diversity(x = phyto.list$tax, index = "simpson", MARGIN = 2)

diversity.table$taxon.invsimpson <- diversity(x = phyto.list$tax, index = "invsimpson", MARGIN = 2)

pres.abs <- phyto.list$tax > 0
diversity.table$taxon.richness <- colSums(pres.abs)


head(diversity.table)

# look at diversity over time ----

x.ax <- unique(diversity.table$Year)
x.ax.ticks <- parse_date_time(paste0("1-1-",x.ax), "mdy")
index <- seq.int(from = 1, by = 5, length.out = 6)
x.ax.lab.loc <- x.ax.ticks[index]
x.ax.lab <- x.ax[index]

c = 5 # 5-16
for (c in 5:16){
  
  filename <- paste0("plots/2022-08-02_diversity_plots/", colnames(diversity.table)[c],".pdf")
  pdf(file = filename, width = 8, height = 10.5)
  
  # all dates
  r <-1:nrow(diversity.table)
  pre <- diversity.table$Year < 2010
  post <- diversity.table$Year >= 2010
  
  par(fig = c(0,.8,.8,1), mar = c(2,2,.5,.5), oma = c(.1,1,.1,1), cex.axis = .7)
  plot(x = diversity.table$Date[r], y = diversity.table[r,c], type = "n", ann = F, axes = F)
  points(x = diversity.table$Date[r], y = diversity.table[r,c], cex = .5)
  lines(x = diversity.table$Date[r], y = diversity.table[r,c])
  mtext(text = "all dates", side = 4, line = .5)
  box()
  axis(side = 1, lwd = 0, lwd.ticks = 1, at = x.ax.ticks, labels = F)
  axis(side = 1, lwd = 0, labels = x.ax.lab, at = x.ax.lab.loc, line = -.5)
  axis(side = 2,lwd = 0, lwd.ticks = 1, labels = F)
  axis(side = 2,las = 2, lwd = 0, labels = T, line = -.5)
  
  par(fig = c(.8,1,.8,1), mar = c(2,2,.5,.5), oma = c(.1,2,.1,0), new = T)
  boxplot(x = list("pre" = diversity.table[pre,c], "post" = diversity.table[post,c]), axes = F)
  box()
  axis(side = 1, at = c(1,2), labels = F, lwd = 0, lwd.ticks = 1)
  axis(side = 1, at = c(1,2), labels = c("pre","post"), lwd = 0, line = -.5)
  axis(side = 2,lwd = 0, lwd.ticks = 1, labels = F)
  axis(side = 2,las = 2, lwd = 0, labels = T, line = -.5)
  
  # spring
  r <- diversity.table$Season == "spring"
  pre <- diversity.table$Season == "spring" & diversity.table$Year < 2010
  post <- diversity.table$Season == "spring" & diversity.table$Year >= 2010
  
  par(fig = c(0,.8,.6,.8), mar = c(2,2,.5,.5), oma = c(.1,1,.1,1), new = T)
  plot(x = diversity.table$Date[r], y = diversity.table[r,c], type = "n", ann = F, axes = F)
  points(x = diversity.table$Date[r], y = diversity.table[r,c], cex = .5)
  lines(x = diversity.table$Date[r], y = diversity.table[r,c])
  mtext(text = "spring", side = 4, line = .5)
  box()
  axis(side = 1, lwd = 0, lwd.ticks = 1, at = x.ax.ticks, labels = F)
  axis(side = 1, lwd = 0, labels = x.ax.lab, at = x.ax.lab.loc, line = -.5)
  axis(side = 2,lwd = 0, lwd.ticks = 1, labels = F)
  axis(side = 2,las = 2, lwd = 0, labels = T, line = -.5)
  
  par(fig = c(.8,1,.6,.8), mar = c(2,2,.5,.5), oma = c(.1,2,.1,0), new = T)
  boxplot(x = list("pre" = diversity.table[pre,c], "post" = diversity.table[post,c]), axes = F)
  box()
  axis(side = 1, at = c(1,2), labels = F, lwd = 0, lwd.ticks = 1)
  axis(side = 1, at = c(1,2), labels = c("pre","post"), lwd = 0, line = -.5)
  axis(side = 2,lwd = 0, lwd.ticks = 1, labels = F)
  axis(side = 2,las = 2, lwd = 0, labels = T, line = -.5)
  
  # stratified
  r <- diversity.table$Season == "stratified"
  pre <- diversity.table$Season == "stratified" & diversity.table$Year < 2010
  post <- diversity.table$Season == "stratified" & diversity.table$Year >= 2010
  
  par(fig = c(0,.8,.4,.6), mar = c(2,2,.5,.5), oma = c(.1,1,.1,1), new = T)
  plot(x = diversity.table$Date[r], y = diversity.table[r,c], type = "n", ann = F, axes = F)
  points(x = diversity.table$Date[r], y = diversity.table[r,c], cex = .5)
  lines(x = diversity.table$Date[r], y = diversity.table[r,c])
  mtext(text = "stratified", side = 4, line = .5)
  box()
  axis(side = 1, lwd = 0, lwd.ticks = 1, at = x.ax.ticks, labels = F)
  axis(side = 1, lwd = 0, labels = x.ax.lab, at = x.ax.lab.loc, line = -.5)
  axis(side = 2,lwd = 0, lwd.ticks = 1, labels = F)
  axis(side = 2,las = 2, lwd = 0, labels = T, line = -.5)
  
  par(fig = c(.8,1,.4,.6), mar = c(2,2,.5,.5), oma = c(.1,2,.1,0), new = T)
  boxplot(x = list("pre" = diversity.table[pre,c], "post" = diversity.table[post,c]), axes = F)
  box()
  axis(side = 1, at = c(1,2), labels = F, lwd = 0, lwd.ticks = 1)
  axis(side = 1, at = c(1,2), labels = c("pre","post"), lwd = 0, line = -.5)
  axis(side = 2,lwd = 0, lwd.ticks = 1, labels = F)
  axis(side = 2,las = 2, lwd = 0, labels = T, line = -.5)
  
  mtext(text = colnames(diversity.table)[c], side = 2, outer = T, line = .75)
  
  # fall
  r <- diversity.table$Season == "fall"
  pre <- diversity.table$Season == "fall" & diversity.table$Year < 2010
  post <- diversity.table$Season == "fall" & diversity.table$Year >= 2010
  
  par(fig = c(0,.8,.2,.4), mar = c(2,2,.5,.5), oma = c(.1,1,.1,1), new = T)
  plot(x = diversity.table$Date[r], y = diversity.table[r,c], type = "n", ann = F, axes = F)
  points(x = diversity.table$Date[r], y = diversity.table[r,c], cex = .5)
  lines(x = diversity.table$Date[r], y = diversity.table[r,c])
  mtext(text = "fall", side = 4, line = .5)
  box()
  axis(side = 1, lwd = 0, lwd.ticks = 1, at = x.ax.ticks, labels = F)
  axis(side = 1, lwd = 0, labels = x.ax.lab, at = x.ax.lab.loc, line = -.5)
  axis(side = 2,lwd = 0, lwd.ticks = 1, labels = F)
  axis(side = 2,las = 2, lwd = 0, labels = T, line = -.5)
  
  par(fig = c(.8,1,.2,.4), mar = c(2,2,.5,.5), oma = c(.1,2,.1,0), new = T)
  boxplot(x = list("pre" = diversity.table[pre,c], "post" = diversity.table[post,c]), axes = F)
  box()
  axis(side = 1, at = c(1,2), labels = F, lwd = 0, lwd.ticks = 1)
  axis(side = 1, at = c(1,2), labels = c("pre","post"), lwd = 0, line = -.5)
  axis(side = 2,lwd = 0, lwd.ticks = 1, labels = F)
  axis(side = 2,las = 2, lwd = 0, labels = T, line = -.5)
  
  
  # ice-on
  r <- diversity.table$Season == "ice-on"
  pre <- diversity.table$Season == "ice-on" & diversity.table$Year < 2010
  post <- diversity.table$Season == "ice-on" & diversity.table$Year >= 2010
  
  par(fig = c(0,.8,0,.2), mar = c(2,2,.5,.5), oma = c(.1,1,.1,1), new = T)
  plot(x = diversity.table$Date[r], y = diversity.table[r,c], type = "n", ann = F, axes = F)
  points(x = diversity.table$Date[r], y = diversity.table[r,c], cex = .5)
  lines(x = diversity.table$Date[r], y = diversity.table[r,c])
  mtext(text = "ice-on", side = 4, line = .5)
  box()
  axis(side = 1, lwd = 0, lwd.ticks = 1, at = x.ax.ticks, labels = F)
  axis(side = 1, lwd = 0, labels = x.ax.lab, at = x.ax.lab.loc, line = -.5)
  axis(side = 2,lwd = 0, lwd.ticks = 1, labels = F)
  axis(side = 2,las = 2, lwd = 0, labels = T, line = -.5)
  
  par(fig = c(.8,1,.0,.2), mar = c(2,2,.5,.5), oma = c(.1,2,.1,0), new = T)
  boxplot(x = list("pre" = diversity.table[pre,c], "post" = diversity.table[post,c]), axes = F)
  box()
  axis(side = 1, at = c(1,2), labels = F, lwd = 0, lwd.ticks = 1)
  axis(side = 1, at = c(1,2), labels = c("pre","post"), lwd = 0, line = -.5)
  axis(side = 2,lwd = 0, lwd.ticks = 1, labels = F)
  axis(side = 2,las = 2, lwd = 0, labels = T, line = -.5)
  
  dev.off()
}
