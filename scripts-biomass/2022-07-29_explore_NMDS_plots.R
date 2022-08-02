# RRR
# look at NMDS plots as a way to summarize the community change seen in the bar plots

phyto.list <- readRDS("robin-data/2022-06-08_phyto_list.rds")
key <- readRDS("robin-data/2022-07-25_season_dates/seasons_by_sample.rds")

library(vegan)

# ---- functions ----

get.yearly.av <- function(my.phy){
  my.phy <- t(my.phy)
  my.phy <- as.data.frame(my.phy)
  agg.by.year <- as.numeric(substr(row.names(my.phy),start = 1, stop = 4))
  my.phy.av <- aggregate(x = my.phy, by = list(agg.by.year), FUN = mean)
  my.phy.av[1:5,1:5]
  row.names(my.phy.av) <- my.phy.av[ ,1]
  my.phy.av <- my.phy.av[ ,-1]
  my.phy.av <- as.matrix(t(my.phy.av))
  return(my.phy.av)
}

make.nmds.plot <- function(phyto, key, season, use.yearly.av = FALSE){
  my.key <- key[key$Season == season, ]
  my.phyto <- phyto[ ,key$Season == season]
  if (use.yearly.av){
    my.phyto <- get.yearly.av(my.phy = my.phyto)
    my.key <- my.key[!duplicated(my.key$Year), ]
  }
  my.nmds <- metaMDS(comm = t(my.phyto), distance = "bray", trymax = 100)
  plot(my.nmds, type = "n", ann = F, axes = F)
  box()
  if (use.yearly.av){
    cat(all.equal(row.names(my.nmds$points), as.character(my.key$Year)))
  }else{
    cat(all.equal(row.names(my.nmds$points), as.character(my.key$Date)))
  }
  points(my.nmds, col = my.key$Color.Invasion, pch = 19, cex = 2.5)
  text(my.nmds, labels = my.key$Year, cex = .5)
  mtext(text = season, side = 3, line = .5)
}

# ---- make color key ----

key$Color.Season <- "hotpink2"
key$Color.Season[key$Season == "ice-on"] <- "snow3"
key$Color.Season[key$Season == "spring"] <- "tan4"
key$Color.Season[key$Season == "stratified"] <- "chartreuse4"

key$Color.Invasion <- "steelblue"
key$Color.Invasion[key$Year >= 2010] <- "orange2"

phyto <- phyto.list$div

phyto[1:5,1:5]
phyto[is.na(phyto)] <- 0

phyto.nmds <- metaMDS(comm = t(phyto), distance = "bray", trymax = 100)
plot(phyto.nmds, type = "n")
all.equal(row.names(phyto.nmds$points), as.character(key$Date))
points(phyto.nmds, col = key$Color.Season, pch = 19, cex = .9)

# ---- all data for each year ----

par(mfrow = c(2,2), mar = c(1,1,2,1))
make.nmds.plot(phyto = phyto, key = key, season = "ice-on")
make.nmds.plot(phyto = phyto, key = key, season = "spring")
make.nmds.plot(phyto = phyto, key = key, season = "stratified")
make.nmds.plot(phyto = phyto, key = key, season = "fall")

# ---- averages for each year ----
par(mfrow = c(2,2), mar = c(1,1,2,1))
make.nmds.plot(phyto = phyto, key = key, season = "ice-on", use.yearly.av = TRUE)
make.nmds.plot(phyto = phyto, key = key, season = "spring", use.yearly.av = TRUE)
make.nmds.plot(phyto = phyto, key = key, season = "stratified", use.yearly.av = TRUE)
make.nmds.plot(phyto = phyto, key = key, season = "fall", use.yearly.av = TRUE)
