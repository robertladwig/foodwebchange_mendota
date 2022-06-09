
biomass <- readRDS(file = "robin-data/2022-06-08_phyto_list.rds")

tot <- biomass$tot

plot(x = tot$date, y = tot$biomass, type = "l")

unique(year(tot$date))

y.lim <- c(0,max(tot$biomass))


for (yr in 2020:1995){
  i <- which(year(tot$date) == yr)
  x.lim <- parse_date_time(x = c(paste(yr,1,1), paste(yr,12,31)), orders = "ymd")
  col.vect <- rep("blue",length(i))
  col.vect[tot$biomass[i] >= 2.5] <- "green"
  plot(x = tot$date[i], y = tot$biomass[i], ylim = y.lim, xlim = x.lim, type = "n")
  points(x = tot$date[i], y = tot$biomass[i], pch = 21, bg = col.vect)
  lines(x = tot$date[i], y = tot$biomass[i])
  mtext(yr)
}

for (yr in 1995:2020){
  i <- which(year(tot$date) == yr)
  x.lim <- parse_date_time(x = c(paste(yr,4,1), paste(yr,7,31)), orders = "ymd")
  col.vect <- rep("blue",length(i))
  col.vect[tot$biomass[i] >= 5] <- "green"
  plot(x = tot$date[i], y = tot$biomass[i], ylim = y.lim, xlim = x.lim, type = "n")
  points(x = tot$date[i], y = tot$biomass[i], pch = 21, bg = col.vect)
  lines(x = tot$date[i], y = tot$biomass[i])
  mtext(yr)
}


i <- which(year(tot$date) == 1995)
plot(x = tot$date[i], y = tot$biomass[i], ylim = y.lim, xlim = x.lim, type = "n")
for (yr in 1995:2009){
  i <- which(year(tot$date) == yr)
  x.vals <- paste(1995, month(tot$date[i]), day(tot$date[i]))
  x.vals <- parse_date_time(x.vals, "ymd")
  lines(x = x.vals, y = tot$biomass[i], col = "blue")
}
for (yr in 2010:2020){
  i <- which(year(tot$date) == yr)
  x.vals <- paste(1995, month(tot$date[i]), day(tot$date[i]))
  x.vals <- parse_date_time(x.vals, "ymd")
  lines(x = x.vals, y = tot$biomass[i], col = "green")
}

