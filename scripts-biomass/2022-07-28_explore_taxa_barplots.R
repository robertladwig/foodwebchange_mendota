# RRR
# look at community composition via barplots
# meh, first look at least just do a barplot for every day

phyto.list <- readRDS("robin-data/2022-06-08_phyto_list.rds")
key <- readRDS("robin-data/2022-07-25_season_dates/seasons_by_sample.rds")

phyto <- phyto.list$div

phyto[1:5,1:5]

# Make division color key ----
col.key <- data.frame("taxa" = row.names(phyto), "color" = rainbow(n = nrow(phyto)))

# colors from photos... too similar I think
col.key$color[col.key$taxa == "Bacillariophyta"] <- adjustcolor("#544614", .8) # diatoms
col.key$color[col.key$taxa == "Chrysophyta"] <- adjustcolor("#e3cd5b",.8)
col.key$color[col.key$taxa == "Chlorophyta"] <- adjustcolor("#275510",.8)
col.key$color[col.key$taxa == "Cryptophyta"] <- adjustcolor("#842b2c",.8)
col.key$color[col.key$taxa == "Cyanophyta"] <- adjustcolor("#9fb9d3",.8)
col.key$color[col.key$taxa == "Pyrrhophyta"] <- adjustcolor("#a3ba2e",.8)
col.key$color[col.key$taxa == "Miscellaneous"] <- adjustcolor("grey",.8)
col.key$color[col.key$taxa == "Xanthophyta"] <- adjustcolor("#718f20",.8)
col.key$color[col.key$taxa == "Euglenophyta"] <- adjustcolor("#3a4d1d",.8)
col.key$color[col.key$taxa == "Haptophyta"] <- adjustcolor("#b67024",.8)

# more different from each other
col.key$color[col.key$taxa == "Bacillariophyta"] <- adjustcolor(rainbow(n = 20, v = .5)[3], .8) # diatoms
col.key$color[col.key$taxa == "Chrysophyta"] <- adjustcolor(rainbow(n = 20, v = 1)[2],.5)
col.key$color[col.key$taxa == "Chlorophyta"] <- adjustcolor(rainbow(n = 20, v = .9)[8],.8)
col.key$color[col.key$taxa == "Cryptophyta"] <- adjustcolor(rainbow(n = 20, v = .9)[1],.7)
col.key$color[col.key$taxa == "Cyanophyta"] <- adjustcolor(rainbow(n = 20, v = .8)[12],.8)
col.key$color[col.key$taxa == "Pyrrhophyta"] <- adjustcolor(rainbow(n = 20, v = .9)[5],.8)
col.key$color[col.key$taxa == "Miscellaneous"] <- adjustcolor("grey",.8)
col.key$color[col.key$taxa == "Xanthophyta"] <- adjustcolor(rainbow(n = 20, v = .5)[9],.8)
col.key$color[col.key$taxa == "Euglenophyta"] <- adjustcolor(rainbow(n = 20, v = .2)[6],.5)
col.key$color[col.key$taxa == "Haptophyta"] <- adjustcolor(rainbow(n = 20, v = .9)[20],.4)


# ----

barplot(phyto)

spring <- phyto[ ,key$Season == "spring"]
spring[is.na(spring)] <- 0

index <- order(rowSums(spring), decreasing = T)
spring <- spring[index, ]
phyto <- phyto[index, ]
col.key <- col.key[index, ]
all.equal(row.names(spring), col.key$taxa)

barplot(spring, las  = 2, cex.names = .5, legend = F, col = col.key$color)

fall <- phyto[ ,key$Season == "fall"]
fall[is.na(fall)] <- 0

all.equal(row.names(fall), col.key$taxa)

barplot(fall, las  = 2, cex.names = .5, legend = F, col = col.key$color)


stratified <- phyto[ ,key$Season == "stratified"]
stratified[is.na(stratified)] <- 0

all.equal(row.names(stratified), col.key$taxa)

barplot(stratified, las  = 2, cex.names = .5, legend = F, col = col.key$color)
