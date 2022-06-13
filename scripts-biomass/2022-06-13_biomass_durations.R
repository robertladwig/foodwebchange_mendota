# RRR

phyto <- readRDS(file = "robin-data/2022-06-09_annual_patterns/interp_table.rds")



colnames(phyto)
rownames(phyto) <- phyto$day
phyto <- phyto[ ,-1]
phyto <- as.matrix(phyto)
phyto[1:3,1:3]

mean.biomass <- apply(X = phyto, MARGIN = 2, FUN = mean, na.rm = T)
boxplot(mean.biomass)
boxplot(phyto)
boxplot(as.vector(phyto))

high.biomass <- 5


