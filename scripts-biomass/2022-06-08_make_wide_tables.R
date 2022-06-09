# RRR
# pull out just the info of interest from LTER data, and make it a wide table.
# not sure if biovolume or biomass is a better metric. 
# maybe biomass, people seem to talk about that more? simple units?

library(tidyr)
library(lubridate)

phyto <- readRDS(file = "data/1_LTER_Mendota_phyto.rds")

created.file <- "robin-data/2022-06-08_phyto_list.rds"

# ----

colnames(phyto)

phyto.division <- pivot_wider(data = phyto, id_cols = "lter.division", names_from = "Date", values_from = "Biomass.mg.L", values_fn = sum)
phyto.genus <- pivot_wider(data = phyto, id_cols = "lter.genus", names_from = "Date", values_from = "Biomass.mg.L", values_fn = sum)
phyto.name <-pivot_wider(data = phyto, id_cols = "lter.taxa_name", names_from = "Date", values_from = "Biomass.mg.L", values_fn = sum)

# stupid tibbles

un.tibble.it <- function(stupid.phyto){
  stupid.phyto <- as.data.frame(stupid.phyto)
  row.names(stupid.phyto) <- stupid.phyto[ ,1]
  stupid.phyto <- stupid.phyto[ ,-1]
  stupid.phyto <- as.matrix(stupid.phyto)
  return(stupid.phyto)
}

phyto.division <- un.tibble.it(stupid.phyto = phyto.division)
phyto.genus <- un.tibble.it(stupid.phyto = phyto.genus)
phyto.name <- un.tibble.it(stupid.phyto = phyto.name)

phyto.total <- data.frame("date" = colnames(phyto.division), "biomass" = colSums(phyto.division))
phyto.total$date <- parse_date_time(x = phyto.total$date, orders = "ymd")

my.phyto <- list("tot" = phyto.total, "div" = phyto.division, "gen" = phyto.genus, "tax" = phyto.name)

# ----
cat(created.file)
saveRDS(object = my.phyto, file = created.file)
