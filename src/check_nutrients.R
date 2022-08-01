# install.packages("devtools")
# devtools::install_github("hdugan/NTLlakeloads")
library(NTLlakeloads)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)

# Use these objects names, hardcoded at the moment
LTERtemp = loadLTERtemp() # Download NTL LTER data from EDI
LTERnutrients = loadLTERnutrients() # Download NTL LTER data from EDI
LTERions = loadLTERions() # Download NTL LTER data from EDI

availableVars()

df.pH = weeklyInterpolate(lakeAbr = 'ME', var = 'ph', maxdepth = 24, 
                          constrainMethod = 'zero', setThreshold = 0.1, printFigs = F)

plotTimeseries(df.pH$weeklyInterpolated, var = 'ph')

df.tp = weeklyInterpolate(lakeAbr = 'ME', var = 'drp_sloh', maxdepth = 24, 
                          constrainMethod = 'zero', setThreshold = 0.1, printFigs = F)

plotTimeseries(df.tp$weeklyInterpolated, var = 'drp_sloh')

df.tn = weeklyInterpolate(lakeAbr = 'ME', var = 'no3no2_sloh', maxdepth = 24, 
                          constrainMethod = 'zero', setThreshold = 0.1, printFigs = F)

plotTimeseries(df.tn$weeklyInterpolated, var = 'no3no2_sloh')

df.sil = weeklyInterpolate(lakeAbr = 'ME', var = 'drsif_sloh', maxdepth = 24, 
                          constrainMethod = 'zero', setThreshold = 0.1, printFigs = F)

plotTimeseries(df.sil$weeklyInterpolated, var = 'drsif_sloh')


hypso <- read_csv('../data/LakeEnsemblR_bathymetry_standard.csv')
H <- hypso$Depth_meter
A <- hypso$Area_meterSquared

areas <- approx(H, A, unique(df.ME$weeklyInterpolated$depth))$y
depths = unique(df.ME$weeklyInterpolated$depth)
bath = data.frame('depths' = depths, 'areas' = areas)

df.ph = data.frame('datetime' = unique(df.pH$weeklyInterpolated$date), 'ph' = NA)
for (i in (df.ph$datetime)){
  data = df.pH$weeklyInterpolated %>%
    dplyr::filter(date == i, depth <= 13) 
  d.bath = bath %>%
    dplyr::filter(depths <= 13)
  df.ph$ph[match(i,df.ph$datetime)] = ( data$var %*% d.bath$areas ) / sum(d.bath$areas)
}

df.phos = data.frame('datetime' = unique(df.tp$weeklyInterpolated$date), 'tp_surf' = NA, 'tp_bottom' = NA)
for (i in (df.phos$datetime)){
  data = df.tp$weeklyInterpolated %>%
    dplyr::filter(date == i, depth <= 13) 
  d.bath = bath %>%
    dplyr::filter(depths <= 13)
  df.phos$tp_surf[match(i,df.phos$datetime)] = ( data$var %*% d.bath$areas ) / sum(d.bath$areas)
  
  data = df.tp$weeklyInterpolated %>%
    dplyr::filter(date == i, depth > 13) 
  d.bath = bath %>%
    dplyr::filter(depths > 13)
  df.phos$tp_bottom[match(i,df.phos$datetime)] = ( data$var %*% d.bath$areas ) / sum(d.bath$areas)
  
}

df.nitrate = data.frame('datetime' = unique(df.tp$weeklyInterpolated$date), 'tn_surf' = NA, 'tn_bottom' = NA)
for (i in (df.nitrate$datetime)){
  data = df.tn$weeklyInterpolated %>%
    dplyr::filter(date == i, depth <= 13) 
  d.bath = bath %>%
    dplyr::filter(depths <= 13)
  df.nitrate$tn_surf[match(i,df.nitrate$datetime)] = ( data$var %*% d.bath$areas ) / sum(d.bath$areas)
  
  data = df.tn$weeklyInterpolated %>%
    dplyr::filter(date == i, depth > 13) 
  d.bath = bath %>%
    dplyr::filter(depths > 13)
  df.nitrate$tn_bottom[match(i,df.nitrate$datetime)] = ( data$var %*% d.bath$areas ) / sum(d.bath$areas)
  
}


df.silica = data.frame('datetime' = unique(df.sil$weeklyInterpolated$date), 'sil' = NA)
for (i in (df.silica$datetime)){
  data = df.sil$weeklyInterpolated %>%
    dplyr::filter(date == i, depth <= 13) 
  d.bath = bath %>%
    dplyr::filter(depths <= 13)
  df.silica$sil[match(i,df.silica$datetime)] = ( data$var %*% d.bath$areas ) / sum(d.bath$areas)
}

start <- read_csv('../output/stratification_start.csv')
end <- read_csv('../output/stratification_end.csv')

df = data.frame('year' =strat$year, 'pH' = NA, 'PO4.P_surf' = NA, 'PO4.P_bot' = NA, 'NO3.NO2.N_surf' = NA, 'NO3.NO2.N_bot' = NA, 'RSi' = NA)
for (i in df$year){
  st = start %>%
    dplyr::filter(year == i) %>%
    mutate(doy = mean(lubridate::yday(linear),lubridate::yday(constant.high),lubridate::yday(constant.low),lubridate::yday(spline)))
  en = end %>%
    dplyr::filter(year == i) %>%
    mutate(doy = mean(lubridate::yday(linear),lubridate::yday(constant.high),lubridate::yday(constant.low),lubridate::yday(spline)))
  
  idx = match(i,df$year)
  
  df1 = df.ph %>%
    mutate(year = year(datetime),doy = lubridate::yday(datetime)) %>%
    dplyr::filter(year == i) %>%
    dplyr::filter(doy >= st$doy & doy <= en$doy) %>%
    summarise(mean = mean(ph))
  df$pH[idx] = as.numeric(df1)
  
  df2 = df.phos %>%
    mutate(year = year(datetime),doy = lubridate::yday(datetime)) %>%
    dplyr::filter(year == i) %>%
    dplyr::filter(doy >= st$doy & doy <= en$doy) %>%
    summarise(surf = mean(tp_surf), bot = mean(tp_bottom))
  df$'PO4.P_surf'[match(i,df$year)] = as.numeric(df2[1])
  df$'PO4.P_bot'[match(i,df$year)] = as.numeric(df2[2])
  
  df3 = df.nitrate %>%
    mutate(year = year(datetime),doy = lubridate::yday(datetime)) %>%
    dplyr::filter(year == i) %>%
    dplyr::filter(doy >= st$doy & doy <= en$doy) %>%
    summarise(surf = mean(tn_surf), bot = mean(tn_bottom))
  df$'NO3.NO2.N_surf'[match(i,df$year)] = as.numeric(df3[1])
  df$'NO3.NO2.N_bot'[match(i,df$year)] = as.numeric(df3[2])
  
  df4 = df.silica %>%
    mutate(year = year(datetime),doy = lubridate::yday(datetime)) %>%
    dplyr::filter(year == i) %>%
    dplyr::filter(doy >= st$doy & doy <= en$doy) %>%
    summarise(mean = mean(sil))
  df$RSi[match(i,df$year)] = as.numeric(df4)
}

write_csv(df, '../output/nutrients.csv')
