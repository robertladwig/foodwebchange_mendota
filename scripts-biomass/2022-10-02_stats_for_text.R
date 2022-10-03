# RRR
options(scipen = 999)


# After the spiny water flea invasion, spring average biomass increased significantly from 1 ± 1 to 3 ± 2 (p = 0.03)
# During ice-on, average biomass also increased significantly (p = 0.02), however the total biomass is much lower during this season.
# We found that after the spiny water flea invasion, the lag between stratification onset and anoxia onset decreased by almost 2 weeks, from 51 ± 9 days to 39 ± 15 days (p = 0.02)
x <- read.csv(file = "plots/2022-10-02_spring_importance_plot/mean_annual_biomass_by_season-stats.csv")
x

# In accordance with the typical phenology, diatoms were predominantly responsible for the increase in spring biomass, 
# comprising the majority of the phytoplankton community before and after the spiny water flea invasion (67 ± 20 \% and 65 ± 25 \% respectively) <- readRDS(file = "robin-data/2022-09-28_phyto_stats_by_taxon/taxon_invasion_group_averages_perc.rds")
x <- readRDS(file = "robin-data/2022-10-02_phyto_stats_by_taxon/taxon_invasion_group_averages_perc.rds")
x$spring

# Diatom biomass in the spring increased by 2-fold (p = 0.1)
x <- readRDS(file = "robin-data/2022-10-02_phyto_stats_by_taxon/taxon_invasion_group_averages_mg_L.rds")
x$spring
x$spring$post[1] / x$spring$pre[1] # 2-fold, p = .1

# up with diatoms:
# Chlorophyta (green algae) remained at 5-9 percent abundance but increased 4-fold, from 0.04 ± .02 to 0.1 ± 0.1 mg/L (p = 0.003)
# Blue-green algae was more variable, but increased by 6 fold, from 0.03 ± 0.03 to 0.2 ±  0.2 (p = 0.06)
# Pyrrhophyta (dinoflagellates) remained at 1-3 percent of the community but increased by 3-fold, from 0.02 ± 0.03 to 0.05 ± 0.03 (p = 0.02)
# down:
# Cryptophyta (cryptophytes) decreased from 17 ± 12 to 9 ± 6 \% of the community (p = 0.04)
# Chrysophyta (golden algae) decreased from 3 ± 2 to 1 ± 1 \% of the community (p = 0.01
x <- readRDS(file = "robin-data/2022-10-02_phyto_stats_by_taxon/taxon_invasion_group_averages_perc.rds")
x$spring
y <- readRDS(file = "robin-data/2022-10-02_phyto_stats_by_taxon/taxon_invasion_group_averages_mg_L.rds")
y$spring

y$spring$post[4] / y$spring$pre[4] # chlorophyta 4-fold, p = .003
y$spring$post[3] / y$spring$pre[3] # cyanophyta 6-fold, p = .06
y$spring$post[6] / y$spring$pre[6] # Pyrrhophyta 3-fold, p = .02
y$spring$post[6] / y$spring$pre[6] # Pyrrhophyta 3-fold, p = .02

# An anosim analysis confirmed that the community composition was more similar during years with the same invasion status than between all years (anosim significance = 0.02)
x <- read.csv("plots/2022-10-02_NMDS_plots/anosim_year_averages.csv")

# these changes were modest enough that phytoplankton diversity did not significantly change, as measured by either the Shannon or Simpson diversity metrics
x <- readRDS("robin-data/2022-10-02_diversity_metrics/diversity_by_invasion.rds")
x$spring

