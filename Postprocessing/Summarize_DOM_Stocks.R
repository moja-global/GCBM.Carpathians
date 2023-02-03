# ----------------------------
# Make Tables and Figures for the GCBM estimations on DOM pools
# ----------------------------

#------------------------------------------------------
# Library management

# Necessary libraries
library(ggplot2) # produce figures
library(readr) # Read data
library(tidyr) # Data cleaning
library(dplyr) # Data processing
library(RSQLite) # Connect with the SQLite database
library(writexl) # Write tables in excel format

# Avoid scientific notation
options(scipen=10000)

#------------------------

# List the directories of the different GCBM runs
# Here only one directory is listed, but mode than one can be made to make a sensitivity analysis
# e.g. runs <- c('../Standalone_GCBM','../Standalone_GCBM_decaymod','../Standalone_GCBM_turnovermod')
runs <- c('../Standalone_GCBM')


# Make a named vector to point the name you want each run to have in the graph
# Here only one name is listed, but mode than one can be made to make a sensitivity analysis
# e.g. names_runs <- c('Default','Modified Decay parameters',Modified Decay, Turnover and Spinup parameters')
names_runs <- c('Modified Decay, Turnover and Spinup parameters')
names_runs <- c('../Standalone_GCBM' = 'Modified Decay, Turnover and Spinup parameters')

#Loop though the runs
for (run in runs) {
  
  # Path to the database
  path_db<-paste0(run,"/processed_output/compiled_gcbm_output.db")
  #----------------------
  
  # Connect to the database
  conn <- dbConnect(RSQLite::SQLite(), path_db)
  
  # List the tables in the databases
  dbListTables(conn)
  
  # Pool indicators
  pool_ind<-dbGetQuery(conn, "SELECT * FROM v_pool_indicators")
  
  
  # Calculate the pools total carbon for forests per lifezone and year
  pools_run<-pool_ind %>% filter(indicator %in% c("Total Biomass","Deadwood","Litter","Soil Carbon")) %>% 
    group_by(year,indicator,LifeZone) %>% 
    summarize(pool_tc_sum=sum(pool_tc))
  
  
  # Get the areas for each lifezone
  age_ind<-dbGetQuery(conn, "SELECT * FROM v_age_indicators")
  areas_run<-age_ind %>%  
    group_by(year,LifeZone) %>% 
    summarize(area_sum=sum(area)) %>% 
    ungroup()
  
  # Divide the DOM values per area to obtain ton/ha values
  pools_run_area <- left_join(pools_run,areas_run,by = c("year","LifeZone"))
  pools_run_area <- mutate(pools_run_area, pool_tc_per_ha = pool_tc_sum/area_sum)
  pools_run_area$run <- run
  
  # Recode the runs 
  pools_run_area$run <- as.character(recode(pools_run_area$run, !!!names_runs))
  
  
  # Make a compiled database
  if(exists("pools_full")){
    pools_full <- unique(rbind(pools_full, pools_run_area))
  } else {
    pools_full <- pools_run_area
  }
  
  dbDisconnect(conn)

}

# Check the recoding
unique(pools_full$run)

#Write full table
write_csv(pools_full,"./Tables/Pools_DOM_Sensitivity_full.csv")


# Make a Table with the DOM stocks every 5 years, from 0 to 10 years old
pools_summary <- pools_full %>% 
  filter(year %in% seq(2010,2020,by=5)) %>% 
  mutate(Age = year - 2010) 

for (ag in unique(pools_summary$Age)) {
  
  # Make a pools table for that specific age (ag)
  pools_forest <- pools_summary %>% 
    filter(Age == ag) %>% 
    select(indicator,LifeZone,pool_tc_per_ha,run) %>%
    pivot_wider(names_from = run,values_from = pool_tc_per_ha)
  
  # Write a table every 10 years
  write_csv(pools_forest,paste0("./Tables/Pools_DOM_Sensitivity_forest_",ag,"_years.csv"))

}

# Make figures for each life zone in Carpathians

# Boreal wet forest

p <- ggplot(filter(pools_full, LifeZone=="Boreal wet forest"), aes(x = year, y = pool_tc_per_ha, fill = indicator))+
  geom_area() +
  facet_grid(indicator~run,labeller=label_wrap_gen(width=7)) +
  ylab("Carbon Stock (ton C / ha)") +
  scale_fill_manual(values = c("darkgoldenrod4","chartreuse3","gray14","forestgreen")) +
  ggtitle("Carbon Stocks of Boreal wet forest (Carpathians) - GCBM Sensitivity analysis") +
  theme_bw(14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p

ggsave(filename=("./Figures/Carpathians_Sensitivity_BorealWet.png"), width = 300, height = 180, units = "mm", dpi = 300)


# Cool temperate moist forest
p <- ggplot(filter(pools_full,LifeZone=="Cool temperate moist forest"),aes(x = year,y = pool_tc_per_ha, fill = indicator))+
  geom_area() +
  facet_grid(indicator~run,labeller = label_wrap_gen(width = 7)) +
  ylab("Carbon Stock (ton C / ha)") +
  scale_fill_manual(values = c("darkgoldenrod4","chartreuse3","gray14","forestgreen")) +
  ggtitle("Carbon Stocks of Cool temperate moist forest (Carpathians) - GCBM Sensitivity analysis") +
  theme_bw(14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p

ggsave(filename=("./Figures/Carpathians_Sensitivity_CoolTemperateMoist.png"), width = 300, height = 180, units = "mm", dpi = 300)


# Cool temperate steppe
p <- ggplot(filter(pools_full,LifeZone=="Cool temperate steppe"), aes(x = year,y = pool_tc_per_ha, fill = indicator))+
  geom_area() +
  facet_grid(indicator~run,labeller = label_wrap_gen(width = 7)) +
  ylab("Carbon Stock (ton C / ha)") +
  scale_fill_manual(values = c("darkgoldenrod4","chartreuse3","gray14","forestgreen")) +
  ggtitle("Carbon Stocks of Cool temperate steppe (Carpathians) - GCBM Sensitivity analysis") +
  theme_bw(14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p

ggsave(filename=("./Figures/Carpathians_Sensitivity_CoolTemperateSteppe.png"), width = 300, height = 180, units = "mm", dpi = 300)


# Cool temperate wet forest
p <- ggplot(filter(pools_full,LifeZone=="Cool temperate wet forest"),aes(x = year, y = pool_tc_per_ha, fill = indicator))+
  geom_area() +
  facet_grid(indicator~run,labeller = label_wrap_gen(width = 7)) +
  ylab("Carbon Stock (ton C / ha)") +
  scale_fill_manual(values = c("darkgoldenrod4","chartreuse3","gray14","forestgreen")) +
  ggtitle("Carbon Stocks of Cool temperate wet forest (Carpathians) - GCBM Sensitivity analysis") +
  theme_bw(14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p

ggsave(filename=("./Figures/Carpathians_Sensitivity_CoolTemperateWet.png"), width = 300, height = 180, units = "mm", dpi = 300)


# Polar rain tundra
p <- ggplot(filter(pools_full,LifeZone=="Polar rain tundra"),aes(x = year, y = pool_tc_per_ha, fill = indicator))+
  geom_area() +
  facet_grid(indicator~run,labeller = label_wrap_gen(width = 7)) +
  ylab("Carbon Stock (ton C / ha)") +
  scale_fill_manual(values = c("darkgoldenrod4","chartreuse3","gray14","forestgreen")) +
  ggtitle("Carbon Stocks of Polar rain tundra (Carpathians) - GCBM Sensitivity analysis") +
  theme_bw(14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p

ggsave(filename=("./Figures/Carpathians_Sensitivity_PolarRainTundra.png"), width = 300, height = 180, units = "mm", dpi = 300)


