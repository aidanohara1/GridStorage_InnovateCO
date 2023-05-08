# Aidan EDA/Development zone

revByutility <- read.csv("Electricity_Revenue_by_Utility_in_Colorado.csv")
View(revByutility)
# https://data.colorado.gov/Business/Electricity-Revenue-by-Utility-in-Colorado/gdh8-8pg4



CSA_Fuel_Use <- read_csv("Colorado_State_Agency_Fuel_Use_FY15_-_FY21.csv")
CSA_NatGas_Use <- read_csv("Colorado_State_Agency_Natural_Gas_Use_FY15_-_FY21.csv")
CSA_Renew_Use <- read_csv("Colorado_State_Agency_Renewable_Energy_Use_FY15_-_FY21.csv")

library(DataExplorer)

plot_bar(CSA_Fuel_Use)
plot_bar(CSA_NatGas_Use)
plot_bar(CSA_Renew_Use)

# https://www.eia.gov/energyexplained/us-energy-facts/

#https://css.umich.edu/publications/factsheets/energy/us-grid-energy-storage-factsheet

# https://www.watereducationcolorado.org/fresh-water-news/two-pumped-water-storage-projects-move-forward-in-colorado/

# https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates/2020-evaluation-estimates/2010s-state-total.html
library(tidyverse)
library(ggplot2)

CO_Energy_Cosumption <- read_csv("CO_Energy_Cosumption.csv")

CA_Energy_Cosumption <- read_csv("CA_Energy_Consumption.csv")
CA_Energy_Cosumption <- CA_Energy_Cosumption[,1:28]

library(readxl)
nst_est2020 <- read_excel("nst-est2020.xlsx", 
                            skip = 3)

nst_est2020 <- nst_est2020 %>% rename(state = `...1`,
                                      `2020` = `April 1`) %>%
  mutate(state = sub("^\\.", "", state)) %>%
  select(-Census, -`Estimates Base`, -`July 1`)

COEnergy <- CO_Energy_Cosumption %>% rename(nat_gas = `Natural Gas`,
                                            renew_total = `Total...25`,
                                            ff_total = `FF Total`,
                                            biomass = `Total...21`,
                                            total = `Total...28`) %>%
  filter(Year>=2010) %>%
  mutate(logCoal = log(Coal),
         logNatGas = log(nat_gas),
         logSolar = log(Solar),
         logWind = log(Wind))

CAEnergy <- CA_Energy_Cosumption %>% 
  filter(Year>=2010) %>%
  mutate(logCoal = log(coal),
         logNatGas = log(nat_gas),
         logSolar = log(solar),
         logWind = log(wind))


coPop <- nst_est2020 %>%
  filter(state == "Colorado") %>%
  pivot_longer(cols = -state, names_to = "Year") %>%
  mutate(Year = as.double(Year)) %>%
  left_join(COEnergy, by = "Year") %>%
  rename(population = value)


caPop <- nst_est2020 %>%
  filter(state == "California") %>%
  pivot_longer(cols = -state, names_to = "Year") %>%
  mutate(Year = as.double(Year)) %>%
  left_join(CAEnergy, by = "Year") %>%
  rename(population = value)
# View the merged dataset
#merged_data

colnames(coPop) = colnames(caPop)

plotSet <- rbind(caPop,coPop)%>%
  rename(renewable_total = renewable_otal) %>%
  select(state,
         Year,
         coal,
         nat_gas,
         ff_total,
         nuclear_electric_power,
         hydro_power,
         wood_and_waste,
         geo_thermal,
         solar,
         wind,
         renewable_total)

sourceSet <- plotSet %>% select(-ff_total,
                                -renewable_total)

totalSet <- plotSet %>% select(Year,
                               state,
                               ff_total,
                               renewable_total)

ggplot(plotSet, aes(x = Year, color = state, col = variable)) +
  # geom_line(aes(y = coal, color = "coal"), linewidth = 1) +
  # geom_smooth(aes(y = coal, color = "coal"), method = "lm", se = TRUE, linetype = "dashed") +
  # geom_line(aes(y = wind, color = "wind"), linewidth = 1) +
  # geom_smooth(aes(y = wind, color = "wind"), method = "lm", se = TRUE, linetype = "dashed") +
  # geom_line(aes(y = solar, color = "solar"), linewidth = 1) +
  # geom_smooth(aes(y = solar, color = "solar"), method = "lm", se = TRUE, linetype = "dashed") +
  geom_line() +
  facet_wrap(~state, ncol = 2, scales = "free_y") +
  labs(title = "Consumption vs. Year by State",
       x = "Year",
       y = "Coal",
       color = "State") +
  # scale_color_manual(values = c("coal" = "red", 
  #                               "wind" = "blue", 
  #                               "solar" = "green")) +
  theme_bw()


library(viridis)
library(hrbrthemes)
melter <- melt(sourceSet, id = c('Year','state'))
melter2 <- melt(totalSet, id = c('Year','state'))

p <- melter %>% 
  ggplot(aes(x=Year, y=value, fill=variable, text=variable)) +
  facet_wrap(~state, ncol = 2, scales = "free_y") +
  scale_fill_viridis(discrete = TRUE) +
  geom_area( ) +
  theme(legend.position="none") +
  ggtitle("consumption") 

q <- melter2 %>% 
  ggplot(aes(x=Year, y=value, fill=variable, text=variable)) +
  facet_wrap(~state, ncol = 2, scales = "free_y") +
  scale_fill_viridis(discrete = TRUE) +
  geom_area( ) +
  theme(legend.position="none") +
  ggtitle("consumption") 
# Turn it interactive
p <- ggplotly(p, tooltip="text")
p

ggplot(COEnergy, aes(x = Year)) +
  geom_line(aes(y = Coal, color = "Coal")) +
  geom_smooth(aes(y = Coal, color = "Coal"), method = "lm", se = FALSE) +
  geom_line(aes(y = nat_gas, color = "Natural Gas")) +
  geom_smooth(aes(y = nat_gas, color = "Natural Gas"), method = "lm", se = FALSE) +
  geom_line(aes(y = Solar, color = "Solar")) +
  geom_smooth(aes(y = Solar, color = "Solar"), method = "lm", se = FALSE) +
  labs(title = "Yearly Energy Consumption by Source",
       x = "Year",
       y = "Energy Consumption (in units)") +
  scale_color_manual(values = c("Coal" = "red", "Natural Gas" = "blue", "Solar" = "green")) +
  theme_classic()

ggplot(COEnergy, aes(x = Year)) +
  geom_line(aes(y = logCoal, color = "Coal")) +
  geom_smooth(aes(y = logCoal, color = "Coal"), method = "lm", se = FALSE) +
  geom_line(aes(y = logNatGas, color = "Natural Gas")) +
  geom_smooth(aes(y = logNatGas, color = "Natural Gas"), method = "lm", se = FALSE) +
  geom_line(aes(y = logSolar, color = "Solar")) +
  geom_smooth(aes(y = logSolar, color = "Solar"), method = "lm", se = FALSE) +
  geom_line(aes(y = logWind, color = "Wind")) +
  geom_smooth(aes(y = logWind, color = "Wind"), method = "lm", se = FALSE) +
  labs(title = "Yearly Energy Consumption by Source",
       x = "Year",
       y = "Energy Consumption (in units)") +
  scale_color_manual(values = c("Coal" = "red", 
                                "Natural Gas" = "blue", 
                                "Solar" = "green",
                                "Wind" = "orange")) +
  theme_classic()

ggplot(COEnergy, aes(x = Year)) +
  geom_line(aes(y = logCoal, color = "Coal")) +
  geom_smooth(aes(y = logCoal, color = "Coal"), method = "lm", se = FALSE) +
  geom_line(aes(y = logNatGas, color = "Natural Gas")) +
  geom_smooth(aes(y = logNatGas, color = "Natural Gas"), method = "lm", se = FALSE) +
  geom_line(aes(y = logSolar, color = "Solar")) +
  geom_smooth(aes(y = logSolar, color = "Solar"), method = "lm", se = FALSE) +
  geom_line(aes(y = logWind, color = "Wind")) +
  geom_smooth(aes(y = logWind, color = "Wind"), method = "lm", se = FALSE) +
  labs(title = "Yearly Energy Consumption by Source",
       x = "Year",
       y = "Energy Consumption (in units)") +
  scale_color_manual(values = c("Coal" = "red", 
                                "Natural Gas" = "blue", 
                                "Solar" = "green",
                                "Wind" = "orange")) +
  theme_classic()

ggplot(COEnergy, aes(x = Year)) +
  geom_line(aes(y = ff_total, color = "fossil fuel")) +
  geom_smooth(aes(y = ff_total, color = "fossil fuel"), method = "lm", se = FALSE) +
  geom_line(aes(y = renew_total, color = "renewables")) +
  geom_smooth(aes(y = renew_total, color = "renewables"), method = "lm", se = FALSE) +
  labs(title = "Yearly Energy Consumption by Source",
       x = "Year",
       y = "Energy Consumption (in units)") +
  scale_color_manual(values = c("renewables" = "red", 
                                "fossil fuel" = "blue")) +
  theme_classic()


