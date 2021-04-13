# Calculations, Background for TCO: Resilient Flooring calculator.  
# https://averysaurus.shinyapps.io/tco_res_floors/
# Avery Richards, UC Berkeley School of Public Health

# Install and load required packages. 
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

## Cost of Square feet per hour, per procedure from ISSA datasheet. 
# Import datasheet.
floor_time <- read_csv("https://raw.githubusercontent.com/Averysaurus/TCO_shiny_R/main/floor_labor_data.csv")
pad_cost <- read_csv("https://raw.githubusercontent.com/Averysaurus/TCO_shiny_R/main/TCO_pad_cost.csv")

### Cost of consumables. 

# Each case of QC 34 Neutral Floor Cleaner yields 176 gallons of ready to use product, total coverage per case is 70,400 square feet. Price is $48.72/case.

cleaner_cost_per_ft <- 48.72 / 70400
buff_pad_cost <- 5.398
strip_pad_cost <- 9.399

buff_avg <- 
  pad_cost %>% 
  filter(procedure == "buffing")
buff_pad_avg_ft <- mean(buff_avg$avg_sq_ft)
buff_pad_avg_cost_ft <- 5.398 / buff_pad_avg_ft

strip_avg <- 
  pad_cost %>% 
  filter(procedure == "stripping")
strip_pad_avg_ft <- mean(strip_avg$avg_sq_ft)
strip_pad_avg_cost_ft <- 9.399 / strip_pad_avg_ft

# Select procedure category based on narrative label 
spray_buff <- 
  floor_time %>% 
  filter(
    str_detect(procedure, "Spray Buff"))
# Average square feet per hour of procedure category. 
sb_mean_ft <- mean(spray_buff$sq_ft_per_hour)
# Divide labor cost by average square feet per hour, multiplied by frequency of procedure per year. 
sb_cost_per_ft <- ((13.92 / sb_mean_ft) + cleaner_cost_per_ft +
                     buff_pad_avg_cost_ft) * 52

# Continue pattern for other four procedures. 
dry_buff <- 
  floor_time %>% 
  filter(
    str_detect(procedure, "Dry Buff"))

db_mean_ft <- mean(dry_buff$sq_ft_per_hour) 
db_cost_per_ft <- ((13.92 / db_mean_ft) + buff_pad_avg_cost_ft) * 52

strip <- 
  floor_time %>% 
  filter(
    str_detect(procedure, "Strip"))
stp_mean_ft <- mean(strip$sq_ft_per_hour) 
stp_cost_per_ft <- ((13.92 / stp_mean_ft) + cleaner_cost_per_ft +
                      strip_pad_avg_cost_ft) * 12

finish <- 
  floor_time %>% 
  slice(1:5)
fin_mean_ft <- mean(finish$sq_ft_per_hour) 
fin_cost_per_ft <- 13.92 / fin_mean_ft 

seal <- 
  floor_time %>% 
  filter(
    str_detect(procedure, "Seal"))
seal_mean_ft <- mean(seal$sq_ft_per_hour) 
seal_cost_per_ft <- 13.92 / seal_mean_ft 

