## code to prepare `green_avg` dataset goes here

library(dplyr)

url1 <- "https://github.com/owid/co2-data/raw/master/owid-co2-data.xlsx"
httr::GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
Greenhouse_Gas_Emissions <- readxl::read_excel(tf, 1L)

green_avg <-  Greenhouse_Gas_Emissions %>% 
  mutate(decade = floor(year/10)*10) %>% 
  filter(decade>=1990) %>% 
  group_by(country, decade) %>% 
  mutate(across(where(is.numeric), as.numeric)) %>%
  summarise(across(
    .cols = is.numeric,
    .fns = list(Mean = mean, Median= median, SD = sd, IQR= IQR), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))

usethis::use_data(green_avg, overwrite = TRUE)
usethis::use_data(Greenhouse_Gas_Emissions, overwrite = TRUE)
