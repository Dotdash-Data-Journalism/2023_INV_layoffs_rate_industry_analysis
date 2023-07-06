library(httr)
library(readr)
library(dplyr)
library(magrittr)
library(lubridate)
library(janitor)
library(stringr)
library(zoo)

### Functions ###

# Custom function to fetch BLS data from flat file database
get_bls_data <- function(url, email) {
  bls_res <- GET(url = url, user_agent(email))
  stop_for_status(bls_res)
  
  bls_content <- content(bls_res, 
                         as = "parsed",
                         type = "text/tab-separated-values",
                         encoding = "UTF-8",
                         col_names = T,
                         col_types = cols(.default = col_character()),
                         trim_ws = T
  )
  return(bls_content)
  
}


### Analysis ###
# Grabbing seasonally-adjusted layoffs and separations rate through time

layoffs_discharges <- get_bls_data(
  "https://download.bls.gov/pub/time.series/jt/jt.data.6.LayoffsDischarges",
  "anesta@dotdashmdp.com"
             ) %>% 
  mutate(
    date = base::as.Date(paste0(year, 
                                "-",
                                str_sub(period, 2, 3),
                                "-01")),
    seas_adj_code = str_sub(series_id, 3, 3),
    industry_code = str_sub(series_id, 4, 9),
    state_code = str_sub(series_id, 10, 11),
    area_code = str_sub(series_id, 12, 16),
    size_class_code = str_sub(series_id, 17, 18),
    data_element_code = str_sub(series_id, 19, 20),
    rate_level_code = str_sub(series_id, 21, 21)
  ) %>% 
  select(-c(year, period))
  
layoffs_discharges_rate <- layoffs_discharges %>% 
  filter(industry_code == "000000",
         state_code == "00",
         area_code == "00000",
         size_class_code == "00",
         data_element_code == "LD",
         rate_level_code == "R",
         seas_adj_code == "S")

# Getting 2019 layoffs and discharge annual average
prepan_ld_avg <- layoffs_discharges_rate %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(year(date) == 2019L) %>% 
  pull(value) %>% 
  mean()

## Visualization 1: Line graph of non-farm seasonally adjusted layoffs & 
## discharges rate from 2001 to present.

# Calculating layoffs & discharges three month moving average 
# without March and April 2020 outliers.
ld_over_time_dw <- layoffs_discharges_rate %>% 
  filter(date >= base::as.Date("2001-01-01"), !(date %in% c(
    base::as.Date("2020-03-01"), base::as.Date("2020-04-01")
  ))) %>%
  mutate(
     `Layoffs and Discharges Rate` = rollmean(x = as.numeric(value),
                                                 k = 3,
                                                 fill = "NA",
                                                 align = "center")) %>% 
  select(date, `Layoffs and Discharges Rate`) %>% 
  bind_rows(tibble(
    date = c(base::as.Date("2020-03-01"), base::as.Date("2020-04-01")),
    `Layoffs and Discharges Rate` = c(NA_real_, NA_real_)
  )) %>% 
  arrange(date)


write_csv(ld_over_time_dw, "./visualizations/ld_over_time_dw.csv")

## Visualization 2: Range plot of major sector layoff & discharge rate averages
## from May '22 to '23 compared to 2001 to present average

# Getting BLS JOLTS industry name file
jt_industry <- get_bls_data(
  "https://download.bls.gov/pub/time.series/jt/jt.industry",
             "anesta@dotdashmdp.com"
  )

# Calculating May '22 to '23 average layoffs & discharge rate by sector
ld_by_sector_may_22_23_avg <- layoffs_discharges %>% 
  filter(industry_code %in% c("000000",
                              "110099",
                              "230000",
                              "300000",
                              "400000",
                              "510000",
                              "520000",
                              "530000",
                              "540099",
                              "600000",
                              "700000",
                              "810000",
                              "900000"),
         state_code == "00",
         area_code == "00000",
         size_class_code == "00",
         data_element_code == "LD",
         rate_level_code == "R",
         seas_adj_code == "S",
         between(date, 
                 base::as.Date("2022-05-01"),
                 base::as.Date("2023-05-01"))) %>% 
  inner_join(jt_industry, by = "industry_code") %>% 
  group_by(industry_text) %>% 
  summarize(`May '22 - May '23 Average` = mean(as.numeric(value))) %>% 
  arrange(desc(`May '22 - May '23 Average`))

# Finding Dec. '00 - Dec. '19 (pre-pandemic average of all data JOLTS has)
ld_by_sector_dec_00_dec_19_avg <- layoffs_discharges %>% 
  filter(industry_code %in% c("000000",
                              "110099",
                              "230000",
                              "300000",
                              "400000",
                              "510000",
                              "520000",
                              "530000",
                              "540099",
                              "600000",
                              "700000",
                              "810000",
                              "900000"),
         state_code == "00",
         area_code == "00000",
         size_class_code == "00",
         data_element_code == "LD",
         rate_level_code == "R",
         seas_adj_code == "S",
         between(date, 
                 base::as.Date("2000-12-01"),
                 base::as.Date("2019-12-01"))) %>% 
  inner_join(jt_industry, by = "industry_code") %>% 
  group_by(industry_text) %>% 
  summarize(`Dec. '00 - Dec. '19 Average` = mean(as.numeric(value))) %>% 
  arrange(desc(`Dec. '00 - Dec. '19 Average`))

ld_by_sector_all_time_avg_vs_may_22_23_avg_dw <- inner_join(
  ld_by_sector_may_22_23_avg,
  ld_by_sector_dec_00_dec_19_avg,
  by = "industry_text"
) %>% 
  mutate(diff = `Dec. '00 - Dec. '19 Average` - `May '22 - May '23 Average`) %>% 
  arrange(diff) %>% 
  select(-diff)


write_csv(ld_by_sector_all_time_avg_vs_may_22_23_avg_dw, 
          "./visualizations/ld_by_sector_all_time_avg_vs_may_22_23_avg_dw.csv")


# BLS Page on Information Sector
# https://www.bls.gov/iag/tgs/iag51.htm
# Largest industries & companies by Information sector
# https://www.naics.com/six-digit-naics/?v=2022&code=51


## Visualization 3: Map of states by proportion of non-farm workforce
## that is in information sector based on 2022 annual averages

# By-state sector composition: https://download.bls.gov/pub/time.series/sm/sm.data.0.Current

# Getting proportion of information sector to non-farm payroll workforce
# in 2022 by state.
sae_current <- get_bls_data(
  "https://download.bls.gov/pub/time.series/sm/sm.data.0.Current",
  "anesta@dotdashmdp.com"
)

sae_state <- get_bls_data(
  "https://download.bls.gov/pub/time.series/sm/sm.state",
  "anesta@dotdashmdp.com"
)

info_sector_by_state_metro <- sae_current %>% 
  filter(period == "M13",
         year == "2022") %>% 
  mutate(
    seas_adj_code = str_sub(series_id, 3, 3),
    state_code = str_sub(series_id, 4, 5),
    area_code = str_sub(series_id, 6, 10),
    industry_code = str_sub(series_id, 11, 18),
    data_type_code = str_sub(series_id, 19, 20),
    value = as.numeric(value)
  ) %>% 
  filter(
    seas_adj_code == "U",
    industry_code %in% c("00000000",
                         "50000000"),
    data_type_code == "01"
  ) %>% 
  group_by(state_code, area_code) %>% 
  summarize(
    nonfarm_size = value[industry_code == "00000000"],
    info_sector_prop = (
      value[industry_code == "50000000"] / value[industry_code == "00000000"]
    ) * 100
  ) %>% 
  ungroup()
  
write_csv(info_sector_by_state_metro,
          "./data/info_sector_by_state_metro.csv")

info_sector_by_state_dw <- info_sector_by_state_metro %>% 
  filter(area_code == "00000") %>% 
  select(-c(area_code, nonfarm_size)) %>% 
  inner_join(sae_state, by = "state_code")

write_csv(info_sector_by_state_dw, 
          "./visualizations/info_sector_by_state_dw.csv")

## Visualization 4: Making line graph of consumer media exposure to 
## negative employment news over time.
## UMich Survey of Consumers Employment Table 24 monthly data
## https://data.sca.isr.umich.edu/data-archive/mine.php


sca_table24 <- read_csv("./data/sca-table24-on-2023-Jul-05.csv",
         col_names = T,
         skip = 1,
         col_types = cols(.default = col_character()))


sca_table24_dw <- sca_table24 %>% 
  mutate(date = base::as.Date(paste0(Year, "-", Month, "-01"))) %>% 
  select(date, contains("employment")) %>% 
  rename_with(.cols = 2:3, .fn = ~str_replace(.x, "\\<br\\>", " ")) %>% 
  rename(`Net Positive Employment News Heard` = `Relative: employment`) %>% 
  mutate(across(-date, function(col) rollmean(x = as.numeric(col),
                                              k = 3,
                                              fill = NA_real_,
                                              align = "center")))

write_csv(sca_table24_dw, "./visualizations/sca_table24_dw.csv")



