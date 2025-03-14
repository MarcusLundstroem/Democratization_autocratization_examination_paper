# Load necessary libraries
library(tidyverse)
library(haven) # stata format on one of the datasets
library(plm)  # Panel Data Fixed Effects
library(lmtest)
library(sandwich)
library(stargazer) # for regression outputs
library(WDI) # GDP data
library(countrycode) # used for joins

# Load V-Dem Data
VDem <- readRDS("C:/Users/Marcu/OneDrive/Skrivbord/Masternivå/Masteruppsats/Inspiration, data/V-Dem-CY-FullOther-v14_rds/V-Dem-CY-Full+Others-v14.rds") %>%
  select(v2x_polyarchy, country_text_id, year)

# Load World Values Survey (WVS) Data
WVS <- readRDS("C:/Users/Marcu/OneDrive/Skrivbord/Masternivå/Democratization and autocratization/Trends_VS_1981_2022_rds_v4_0.rds") %>%
  select(S020, s002, S001, S009, COUNTRY_ALPHA, pwgt, E114, E117, Y002, Y001) %>%
  rename(year = S020) %>%
  mutate(year = as.numeric(year))

# Load GDP Data
gdp_data <- WDI(
  country = "all",
  indicator = c("NY.GDP.MKTP.CD", "SP.POP.TOTL"),  # GDP and population
  start = 1981,
  end = 2022,
  extra = TRUE
) %>%
  mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c")) %>%
  rename(GDP_current_USD = NY.GDP.MKTP.CD, population = SP.POP.TOTL)

# Merge Data
merged_data <- WVS %>%
  left_join(gdp_data, by = c("year" = "year", "COUNTRY_ALPHA" = "iso3c")) %>%
  left_join(VDem, by = c("year" = "year", "COUNTRY_ALPHA" = "country_text_id")) %>%
  select(year, COUNTRY_ALPHA, s002, S001, S009, pwgt, E114, E117, Y001, Y002, 
         GDP_current_USD, population, region, income, v2x_polyarchy) %>%
  mutate(GDP_per_capita = GDP_current_USD / population) %>%
  rename(GDP_current_USD = GDP_current_USD)

# Compute Country Counts per Region-Year
region_year_counts <- merged_data %>%
  group_by(region, year) %>%
  summarise(num_countries = n_distinct(COUNTRY_ALPHA), .groups = "drop")

# Identify Region-Years with at Least 5 Countries
valid_region_years <- region_year_counts %>%
  filter(num_countries >= 5) %>%
  select(region, year)

# Compute Regional Democracy (Only for Region-Years with 5+ Countries)
regional_democracy_data_filtered <- merged_data %>%
  semi_join(valid_region_years, by = c("region", "year")) %>%
  group_by(region, year) %>%
  summarise(regional_democracy = mean(v2x_polyarchy, na.rm = TRUE), .groups = "drop")

# Merge Updated Regional Democracy into Data
final_data_cleaned <- merged_data %>%
  left_join(regional_democracy_data_filtered, by = c("region", "year")) %>%
  mutate(across(everything(), ~ replace(., . %in% c(-1, -2, -3, -4, -5), NA))) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(log_GDP_per_capita = log(GDP_per_capita + 1))  # Log-transform GDP per capita

# Filter Countries with at Least 3 & 5 Years of Data Before Lagging
valid_countries_3y <- final_data_cleaned %>%
  group_by(COUNTRY_ALPHA) %>%
  summarise(num_years = n_distinct(year), .groups = "drop") %>%
  filter(num_years >= 3) %>%
  pull(COUNTRY_ALPHA)

valid_countries_5y <- final_data_cleaned %>%
  group_by(COUNTRY_ALPHA) %>%
  summarise(num_years = n_distinct(year), .groups = "drop") %>%
  filter(num_years >= 5) %>%
  pull(COUNTRY_ALPHA)

# 3-year lag
final_data_lagged_3y <- final_data_cleaned %>%
  filter(COUNTRY_ALPHA %in% valid_countries_3y) %>%
  arrange(COUNTRY_ALPHA, year) %>%
  group_by(COUNTRY_ALPHA) %>%
  mutate(
    lag3_regional_democracy = lag(regional_democracy, 3),
    lag3_E114 = lag(E114, 3),
    lag3_E117 = lag(E117, 3),
    lag3_Y002 = lag(Y002, 3),
    lag3_Y001 = lag(Y001, 3),
    log_lag3_GDP_per_capita = log(lag(GDP_per_capita, 3) + 1)
  ) %>%
  filter(!is.na(lag3_E114) & !is.na(lag3_E117) & !is.na(lag3_Y002) & 
           !is.na(lag3_Y001) & !is.na(log_lag3_GDP_per_capita) & !is.na(lag3_regional_democracy)) %>%
  ungroup()

# 5-year lag
final_data_lagged_5y <- final_data_cleaned %>%
  filter(COUNTRY_ALPHA %in% valid_countries_5y) %>%
  arrange(COUNTRY_ALPHA, year) %>%
  group_by(COUNTRY_ALPHA) %>%
  mutate(
    lag5_regional_democracy = lag(regional_democracy, 5),
    lag5_E114 = lag(E114, 5),
    lag5_E117 = lag(E117, 5),
    lag5_Y002 = lag(Y002, 5),
    lag5_Y001 = lag(Y001, 5),
    log_lag5_GDP_per_capita = log(lag(GDP_per_capita, 5) + 1)
  ) %>%
  filter(!is.na(lag5_E114) & !is.na(lag5_E117) & !is.na(lag5_Y002) & 
           !is.na(lag5_Y001) & !is.na(log_lag5_GDP_per_capita) & !is.na(lag5_regional_democracy)) %>%
  ungroup()

# OLS regressions
model_no_lag <- lm(v2x_polyarchy ~ E114 + E117 + Y002 + Y001 + log_GDP_per_capita + regional_democracy, data = final_data_cleaned)
model_3y <- lm(v2x_polyarchy ~ lag3_E114 + lag3_E117 + lag3_Y002 + lag3_Y001 + log_lag3_GDP_per_capita + lag3_regional_democracy, data = final_data_lagged_3y)
model_5y <- lm(v2x_polyarchy ~ lag5_E114 + lag5_E117 + lag5_Y002 + lag5_Y001 + log_lag5_GDP_per_capita + lag5_regional_democracy, data = final_data_lagged_5y)

# Fixed Effects (FE) Models
fe_model_no_lag <- plm(v2x_polyarchy ~ E114 + E117 + Y002 + Y001 + log_GDP_per_capita + regional_democracy, data = pdata.frame(final_data_cleaned, index = c("COUNTRY_ALPHA", "year")), model = "within")
fe_model_3y <- plm(v2x_polyarchy ~ lag3_E114 + lag3_E117 + lag3_Y002 + lag3_Y001 + log_lag3_GDP_per_capita + lag3_regional_democracy, data = pdata.frame(final_data_lagged_3y, index = c("COUNTRY_ALPHA", "year")), model = "within")
fe_model_5y <- plm(v2x_polyarchy ~ lag5_E114 + lag5_E117 + lag5_Y002 + lag5_Y001 + log_lag5_GDP_per_capita + lag5_regional_democracy, data = pdata.frame(final_data_lagged_5y, index = c("COUNTRY_ALPHA", "year")), model = "within")

# Regression Outputs
stargazer(model_no_lag, model_3y, model_5y, type = "text", title = "OLS Models")
stargazer(fe_model_no_lag, fe_model_3y, fe_model_5y, type = "text", title = "Fixed Effects Models")

summary(model_5y)
summary(model_3y)
summary(model_no_lag)
summary(fe_model_no_lag)
summary(fe_model_3y)
summary(fe_model_5y)

# Cleanup
remove(gdp_data, merged_data, VDem, WVS, regional_democracy_data)

# Extract regressions to latex format

stargazer(model_no_lag, model_3y, model_5y,
          type = "latex", title = "OLS Models",
          out = "ols_models.tex")  #saves as a text file to the wd

stargazer(fe_model_no_lag, fe_model_3y, fe_model_5y,
          type = "latex", title = "Fixed Effects Models",
          out = "fe_models.tex") # saves as a text file to the wd