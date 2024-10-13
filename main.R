library(tidycensus)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(stringr)
library(reshape2)

# https://walker-data.com/tidycensus/articles/basic-usage.html

key <- readr::read_file("census_api_key.txt")
census_api_key(key)

focus_city <- "North Adams"
county <- "Berkshire"
year <- 2022
census_year <- 2020
comparison_year <- 2018
places <- c("North Adams", "Williamstown", "Clarksburg", "Florida", 
            "Savoy", "Adams", "New Ashford", "Berkshire County")
# geo_ids for all the cities, not including Berkshire County - can get it manually by running the Table 2.1
geo_ids <- c(2500346225, 2500379985, 2500314010, 2500324120, 2500360225, 
             2500300555, 2500344385)
# geo_id for North Adams only
geo_id <- 2500346225
county_geo_id <- 25003
state_geo_id <- 25

#### TO MAKE THIS GENERALIZABLE - Fix currently hard-coded
### Table 2.1 - census years 1960 -2010
### Table 2.2 - census year 2010
### Table 2.3 is from UMass Donahue, just need to get it manually
### Table 2.6 is a different datasource
### Table 2.8 and Table 2.9 is a different datasource, numbers manually retrieved
### Table 2.17 year buckets will be different for different years
### Table 2.19 data manually retrieved from a different source
### Table 2.18 income buckets might be different
### Table 2.21a is based on manually pulled AMI
### Table 2.21b rent and mortgage cost buckets may change, also uses AMI from 2.21a
### Table 3.3 The year buckets will change
### Table 3.4 The year buckets will change
### Table 3.6 Income buckets might change
### Table 3.7 Rent amount buckets might change. I separated median rent from the table itself
### Table 3.10 SHI is from external data 
### ---------------
### Table 5.2 I used ACS 2018 for comparison year, will want to use census year 
### (more accurate), when there's a big enough gap, ie 2020 census compared to 2025 ACS.
### https://api.census.gov/data/2020/dec/dp/variables.html
### Manually pulled https://donahue.umass.edu/business-groups/economic-public-policy-research/massachusetts-population-estimates-program/population-projections
### Table 5.4 Year buckets change
### --------------
### Table 7.1 Created this one externally
### Table 7.2 and Figure 7.1 Created based on Assessor data from 7/22/24
### Table 7.3 Based on Assessor data from 7/22/24 and my QGIS marked neighborhoods
### Table 7.3 and Figure 7.2 - skipped, because I don't have grade data. 
### Table 7.3 (there are two 7.3s in the original) based on 7/22/24 Assessor data
### Table 7.4 skipped, because I don't have grade data from assessor
### Table 8.1 Pulled manually from link

## 2. Demographics
### Table 2.1 Population Change, 1960 - 2024

# get 2020 population data for all the selected cities in the county
population <- get_decennial(geography = "county subdivision", variables = "P1_001N",
                            state = "MA", county = county, year = census_year)

year_census <- c()

for(i in 1:length(geo_ids)){
  year_census <- c(year_census, population$value[population$GEOID == geo_ids[i]])
}

# The population for 
population_county <- get_decennial(geography = "county", variables = "P1_001N", state = "MA",
                            county = county, year = census_year)

year_census <- c(year_census, population_county$value)

# Get 2022
# Using 5-year ACS estimates ending in 2022 
# 1 year estimates are only available for places with over 65,000 people, so they must
# have also used 5 -year data

population_current <- get_acs(geography = "county subdivision", variables = "DP05_0001",
                            state = "MA", year = year)

year_current <- c()

for(i in 1:length(geo_ids)){
  year_current <- c(year_current, population_current$estimate[population_current$GEOID == geo_ids[i]])
}

population_current_county <- get_acs(geography = "county", variables = "DP05_0001",
                         state = "MA", county = county, year = year)

year_current <- c(year_current, population_current_county$estimate)

table_2_1 <- data.frame(location =  places, 
                        y_1960 = c(19905, 7322, 1741, 569, 277, 12391, 165, 142135),
                        y_1970 = c(19195, 8454, 1987, 672, 322, 11772, 183, 149402),
                        y_1980 = c(18063, 8741, 1871, 730, 644, 10381, 159, 145110),
                        y_1990 = c(16964, 8426, 1599, 723, 634, 9445, 192, 139352),
                        y_2000 = c(14691, 8418, 1682, 676, 705, 8809, 247, 134953),
                        y_2010 = c(13708, 7754, 1702, 752, 692, 8485, 228, 131219),
                        y_2020 = year_census,
                        y_2022 = year_current)

write.csv(table_2_1,"data/table_2_1.csv")

### Table 2.2 Population Distribution by Age

# We need 2020 Decennial Census Data 
# 0 - 19. Sum of Under 5, 5 to 9, 10 to 14, 15 to 19. 
# DP1_0002C,DP1_0003C, DP1_0004C, 
# 20 - 34. Sum of 20 to 24, 25 to 29, 30 to 34
# DP1_0006C, DP1_0007C, DP1_0008C 
# 35 - 54. Sum of 35 to 39, 40 to 44, 45 to 49, 50 to 54
# DP1_0009C, DP1_0010C, DP1_0011C, DP1_0012C 
# Over 55. 
# DP1_0013C, DP1_0014C, DP1_0015C, DP1_0016C, DP1_0017C, DP1_0018C, 
# DP1_0019C 

population_up_to_19 <- get_decennial(geography = "county subdivision", 
                                      variables = c("DP1_0002C", "DP1_0003C", "DP1_0004C",
                                                    "DP1_0005C"),
                                      state = "MA",
                                      county = county,
                                      year = census_year,
                                     sumfile = "dp")

zero_to_19 <- sum(filter(population_up_to_19, GEOID==geo_id)$value)

population_20_to_34 <- get_decennial(geography = "county subdivision", 
                                     variables = c("DP1_0006C", "DP1_0007C", "DP1_0008C" ),
                                     state = "MA",
                                     county = county,
                                     year = census_year,
                                     sumfile = "dp")

twenty_to_34 <- sum(filter(population_20_to_34, GEOID==geo_id)$value)

population_35_to_54 <- get_decennial(geography = "county subdivision", 
                                     variables = c("DP1_0009C", "DP1_0010C", "DP1_0011C", "DP1_0012C"),
                                     state = "MA",
                                     county = county,
                                     year = census_year,
                                     sumfile = "dp")

thirty5_to_54 <- sum(filter(population_35_to_54, GEOID==geo_id)$value)

population_55_plus <- get_decennial(geography = "county subdivision", 
                                     variables = c("DP1_0013C", "DP1_0014C", "DP1_0015C", 
                                                   "DP1_0016C", "DP1_0017C", "DP1_0018C", 
                                                   "DP1_0019C"),
                                     state = "MA",
                                     county = county,
                                     year = census_year,
                                     sumfile = "dp")

fifty5_plus <- sum(filter(population_55_plus, GEOID==geo_id)$value)

total_pop <- sum(zero_to_19, twenty_to_34, thirty5_to_54, fifty5_plus)

median_age <- get_decennial(geography = "county subdivision", variables = "P13_001N",
                                                  state = "MA",
                                                  county = county,
                                                  year = census_year,
                                                  sumfile = "dhc")

median_age_na <- filter(median_age, GEOID==geo_id)$value
tot_census_year = c(zero_to_19, twenty_to_34, thirty5_to_54, fifty5_plus, total_pop, median_age_na)
per_census_year = tot_census_year[1:length(tot_census_year) -1]/total_pop * 100

# get ACS data for 2022
# variables - https://api.census.gov/data/2018/acs/acs5/profile/groups/DP05.html

# 0 to 19, 
# DP05_0005E, DP05_0006E, DP05_0007E, DP05_0008E
# 20 to 34 
# DP05_0009E, DP05_0010E
# 35 to 54
# DP05_0011E, DP05_0012E, 
# over 55
# DP05_0013E, DP05_0014E, DP05_0015E, DP05_0016E, DP05_0017E

# zero to 19, 2022
zero_to_19_current <- get_acs(geography = "county subdivision", 
                         variables = c("DP05_0005E", "DP05_0006E", "DP05_0007E", "DP05_0008E"),
                         state = "MA",
                         county = county,
                         year = year)
zero_to_19_current_val <- sum(filter(zero_to_19_current, GEOID==geo_id)$estimate)

# twenty to 34, 2022
twenty_to_34_current <- get_acs(geography = "county subdivision", 
                           variables = c("DP05_0009E", "DP05_0010E"),
                           state = "MA",
                           county = county,
                           year = year)
twenty_to_34_current_val <- sum(filter(twenty_to_34_current, GEOID==geo_id)$estimate)

# 35 to 54, 2022 
thirty5_to_54_current <- get_acs(geography = "county subdivision", 
                             variables = c("DP05_0011E", "DP05_0012E"),
                             state = "MA",
                             county = county,
                             year = year)
thirty5_to_54_current_val <- sum(filter(thirty5_to_54_current, GEOID==geo_id)$estimate)

over_fifty5_current <- get_acs(geography = "county subdivision", 
                            variables = c("DP05_0013E", "DP05_0014E", "DP05_0015E", 
                                          "DP05_0016E", "DP05_0017E"),
                            state = "MA",
                            county = county,
                            year = year)

over_fifty5_current_val <- sum(filter(over_fifty5_current, GEOID==geo_id)$estimate)

total_current <- sum(zero_to_19_current_val, twenty_to_34_current_val, thirty5_to_54_current_val, 
              over_fifty5_current_val)

# Median age, 2022, North Adams

median_age_current <- get_acs(geography = "county subdivision", 
                            variables = c("DP05_0018E"),
                            state = "MA",
                            county = county,
                            year = year)

median_age_current_val <- filter(median_age_current, GEOID==geo_id)$estimate

tot_current <- c(zero_to_19_current_val, twenty_to_34_current_val, thirty5_to_54_current_val, 
              over_fifty5_current_val, tot_current, median_age_current_val)

per_current <- tot_current[1:length(tot_current) -1]/total_current * 100


per_change_census_to_current <- percent_change(tot_census_year, tot_current)

age_cohorts <- c("0-19 years old", "20-34 years old",
                 "35-54 years old", "55+ years old")

table_2_2 <- data.frame(age_cohort = c(age_cohorts, "Total Population", "Median Age"),
                        tot_2010 = c(3359, 2945, 3506, 3898, 13708, 38.9),
                        tot_census_year = tot_census_year, 
                        per_census_year = c(per_census_year, NA), 
                        tot_current_year = tot_current, 
                        per_current_year = c(per_current, NA),
                        per_change_census_to_current = per_change_census_to_current)

write.csv(table_2_2,"data/table_2_2.csv")

### Table 2.3 North Adams Population Projection, 2035

# UMass Donahue 
# https://donahue.umass.edu/business-groups/economic-public-policy-research/massachusetts-population-estimates-program/population-projections
# https://donahue.umass.edu/documents/UMDI_V2024_Long-Term_Population_Projections_MCD%2C_County%2C_RPA_Age_Sex_detail_2010-2050_a.xlsx
zero_to_19_2035 = 420 + 398 + 480 + 753
twenty_to_34_2035 = 1053 + 631 + 515
thirty5_to_54_2035 = 588 + 660 + 676 + 723
fifty5_plus_2035 = 692 + 747 + 768 + 865 + 786 + 528 + 345
sum_2035 = zero_to_19_2035 + twenty_to_34_2035 + thirty5_to_54_2035 + fifty5_plus_2035
proj_2035 = c(zero_to_19_2035, twenty_to_34_2035, thirty5_to_54_2035, fifty5_plus_2035, sum_2035)

per_change_2035 = (proj_2035 - tot_2022[1:5])/tot_2022[1:5] * 100

table_2_3 <- data.frame(age_cohort = c(age_cohorts, "Total Population"), 
                        tot_2022[1:5], 
                        proj_2035 = proj_2035,
                        per_change_2035 = per_change_2035 )

write.csv(table_2_3, "data/table_2_3.csv")

### Table 2.4 Race
# white alone - DP05_0037E
# Black alone - DP05_0038E
# Hispanic/Latino - DP05_0071E
# American Indian or Alaska Native alone - DP05_0039E
# Asian alone - DP05_0044E
# Native Hawaiian or Other Pacific Islander Alone - DP05_0052E
# Some other race alone - DP05_0057E
# Two or more races - DP05_0058E

race_vars <- c("DP05_0037", "DP05_0038", "DP05_0071", 
               "DP05_0039", "DP05_0044", "DP05_0052", 
               "DP05_0057", "DP05_0058")
county_race <- get_acs(geography = "county subdivision", 
                            variables = race_vars,
                            state = "MA",
                            county = county,
                            year = year)

city_race <- filter(county_race, GEOID==geo_id)

race_current_year <- c()

for(i in 1:length(race_vars)){
  race_current_year <- c(race_current_year, filter(city_race, variable == race_vars[i])$estimate) 
}

total <- sum(race_current_year)
race_current_year <- c(race_current_year, total)

table_2_4 <- data.frame(race = c("White alone", "Black or African American alone", 
                                 "Hispanic or Latino", "American Indian or Alaskan Native alone",
                                 "Asian alone", "Native Hawaiian or Other Pacific Islander alone",
                                 "Some other race alone", "Two or more races", "Total"),
                        race_current_year = race_current_year,
                        per_total = race_current_year/total * 100)

write.csv(table_2_4, "data/table_2_4.csv")

### 2.5 

# for variables https://api.census.gov/data/2018/acs/acs5/subject/groups/S1701.html

# total pop for whom poverty is determined
# Under 18 - S1701_C01_002
# 18 to 65 - S1701_C01_006
# over 65 - S1701_C01_010
# total - S1701_C01_001

tot_pov_vars <- c("S1701_C01_002", "S1701_C01_006", "S1701_C01_010", "S1701_C01_001")
tot_pov_vec <- get_acs_vec(vars_vec = tot_pov_vars)
tot_pov_vec <- c(tot_pov_vec[1] + tot_pov_vec[2], tot_pov_vec[3], tot_pov_vec[4])

# total below poverty 
# Under 18 S1701_C02_002
# 18 to 64 S1701_C02_006
# Over 65 S1701_C02_010
# total S1701_C02_001

pop_below_pov_vars <- c("S1701_C02_002", "S1701_C02_006", "S1701_C02_010", "S1701_C02_001")
pop_below_pov_vec <- get_acs_vec(vars_vec = pop_below_pov_vars)
pop_below_pov_vec <- c(pop_below_pov_vec[1] + pop_below_pov_vec[2], pop_below_pov_vec[3], pop_below_pov_vec[4])

# state_tot_pov 
state_tot_pov_vec <- get_acs_vec(geoid= 25, vars_vec = tot_pov_vars, geography = "state")
state_tot_pov_vec <- c(state_tot_pov_vec[1] + state_tot_pov_vec[2], state_tot_pov_vec[3], state_tot_pov_vec[4])

# state_below_pov
state_below_pov_vec <- get_acs_vec(geoid= 25, vars_vec = pop_below_pov_vars, geography = "state")
state_below_pov_vec <- c(state_below_pov_vec[1] + state_below_pov_vec[2], state_below_pov_vec[3], state_below_pov_vec[4])

table_2_5 <- data.frame(age_cohort = c("under 65", "over 65", "total"), 
                        tot = tot_pov_vec,
                        below_poverty = pop_below_pov_vec,
                        per_below = pop_below_pov_vec/tot_pov_vec * 100, 
                        state_below_pov = state_below_pov_vec/state_tot_pov_vec * 100)

write.csv(table_2_5, "data/table_2_5.csv")

### Table 2.6 Unemployment Rates and Labor Force 
# https://lmi.dua.eol.mass.gov/LMI/LaborForceAndUnemployment#
unemployment_data <- read.csv("data/LURReport.csv")
county_data <- read.csv("data/county_data.csv")
county_data <- cbind(Area = paste(county, "County"), county_data)
# state not seasonally adj
# https://lmi.dua.eol.mass.gov/LMI/LaborForceAndUnemployment/LURResults?A=01&GA=000025&TF=1&Y=&Sopt=N&Dopt=TEXT
state_data <- data.frame(Area = "Massachusetts", Labor.Force = 3800827, 
                         Employed = 3657969, Unemployed = 142858, Area.Rate = 3.8, 
                         Massachusetts.Rate = 3.8)

table_2_6 <- rbind(unemployment_data, county_data, state_data)
write.csv(table_2_6, "data/table_2_6.csv")


### Table 2.7 Households by Income Level in North Adams
income_vars <- c("DP03_0052", "DP03_0053", "DP03_0054", "DP03_0055", "DP03_0056",
                 "DP03_0057", "DP03_0058", "DP03_0059", "DP03_0060", 
                 "DP03_0061", "DP03_0051")

income_hh_comp <- get_acs_vec(vars_vec = income_vars, yr = comparison_year)
income_hh_current <- get_acs_vec(vars_vec = income_vars)

income_levels <- c("Less than $10,000", "$10,000 to $14,999", "$15,000 to $24,999",
                   "$25,000 to $34,999", "$35,000 to $49,999", "$50,000 to $74,999",
                   "$75,000 to $99,999", "$100,000 to $149,999", "$150,000 to $199,999",
                   "$200,000 or More", "Total Households")

table_2_7 <- data.frame(Income.Level = income_levels, 
                        Number.of.Households.Compare = income_hh_comp, 
                        Number.of.Households.Current = income_hh_current,
                        Percent.Total.Curr = income_hh_current/last(income_hh_current) * 100,
                        Percent.Change = percent_change(income_hh_comp, income_hh_current))

write.csv(table_2_7, "data/table_2_7.csv")

### Figure 2.1 Households by Income in North Adams
fig_2_1 <- data.frame(Income.Level = income_levels, 
                      hh_comp = income_hh_comp, 
                      hh_current = income_hh_current)
fig_2_1 <- fig_2_1[1:10, ]
  
fig_2_1_long <- gather(fig_2_1, year, total, hh_comp:hh_current)
plot <- ggplot(fig_2_1_long, aes(factor(Income.Level, level = income_levels[1:10]) , total, fill=year))
plot <- plot + geom_bar(stat = "identity", position = 'dodge') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
plot

### Table 2.8 Average Weekly Wage by Job Type
# Manually retrieved from here - https://lmi.dua.eol.mass.gov/LMI/EmploymentAndWages#

job_types <- c("Service-providing Jobs", "Goods-producing Jobs", "Total")
avg_monthly = c(5114, 304, 5418)

table_2_8 <- data.frame(Job.Type = job_types,
                        Avg.Weekly.Wage = c(1006, 1075, 1010), 
                        Avg.Monthly.Employment = avg_monthly,
                        Per.Avg.Total.Monthly.Employment = avg_monthly/avg_monthly[3] * 100)

write.csv(table_2_8, "data/table_2_8.csv")

### Table 2.9 Average Weekly Wages Compared to County and State, 2022
NA_avg_weekly = c(960, 984)
Berk_avg_weekly = c(1082, 1254)
MA_avg_Weekly = c(1713, 1826)

table_2_9 <- data.frame(Job.Type = job_types[1:2], 
                        North.Adams.Avg.Weekly = NA_avg_weekly,
                        Berk.Avg.Weekly = Berk_avg_weekly,
                        MA.Avg.Weekly = MA_avg_Weekly,
                        Per.of.MA.Avg.Weekly = NA_avg_weekly/MA_avg_Weekly * 100)

write.csv(table_2_9, "data/table_2_9.csv")

### Table 2.10 Industry Distribution, 

industry <- c("Civilian Employed, over 16", "Agriculture, Forestry, Fishing & Hunting, And Mining",
              "Construction", "Manufacturing", "Wholesale Trade", "Retail Trade",
              "Transportation & Warehousing & Utilities", "Information", 
              "Financing & Insurance, Real Estate, Rental & Leasing", 
              "Professional, Scientific, Management,Administrative & Waste Management Services",
              "Educational Services, Health Care, and Social Assistance", 
              "Arts, Entertainment, Recreation, and Accommodation & Food Service", 
              "Other Services, except Public Administration", "Public Administration")
industry_vars <- c("DP03_0004", "DP03_0033", "DP03_0034", "DP03_0035", 
                   "DP03_0036", "DP03_0037", "DP03_0038", "DP03_0039", 
                   "DP03_0040", "DP03_0041", "DP03_0042", "DP03_0043", 
                   "DP03_0044", "DP03_0045")

industry_vec_current <- get_acs_vec(vars_vec = industry_vars)
industry_vec_comp <- get_acs_vec(vars_vec = industry_vars, yr = comparison_year)

table_2_10 <- data.frame(Industry = industry, 
                         acs_comp = industry_vec_comp, 
                         acs_current = industry_vec_current, 
                         per_of_jobs_22 = industry_vec_current/industry_vec_current[1] * 100, 
                         per_change = percent_change(industry_vec_comp, industry_vec_current))

write.csv(table_2_10, "data/table_2_10.csv")

### Table 2.11 Age of Housing Units in North Adams
# https://api.census.gov/data/2022/acs/acs5/profile/groups/DP04.html
year_built <- c("Built 2020 or later", "Built 2010 to 2019", "Built 2000 to 2009",
                "Built 1990 to 1999", "Built 1980 to 1989", "Built 1970 to 1979",
                "Built 1960 to 1969", "Built 1950 to 1959", "Built 1940 to 1949",
                "Built 1939 or earlier","Total Housing Units")
year_built_vars <- c("DP04_0017", "DP04_0018", "DP04_0019", "DP04_0020", 
                     "DP04_0021", "DP04_0022", "DP04_0023", "DP04_0024", 
                     "DP04_0025", "DP04_0026", "DP04_0016")

year_built_vec <- get_acs_vec(vars_vec = year_built_vars)

table_2_11 <- data.frame(Year.Built = year_built, Num.Units=year_built_vec,
                         Per.Total.Units = year_built_vec/year_built_vec[11]*100)

write.csv(table_2_11, "data/table_2_11.csv")

### Table 2.12 Homeowner- Versus Renter-Occupied Housing

# DP04_0002
type_housing <- c("Owern-occupied", "Renter-occupied")
h_v_r_vars <- c("DP04_0046", "DP04_0047")
h_v_r_vec <- get_acs_vec(vars_vec = h_v_r_vars)

household_size_vars <- c("DP04_0048", "DP04_0049")
household_size_vec <- get_acs_vec(vars_vec = household_size_vars)

med_hh_income_vars <- c("S2503_C03_013", "S2503_C05_013")
med_hh_income_vec <- get_acs_vec(vars_vec = med_hh_income_vars)

# I tested against 2018 acs to ensure that I'm using the same metrics
cost_burdened_owner_vars <- c("S2503_C03_028", "S2503_C03_032",
                                 "S2503_C03_036", "S2503_C03_040", 
                                 "S2503_C03_044")
cost_burdened_owner_vec <- get_acs_vec(vars_vec = cost_burdened_owner_vars)

cost_burdened_renter_vars <- c("S2503_C05_028", "S2503_C05_032", 
                               "S2503_C05_036", "S2503_C05_040", 
                               "S2503_C05_044")
cost_burdened_renter_vec <- get_acs_vec(vars_vec = cost_burdened_renter_vars)

cost_burdened <- c(sum(cost_burdened_owner_vec)/h_v_r_vec[1] *100, 
                   sum(cost_burdened_renter_vec)/h_v_r_vec[2] *100)

table_2_12 <- data.frame(Type.of.Housing = type_housing, 
                         Num.Occupied.Units = h_v_r_vec,
                         Per.Occupied.Units = h_v_r_vec/sum(h_v_r_vec) * 100,
                         Avg.Household.Size = household_size_vec,
                         Median.Household.Income = med_hh_income_vec,
                         Per.Housing.Cost.Burdened = cost_burdened)

write.csv(table_2_12, "data/table_2_12.csv")

### Table 2.13 Vacancy Status
# https://api.census.gov/data/2022/acs/acs5/groups/B25004.html
vac_status <- c("For rent", "Rented, not occupied", "For sale only", 
                "Sold, not occupied", "For seasonal,recreational, or occasional use", 
                "For migrant workers", "Other vacant", "Total")
vac_status_vars <- c("B25004_002", "B25004_003", "B25004_004", "B25004_005",
                     "B25004_006", "B25004_007", "B25004_008", "B25004_001")

vac_status_vec <- get_acs_vec(vars_vec = vac_status_vars)

table_2_13 <- data.frame(Vacancy.Status = vac_status, 
                         Num.Housing.Units = vac_status_vec, 
                         Per.Housing.Units = vac_status_vec/vac_status_vec[8]*100)

write.csv(table_2_13, "data/table_2_13.csv")

### Figure 2.2 Residential Use - to do later
parcels <- st_read("~/North Adams Properties/data/joined_data.shp")
use_codes <-c(101, 102, 103, 104, 105, 109, 111, 112, 013)
parcels <- parcels %>% filter(USE_CODE %in% use_codes)

plot <- ggplot(parcels, aes(x=USE_CODE)) + geom_histogram(stat="count") +
  scale_x_discrete(labels=c("Single-family", "Condominium", 
  "Mobile home", "Two-family", "Three-family", "Multiple Houses", "Four- to Eight Units", 
  "More than eight units", "Multi-use res/com")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_text(
    stat = "count",
    aes(
      label = after_stat(count)
    ),
    position = position_dodge(),
    color = "black",
    size = 4,
    vjust = -0.2
  )

### Table 2.14 
# 2018 combined 10 - 19 and 20 +, I left them separate
struc_type <- c("1 unit, detached", "1 unit, attached", "2 units", 
                "3 to 4 units", "5 to 9 units", "10 - 19 units", 
                "20+ units", "Mobile Home", 
                "Boat, RV, van, etc", "Total units")
struc_vars <- c("DP04_0007", "DP04_0008", "DP04_0009", "DP04_0010", 
                "DP04_0011", "DP04_0012", "DP04_0013", "DP04_0014", 
                "DP04_0015", "DP04_0006")
struc_vec_comp <- get_acs_vec(vars_vec = struc_vars, year = comparison_year)
struc_vec_current <- get_acs_vec(vars_vec = struc_vars)

table_2_14 <- data.frame(Structure.Type = struc_type, 
                         acs.comp = struc_vec_comp,
                         acs.current = struc_vec_current, 
                         Percent.Housing.Units = struc_vec_current/struc_vec_current[10]*100)

write.csv(table_2_14, "data/table_2_14.csv")

### Table 2.15 Housing Stock by Number of Bedrooms
num_bedrooms <- c("Studio, 1 bedroom", "2 bedrooms", "3 bedrooms", 
                  "4 bedrooms", "5+ bedrooms", "Total units")

num_bedrooms_vars <- c("DP04_0039", "DP04_0040", "DP04_0041", 
                       "DP04_0042", "DP04_0043", "DP04_0044", 
                       "DP04_0038")

num_bedrooms_vec_comp <- get_acs_vec(vars_vec = num_bedrooms_vars, year = comparison_year)
# Need to combine DP04_0039E (no bedrooms), DP04_0040 (1 bedroom)
num_bedrooms_vec_comp <- c(num_bedrooms_vec_comp[1] + num_bedrooms_vec_comp[2], 
                         num_bedrooms_vec_comp[3:length(num_bedrooms_vec_comp)])

num_bedrooms_vec_current <- get_acs_vec(vars_vec = num_bedrooms_vars)
# Need to combine DP04_0039E (no bedrooms), DP04_0040 (1 bedroom)
num_bedrooms_vec_current <- c(num_bedrooms_vec_current[1] + num_bedrooms_vec_current[2], 
                         num_bedrooms_vec_current[3:length(num_bedrooms_vec_current)])

table_2_15 <- data.frame(Num.Bedrooms = num_bedrooms, 
                         acs_comp = num_bedrooms_vec_comp,
                         acs_current = num_bedrooms_vec_current,
                         Percent.Total.Housing.Current = num_bedrooms_vec_current/num_bedrooms_vec_current[6] * 100,
                         Percent.Change = percent_change(num_bedrooms_vec_comp, num_bedrooms_vec_current))

write.csv(table_2_15, "data/table_2_15.csv")

### Table 2.16 Age of Householder
age_of_hh <- c("Under 35 years old", "35-44 years old", "45-54 years old",
               "55-64 years old", "65-74 years old", "75-84 years old", 
               "85 years old and older", "Total")
age_of_hh_vars <- c("S2502_C01_011", "S2502_C01_012", "S2502_C01_013",
                    "S2502_C01_014", "S2502_C01_015", "S2502_C01_016", 
                    "S2502_C01_017", "S2502_C01_001")
age_of_hh_vec <- get_acs_vec(vars_vec = age_of_hh_vars)
per_age_hh_vec <- age_of_hh_vec/age_of_hh_vec[length(age_of_hh_vec)] * 100

age_of_hh_renter_vars <- c("S2502_C05_011", "S2502_C05_012", 
                           "S2502_C05_013", "S2502_C05_014",
                           "S2502_C05_015", "S2502_C05_016", 
                           "S2502_C05_017", "S2502_C05_001")
age_of_hh_renters_vec <- get_acs_vec(vars_vec = age_of_hh_renter_vars)
per_of_renters_vec <- age_of_hh_renters_vec/age_of_hh_renters_vec[length(age_of_hh_renters_vec)] * 100

age_of_hh_owners_vars <- c("S2502_C03_011", "S2502_C03_012",
                           "S2502_C03_013", "S2502_C03_014", 
                           "S2502_C03_015", "S2502_C03_016",
                           "S2502_C03_017", "S2502_C03_001")
age_of_hh_owners_vec <- get_acs_vec(vars_vec = age_of_hh_owners_vars)
per_of_owners_vec <- age_of_hh_owners_vec/age_of_hh_owners_vec[length(age_of_hh_owners_vec)] * 100

table_2_16 <- data.frame(Age.of.Householder = age_of_hh, 
                         Percent.of.Total.Households = per_age_hh_vec, 
                         Number.of.Renters = age_of_hh_renters_vec, 
                         Percent.of.Renters = per_of_renters_vec, 
                         Number.of.Owners = age_of_hh_owners_vec, 
                         Percent.of.Owners = per_of_owners_vec)

write.csv(table_2_16, "data/table_2_16.csv")

### Table 2.17 Household Tenure
year_hh_moved_in <- c("2021 or later", "2018 to 2020", "2010 to 2017", 
                      "2000 to 2009", "1990 to 1999", "1989 or before")

# total
year_hh_moved_in_per_vars <- c("S2502_C02_022", "S2502_C02_023", 
                           "S2502_C02_024", "S2502_C02_025", 
                           "S2502_C02_026", "S2502_C02_027")
year_hh_moved_in_per_vec <- get_acs_vec(vars_vec = year_hh_moved_in_per_vars)

# renter num
year_hh_moved_in_renter_vars <- c("S2502_C05_022", "S2502_C05_023", 
                               "S2502_C05_024", "S2502_C05_025", 
                               "S2502_C05_026", "S2502_C05_027")
year_hh_moved_in_renter_vec <- get_acs_vec(vars_vec = year_hh_moved_in_renter_vars)

# renter percent
year_hh_moved_in_renter_per_vars <- c("S2502_C06_022", "S2502_C06_023", 
                                  "S2502_C06_024", "S2502_C06_025", 
                                  "S2502_C06_026", "S2502_C06_027")
year_hh_moved_in_renter_per_vec <- get_acs_vec(vars_vec = year_hh_moved_in_renter_per_vars)

# owner num
year_hh_moved_in_owner_vars <- c("S2502_C03_022", "S2502_C03_023", 
                                  "S2502_C03_024", "S2502_C03_025", 
                                  "S2502_C03_026", "S2502_C03_027")
year_hh_moved_in_owner_vec <- get_acs_vec(vars_vec = year_hh_moved_in_owner_vars)

# owner per
year_hh_moved_in_owner_per_vars <- c("S2502_C04_022", "S2502_C04_023", 
                                 "S2502_C04_024", "S2502_C04_025", 
                                 "S2502_C04_026", "S2502_C04_027")
year_hh_moved_in_owner_per_vec <- get_acs_vec(vars_vec = year_hh_moved_in_owner_per_vars)

table_2_17 <- data.frame(Year.Householder.Moved.In = year_hh_moved_in,
                         Number.of.Renter.Households = year_hh_moved_in_renter_vec,
                         Percent.Renter.Households = year_hh_moved_in_renter_per_vec,
                         Number.of.Owner.Households = year_hh_moved_in_owner_vec,
                         Percent.Owner.Households = year_hh_moved_in_owner_per_vec)
write.csv(table_2_17, "data/table_2_17.csv")

### Table 2.19 FY 2022 Income Limits, Berkshire County
# https://www.huduser.gov/portal/datasets/il/il2022/2022summary.odn

income_category <- c("extremely low income, 30%", "very low income, 50%", "low income, 80%")

median_family_income <- 92100
eli_30 <- c(19800,	22600,	25450,	28250,	32470,	37190,	41910,	46630)
vli_50 <- c(32950,	37650,	42350,	47050,	50850,	54600,	58350,	62150)
li_80 <- c(52750,	60250,	67800,	75300,	81350,	87350,	93400,	99400)

table_2_19 <- rbind(c("Income Category", 1:8),
  c(income_category[1], eli_30),  
  c(income_category[2], vli_50), 
  c(income_category[3], li_80))

write.csv(table_2_19, "data/table_2_19.csv")


### Table 2.18 Percentage of Households by Area Median Income
# It's easier to do this table after 2.19
# I'm copying the methodology from the North Adams Housing Needs Assessment, 2020
# The income buckets don't line up to the HUD ELI/VLI/LI amounts. 
# However, they assume that households are evenly distributed within those buckets,
# if, for example, the ELI is 53% of the way into a income bracket ($20,300 is 
# roughly 53% of the way between 15k and 25k), then 53% of the households in that 
# bracket are included in the calculation. 

t218_locations <-  c(paste(county, "County"), places[1:7])
t218_geoids <- c(county_geo_id, geo_ids)

eli <- table_2_19[2,3] %>% strtoi
vli <- table_2_19[3, 3] %>% strtoi
li <- table_2_19[4, 3] %>% strtoi

bucket_vars = c("DP03_0052", "DP03_0053", "DP03_0054",
                "DP03_0055", "DP03_0056", "DP03_0057", 
                "DP03_0058", "DP03_0059", "DP03_0060",
                "DP03_0061")
upper_limits <-  c(9999, 14999, 24999, 34999, 
                   49999, 74999, 100000, 150000, 
                   199999, Inf)

income_buckets <- data.frame( 
  var.names = bucket_vars,
  upper.limts = upper_limits)

table_2_18 <- data.frame()


get_hh_nums <- function(eli, vli, li, res){
  income_lim <- c(eli, vli, li)
  hhs <- data.frame(c(0, 0, 0))
  
  j <- 1
  
  for(i in 1:length(bucket_vars)){
    if(income_lim[j] > income_buckets$upper.limts[i]){
      hhs[j,1] = hhs[j,1] + res[i]
    } else {
      distance <- income_buckets$upper.limts[i] - income_buckets$upper.limts[i -1] -1
      per <- (income_lim[j] - income_buckets$upper.limts[i -1])/distance
      hhs[j,1] = hhs[j,1] + (per * res[i])
      if(j == length(income_lim)){
        break
      } else {
        j <- j + 1
        hhs[j, 1] = (1 - per) * res[i]
      }
    }
  }
  
  return(hhs[,1])
}


get_2_18_row <- function(i, eli, vli, li, yr = year){
  loc <- t218_locations[i]
  geo <- t218_geoids[i]
  
  if( grepl("county", loc, ignore.case=TRUE)){
    geography = "county"
  } else {
    geography = "county subdivision"
  }
  
  hh_acs <- get_acs_vec(vars_vec = bucket_vars, geoid = geo, geography = geography, yr = year)
  
  tot_hh <- sum(hh_acs)
  hhs_by_income <- get_hh_nums(eli, vli, li, hh_acs)
  
  return(c(loc, tot_hh, hhs_by_income[1], hhs_by_income[1]/tot_hh * 100,
           hhs_by_income[2], hhs_by_income[2]/tot_hh * 100,
           hhs_by_income[3], hhs_by_income[3]/tot_hh * 100 ))
  
}

# This is checking against 2018.
# I'm confused, because my calculations for all the other cities match up, but not for 
# Berkshire County as a whole... 
# for(i in 1:length(t218_locations)){
#   row <- get_2_18_row(i, 20500, 34200, 54650, year = 2018)
#   table_2_18 <- rbind(table_2_18, row)
# }


for(i in 1:length(t218_locations)){
  table_2_18 <- rbind(table_2_18, get_2_18_row(i, eli, vli, li))
}

write.csv(table_2_18, "data/table_2_18.csv")

### Table 2.20 Prevalence of Housing Cost Burden
t2_20_loc <- places
t2_20_geoids <- c(geo_ids, county_geo_id)

# S2503_C01_024E - median housing costs, just curious about this. 
# get_acs_vec(vars_vec= c("S2503_C01_024"))
# $911

# number of renter households - 

# number of owner households - 
# Making less than 20k S2503_C03_028
# 20 to 35k S2503_C03_032
# 35 to 50k S2503_C03_036
# 50 to 75k S2503_C03_040
# 75 or more S2503_C03_044

owner_vars <- c("S2503_C03_028", "S2503_C03_032",
                "S2503_C03_036", "S2503_C03_040", 
                "S2503_C03_044")
renter_vars <- c("S2503_C05_028", "S2503_C05_032",
                 "S2503_C05_036", "S2503_C05_040", 
                 "S2503_C05_044")

num_owners <- "DP04_0046"
num_renters <-"DP04_0047"

table_2_20 <- data.frame()

get_t2_20_row <- function(i, yr = year){
  if(grepl("county", t2_20_loc[i], ignore.case = TRUE)){
    geography <- "county"
  } else {
    geography <- "county subdivision"
  }
  
  hh_nums <-  get_acs_vec(geoid = t2_20_geoids[i], 
                          yr = year,
                          geography = geography,
                          vars_vec = c(num_renters, num_owners))
  
  renter_cost_burdened <- get_acs_vec(geoid = t2_20_geoids[i], 
                                     geography = geography,
                                     yr = year,
                                     vars_vec = renter_vars) %>% sum
    
  owner_cost_burdened <- get_acs_vec(geoid = t2_20_geoids[i], 
                           geography = geography,
                           yr = year,
                           vars_vec = owner_vars) %>% sum
  
  row <- c(t2_20_loc[i], hh_nums[1], renter_cost_burdened, 
           renter_cost_burdened/hh_nums[1] * 100, 
           hh_nums[2], owner_cost_burdened, 
           owner_cost_burdened/hh_nums[2] *100)
  return(row)
}

# I checked against 2018, and it matches up to the old Housing Needs Assessment,
# except for Berkshire county - everything is slightly off

for(i in 1:length(t2_20_geoids)){
  table_2_20 <- rbind(table_2_20, get_t2_20_row(i, year))
}

names(table_2_20) <- c("Location", "Number of Rental Households", 
                       "Renters Burdened", "% Renters Burdened", "Number of Owner Households", "Owners Burdened", "% Owners Burdened")

write.csv(table_2_20, "data/table_2_20.csv")

### Figure 2.4 Prevalence of Housing Cost Burden
fig_2_4 <- table_2_20 %>% select(Location, '% Renters Burdened', '% Owners Burdened')

fig_2_4_long <- gather(fig_2_4, type, percent, '% Renters Burdened':'% Owners Burdened')

plot <- ggplot(fig_2_4_long, aes(factor(Location, level = t2_20_loc), as.numeric(percent), fill=type))
plot <- plot + geom_bar(stat = "identity", position = 'dodge') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
plot

### Table 2.21a Definition of Income Categories
# North Adams Median Family income - HUD just gives the same number for each city in Berkshire county, 
# excluding Pittsfield, and a few others
# 2022 is 92,100
# 2024 is 105,700

na_ami_24 <- 105700
middle_income <- paste0(1 + na_ami_24, " - ", na_ami_24 * 1.2)
moderate_income <- paste0(1 + na_ami_24 * .8, " - ", na_ami_24)
low_income <- paste0(1+ na_ami_24 * .5, " - ", na_ami_24 * .8)
very_low_income <- paste0(na_ami_24 * .3, " - ", na_ami_24 * .5)
extremely_low_income <- paste0("Below ", na_ami_24* .3)

income_categories <- c("Middle Income", "Moderate Income",
                      "Low Income", "Very Low Income", 
                      "Extremely Low Income")

table_2_21a <- data.frame(Income.Category = income_categories,
                          Percent.AMI = c("120%", "100%", "80%", "50%", 
                                          "Under 30%"), 
                          Income.Range = c(middle_income, moderate_income, 
                                           low_income, very_low_income, 
                                           extremely_low_income))

# 2019 AMI for North Adams is 78900, but that doesn't line up with the numbers in 2.21a
# 80,899 looks like the number being used. 

write.csv(table_2_21a, "data/table_2_21a.csv")

### Table 2.21b Affordable Housing Supply Analysis for North Adams
# Total households is very similar to 2.18 calculations, except that they use median 
# household income for a two-person household, whereas this is just AMI

top_limit_per <- c(1.2, 1, .8, .5, .3)
affordable_monthly_housing <- c()

for(i in top_limit_per){
  affordable_monthly_housing <- c(affordable_monthly_housing, (i * na_ami_24 * .3)/12)
}

total_households_res <- get_acs_vec(vars_vec = bucket_vars)

# Just (barely) modified function for 2.18, using income_buckets from there
get_total_households <- function(res){
  income_lim <- rev(top_limit_per) * na_ami_24
  hhs <- data.frame(c(0, 0, 0, 0, 0))
  
  j <- 1
  
  for(i in 1:length(bucket_vars)){
    if(income_lim[j] > income_buckets$upper.limts[i]){
      hhs[j,1] = hhs[j,1] + res[i]
    } else {
      distance <- income_buckets$upper.limts[i] - income_buckets$upper.limts[i -1] -1
      per <- (income_lim[j] - income_buckets$upper.limts[i -1])/distance
      hhs[j,1] = hhs[j,1] + (per * res[i])
      if(j == length(income_lim)){
        break
      } else {
        j <- j + 1
        hhs[j, 1] = (1 - per) * res[i]
      }
    }
  }
  
  return(hhs[,1])
}

total_hh <- get_total_households(total_households_res)

# B25063_027 - no cash rent - exclude! In the old housing needs assessment, 
# they exclude no cash rent, and I'm not sure why/how, and why this wouldn't
# fall under Less than 100
# B25063_003 - Less than 100
# B25063_004 - 100 to 149
# B25063_005 - 150 to 199
# B25063_006 - 200 to 249
# B25063_007 - 250 to 299
# B25063_008  - 300 to 349
# B25063_009 - 350 to 399
# B25063_010 - 400 to 449
# B25063_011 - 450 to 499
# B25063_012 - 500 to 549
# B25063_013 - 550 to 599
# B25063_014 - 600 to 649
# B25063_015 - 650 to 699
# B25063_016 - 700 to 749
# B25063_017 - 750 to 799
# B25063_018 - 800 to 899
# B25063_019 - 900 to 999
# B25063_020 - 1,000 to 1,249
# B25063_021 - 1,250 to 1,499
# B25063_022 - 1,500 to 1,999
# B25063_023 - 2,000 to 2,499
# B25063_024 - 2,500 to 2,999
# B25063_025 - 3,000 to $3,499
# B25063_026 - 3,500 or more 

rent_vars <- c("B25063_003", "B25063_004", "B25063_005",
               "B25063_006", "B25063_007", "B25063_008", "B25063_009",
               "B25063_010", "B25063_011", "B25063_012", "B25063_013",
               "B25063_014", "B25063_015", "B25063_016", "B25063_017",
               "B25063_018", "B25063_019", "B25063_020", "B25063_021",
               "B25063_022", "B25063_023", "B25063_024", "B25063_025",
               "B25063_026")

rent_upper <- c(100, 149, 199, 249, 299, 349, 399, 449, 499, 549, 599, 649, 699, 
                749, 799, 899, 999, 1249, 1499, 1999, 2499, 2999, 3499, Inf)

# res_18 <- get_acs_vec(vars_vec = rent_vars, year = 2018)
# tes_18 <- get_affordable_units(rent_buckets, res_18)
# it's very slightly off for 2018.
# 1177 in VLI, instead of 980; and 13 in middle, instead of 11. 

rent_buckets <- data.frame(vars = rent_vars, upper.limits = rent_upper)

get_affordable_units <- function(buckets, res){
  cost_limits <- rev(affordable_monthly_housing)
  # cost limits for 2018 below
  # cost_limits <- c(24270, 40449, 64709, 80899, 97079) * .3 * (1/12)
  units <- data.frame(c(0, 0, 0, 0, 0))
  
  j <- 1
  
  for(i in 1:length(buckets$vars)){
    if(cost_limits[j] > buckets$upper.limits[i]){
      units[j,1] = units[j,1] + res[i]
    } else {
      distance <- buckets$upper.limits[i] - buckets$upper.limits[i -1] -1
      per <- (cost_limits[j] - buckets$upper.limits[i -1])/distance
      units[j,1] = units[j,1] + (per * res[i])
      if(j == length(cost_limits)){
        break
      } else {
        j <- j + 1
        units[j, 1] = (1 - per) * res[i]
      }
    }
  }
  
  return(units[,1])
}

rent_bucket_res <- get_acs_vec(vars_vec = rent_vars, yr = year)
affordable_rent_res <- get_affordable_units(rent_buckets, rent_bucket_res)

# Mortgage amts
# B25087_003 - Less than 200
# B25087_004 - 200 to 299
# B25087_005 - 300 to 399
# B25087_006 - 400 to 499
# B25087_007 - 500 to 599
# B25087_008 - 600 to 699
# B25087_009 - 700 to 799
# B25087_010 - 800 to 899
# B25087_011 - 900 to 999
# B25087_012 - 1000 to 1249
# B25087_013 - 1250 to 1499
# B25087_014 - 1500 to 1999
# B25087_015  - 2000 to 2499
# B25087_016 - 2500 to 2999
# B25087_017- 3000 to 3499
# B25087_018 - 3500 to 3999
# B25087_019 - 4000 or more
owner_vars <- c("B25087_003", "B25087_004", "B25087_005", "B25087_006", "B25087_007",
                "B25087_008", "B25087_009", "B25087_010", "B25087_011", "B25087_012",
                "B25087_013", "B25087_014", "B25087_015", "B25087_016", "B25087_017",
                "B25087_018", "B25087_019")
mort_upper <- c(200, 299, 399, 499, 599, 699, 799, 899, 999, 1249, 1499, 1999, 
                2499, 2999, 3499, 3999, Inf)
mort_buckets <- data.frame(vars = owner_vars, upper.limits = mort_upper)

owner_bucket_res <- get_acs_vec(vars_vec = owner_vars, yr = year)
affordable_mort_res <- get_affordable_units(mort_buckets, owner_bucket_res)

tot_units <- rev(affordable_rent_res) + rev(affordable_mort_res)

# Need to add a totals row 
table_2_21b <- data.frame(Income.Category = income_categories,
                          Affordable.Monthly.Housing.Costs = affordable_monthly_housing,
                          Affordable.Rental.Units.Available = rev(affordable_rent_res),
                          Affordable.Homeownership.Units.Available = rev(affordable_mort_res), 
                          Total.Affordable.Units.Available = tot_units, 
                          Total.Number.of.Households = rev(total_hh),
                          Estimated.Affordable.Housing = tot_units - rev(total_hh))

write.csv(table_2_21b, "data/table_2_21b.csv")

### Table 3.1 Overview of Rental Units

housing_type <- c("Owner-Occupied", "Renter-Occupied", "Total Occupied Units")
t_3_1_vars <- c("S2504_C03_001", "S2504_C05_001", "S2504_C01_001")

num_comp <- get_acs_vec(vars_vec = t_3_1_vars, yr = comparison_year)
num_current <- get_acs_vec(vars_vec = t_3_1_vars)

table_3_1 <- data.frame(Housing.Type = housing_type,
                        Units.Comp = num_comp, 
                        Percent.Comp = num_comp/(num_comp[3]) * 100,
                        Units.Current = num_current, 
                        Percent.Current = num_current/(num_current[3]) * 100,
                        Per.Change = percent_change(num_comp, num_current))

write.csv(table_3_1, "data/table_3_1.csv")

### Table 3.2 Number of Rental Units in Each Buildings
# S2504_C01_002 - 1 unit, detached 
# S2504_C01_003 - 1 unit, attached
# S2504_C05_008 - mobile home

# S2504_C01_004 - 2 units
# S2504_C01_005 - 3 or 4 apts

# S2504_C01_006 - 5- 9 units

# S2504_C01_007 - 10 + units
# Total Units

building_type_vars <- c("S2504_C05_002", "S2504_C05_003",
                        "S2504_C05_004", "S2504_C05_005",
                        "S2504_C05_006", "S2504_C05_007",
                        "S2504_C05_008")

acs_comp_res <- get_acs_vec(vars_vec = building_type_vars, 
                            yr= comparison_year)

acs_current_res <- get_acs_vec(vars_vec = building_type_vars, 
                            yr= year)

acs_comp <- c(acs_comp_res[1] + acs_comp_res[2]  + acs_comp_res[7],
              acs_comp_res[3] + acs_comp_res[4],
              acs_comp_res[5], acs_comp_res[6])
acs_current <- c(acs_current_res[1] + acs_current_res[2] + acs_current_res[7],
              acs_current_res[3] + acs_current_res[4],
              acs_current_res[5], acs_current_res[6])

acs_comp <- c(acs_comp, sum(acs_comp))
tot_units_current = sum(acs_current)
acs_current <- c(acs_current, tot_units_current)

table_3_2 <- data.frame(
  building_type = c("Single-family Units", "2-4 Units", "5-9 Units",
                    "10+ Units", "Total Units"),
  acs.comp = acs_comp,
  acs.comp = acs_current,
  Percent.Change = percent_change(acs_comp, acs_current), 
  Percent.of.Total.Units.Current = acs_current/tot_units_current * 100)

write.csv(table_3_2, "data/table_3_2.csv")

### Table 3.3 Age of Renter-Occupied Housing
year_built_vars <- c("B25036_014", "B25036_015", "B25036_016", 
                     "B25036_017", "B25036_018", "B25036_019", 
                     "B25036_020", "B25036_021", "B25036_022", 
                     "B25036_023")
year_built <- c("2020 or later", "2010 to 2019", 
                "2000 to 2009", "1990 to 1999",
                "1980 to 1989", "1970 to 1979",
                "1960 to 1969", "1950 to 1959",
                "1940 to 1949", "1939 or earlier", "Total Units")

# 2018 test - test checks out
# acs_res <- get_acs_vec(vars_vec = year_built_vars, year = 2018)
num_units <-  acs_res <- get_acs_vec(vars_vec = year_built_vars, yr = year)
tot_units <- sum(num_units)
num_units <- c(num_units, tot_units)

table_3_3 <- data.frame(Year.Rental.Unit.Built = year_built,
                        Number.of.Units = num_units, 
                        Percent.of.Total = num_units/tot_units * 100)

write.csv(table_3_3, "data/table_3_3.csv")

### Table 3.4 Household Tenure for Renters
Year_Renter_Moved_In <- c("2021 or later", "2018 to 2020", "2010 to 2017", 
                          "2000 to 2009", "1990 to 1999", "1989 or Earlier")

Year_Renter_Moved_In_Vars <- c("B25038_010", "B25038_011", "B25038_012",
                               "B25038_013", "B25038_014", "B25038_015")

num_renters <- get_acs_vec(vars_vec = Year_Renter_Moved_In_Vars, yr = year)

table_3_4 <- data.frame(Year.Renter.Moved.In = Year_Renter_Moved_In,
                        Number.of.Renters = num_renters,
                        Percent.of.Total = num_renters/sum(num_renters) * 100)

write.csv(table_3_4, "data/table_3_4.csv")

### Table 3.5 Rental Vacancy Rate

# B25004_002 - total vacant rentals
# S2504_C05_001 - renter-occupied units
table_3_5_vars <- c("B25004_002", "S2504_C05_001")

res_comp <- get_acs_vec(vars_vec = table_3_5_vars, yr = comparison_year)
res_current <- get_acs_vec(vars_vec = table_3_5_vars, yr = year)

table_3_5 <- data.frame( var= c("Total vacant rental units", "% of all rental units vacant"),
                         acs.comp = c(res_comp[1], res_comp[1]/res_comp[2] * 100),
                         acs.current = c(res_current[1], res_current[1]/res_current[2] * 100))

write.csv(table_3_5, "data/table_3_5.csv")


### Table 3.6 Renter Household Income in the past 12 months
renter_household_income <- c("Less than $5,000", "$5,000 to $9,999", "$10,000 to $14,999",
                             "$15,000 to $19,999", "$20,000 to $24,999", "$25,000 to $34,999", 
                             "$35,000 to $49,999", "$50,000 to $74,999", "$75,000 to $99,999",
                             "$100,000 to $149,999", "$150,000 or more")

renter_household_income_vars <- c("B25118_015", "B25118_016", "B25118_017", "B25118_018", 
                                  "B25118_019", "B25118_020", "B25118_021", "B25118_022", 
                                  "B25118_023", "B25118_024", "B25118_025")

renter_household_income_res <- get_acs_vec(vars_vec = renter_household_income_vars)

table_3_6 <- data.frame( household.income = renter_household_income,
                         number.of.renter.occupied.household = renter_household_income_res,
                         percent.renter.occupied = renter_household_income_res/sum(renter_household_income_res) *100)

write.csv(table_3_6, "data/table_3_6.csv")

### Table 3.7 and Table 3.8 Monthly Housing Costs for Renters and Change in Gross Rent Paid
rent <- c("No rent paid", "Less than $500", "$500 to $999", "$1,000 to $1,499", "$1,500 to $1,999", 
          "$2,000 to $2,499", "$2,500 to $2,999", "$3,000 or more")
rent_vars <- c("DP04_0135", "DP04_0127", "DP04_0128", "DP04_0129", "DP04_0130", "DP04_0131",
               "DP04_0132", "DP04_0133")
rent_res = get_acs_vec(vars_vec = rent_vars)

table_3_7 <- data.frame(rent = rent, 
                        number.of.renter.households = rent_res, 
                        percent.renter.households = rent_res/num_renters *100)

median_rent <- get_acs_vec(vars_vec = c("DP04_0134"))

write.csv(table_3_7, "data/table_3_7.csv")

rent_comp = get_acs_vec(vars_vec = rent_vars, yr = comparison_year)

table_3_8 <- data.frame(gross.rent= rent, 
                        number.of.renters.comp = rent_comp,
                        number.of.renters.current = rent_res,
                        percent.change = percent_change(rent_comp, rent_res))

write.csv(table_3_8, "data/table_3_8.csv")

### Table 3.9 Age of Rental Householder Paying 30% or More for Rent
household_ages <- c("15-24 years old", "25-34 years old", "35-64 years old",
                    "65+ years old")
hh_paying_30_to_35_vars <- c("B25072_006", "B25072_013", "B25072_020",
                             "B25072_027")

hh_paying_over_35_vars <- c("B25072_007", "B25072_014", "B25072_021",
                            "B25072_028")

hh_paying_over_35_comp_res <- get_acs_vec(vars_vec = hh_paying_over_35_vars, yr = comparison_year)
hh_total_by_age_comp <- get_acs_vec(vars_vec= c("B25072_002", "B25072_009", "B25072_016", "B25072_023"), yr = comparison_year)
hh_paying_over_35_comp_res <- get_acs_vec(vars_vec = hh_paying_over_35_vars, yr = comparison_year)
hh_paying_30_to_35_comp_res <- get_acs_vec(vars_vec = hh_paying_30_to_35_vars, yr = comparison_year)

hh_paying_over_35_res <- get_acs_vec(vars_vec = hh_paying_over_35_vars)
hh_total_by_age <- get_acs_vec(vars_vec= c("B25072_002", "B25072_009", "B25072_016", "B25072_023"))
hh_paying_over_35_res <- get_acs_vec(vars_vec = hh_paying_over_35_vars)
hh_paying_30_to_35_res <- get_acs_vec(vars_vec = hh_paying_30_to_35_vars)

table_3_9_comp <- data.frame(age.of.rental.householder = household_ages,
                        number.of.renters.paying.30.to.35 = 
                          hh_paying_30_to_35_comp_res,
                        number.of.renters.paying.over.35 = 
                          hh_paying_over_35_comp_res,
                        percent.paying.30 =  (hh_paying_30_to_35_comp_res + hh_paying_over_35_comp_res)/hh_total_by_age_comp * 100)

table_3_9 <- data.frame(age.of.rental.householder = household_ages,
                        number.of.renters.paying.30.to.35 = 
                          hh_paying_30_to_35_res,
                        number.of.renters.paying.over.35 = 
                          hh_paying_over_35_res,
                        percent.paying.30 =  (hh_paying_30_to_35_res + hh_paying_over_35_res)/hh_total_by_age * 100)

write.csv(table_3_9_comp, "data/table_3_9_2018.csv")
write.csv(table_3_9, "data/table_3_9.csv")

### Table 3.10 Subsidized Rental Housing Stock
# SHI - https://www.mass.gov/doc/subsidized-housing-inventory-2/download

renter_occupied <- get_acs_vec(vars_vec = c("S2504_C05_001"))
SHI <- 866
num_units <- c(renter_occupied, SHI)

table_3_10 <- data.frame(. = c("Total Rental Units, (2022 ACS)", "Subsidized Rental Units (DHCD 2023 SHI)"),
                         number.of.units = num_units,
                         percent.of.total = num_units/renter_occupied *100)

write.csv(table_3_10, "data/table_3_10.csv")

### Table 3.11 Changes in Rental Expense
median_rent_var = "DP04_0134"
north_adams_median_comp <- get_acs_vec(vars_vec = c(median_rent_var), yr= comparison_year)
north_adams_median_current <- get_acs_vec(vars_vec = c(median_rent_var))
berkshire_median_comp <- get_acs_vec(vars_vec = c(median_rent_var), yr= comparison_year, geography = "county",
                                     geoid = county_geo_id)
berkshire_median_current <- get_acs_vec(vars_vec = c(median_rent_var), geography = "county", 
                                        geoid = county_geo_id)
ma_median_comp <- get_acs_vec(vars_vec = c(median_rent_var), yr= comparison_year, geography = "state",
                              geoid = state_geo_id)
ma_median_current <- get_acs_vec(vars_vec = c(median_rent_var), geography = "state", 
                                 geoid = state_geo_id)
comp_vec <- c(north_adams_median_comp, berkshire_median_comp, 
              ma_median_comp)
current_vec <- c(north_adams_median_current, berkshire_median_current, 
                 ma_median_current)

table_3_11 <- data.frame( . = c("North Adams", "Berkshire County", "Massachusetts"),
                          comparison = comp_vec, 
                          current = current_vec,
                          percent_change = percent_change(comp_vec, current_vec))

write.csv(table_3_11, "data/table_3_11.csv")

#### Skipping a chunk!

### Table 5.1 Age Distribution of North Adams Senior Householders by Home Type
hh_age <- c("60-64 years old", "65-74 years old", "75-84 years old",
            "85 years old and older", "Total")
num_renters_vars <- c("B25007_018", "B25007_019", "B25007_020",
                      "B25007_021")
num_owners_vars <- c("B25007_008", "B25007_009", "B25007_010",
                     "B25007_011")
num_renters_res <- get_acs_vec(vars_vec = num_renters_vars)
num_owners_res <- get_acs_vec(vars_vec = num_owners_vars)
total_senior_pop <- sum(num_renters_res) + sum(num_owners_res)
num_renters_res <- c(num_renters_res, sum(num_renters_res))
num_owners_res <- c(num_owners_res, sum(num_owners_res))

table_5_1 <- data.frame(householder.age = hh_age,
                        num.renters = num_renters_res,
                        percent.total.seniors = num_renters_res/total_senior_pop  *100,
                        num.owners = num_owners_res,
                        percent.total.seniors = num_owners_res/total_senior_pop * 100)
write.csv(table_5_1, "data/table_5_1.csv")

### Table 5.2 Senior Populaton and Projections
age_group <- c("55-59 years old", "60-64 years old", "65-74 years old",
               "75-84 years old", "85 years old and older", "total")

age_vars <- c("DP05_0013", "DP05_0014", "DP05_0015", "DP05_0016", "DP05_0017")

age_comp_res <- get_acs_vec(vars_vec = age_vars, yr = comparison_year)
age_comp_res <- c(age_comp_res, sum(age_comp_res))
age_current_res <- get_acs_vec(vars_vec = age_vars)
age_current_res <- c(age_current_res, sum(age_current_res))

proj_2035 <- c(692, 747, 768 + 865, 786 + 528, 345)
proj_2035 <- c(proj_2035, sum(proj_2035))
proj_2035_total <- sum(420, 398, 480, 753, 1053, 631,
                       515,588,660,676,723,692,747,768,
                       865,786,528,345)

table_5_2 <- data.frame(age.group = age_group, 
                        comp.year = age_comp_res,
                        current.year = age_current_res,
                        projections.2035 = proj_2035, 
                        percent.total.2035 = proj_2035/proj_2035_total *100,
                        percent.change.comp.proj = percent_change(age_comp_res, proj_2035))

write.csv(table_5_2, "data/table_5_2.csv")

### Table 5.3 Household Incomes of Senior Households in North Adams
income_and_benefits <- c("Less than $10,000", "$10,000 to $14,999", "$15,000 to $19,999",
                         "$20,000 to $24,999", "$25,000 to $29,999", "$30,000 to $34,999",
                         "$35,000 to $39,999", "$40,000 to $44,999", "$45,000 to $49,999",
                         "$50,000 to $59,999", "$60,000 to $74,999", "$75,000 to $99,999",
                         "$100,000 to $124,999", "$125,000 to $149,999", "$150,000 to $199,999",
                         "$200,000 or more")
income_and_benefits_vars <- c("B19037_054", "B19037_055", "B19037_056", 
                              "B19037_057", "B19037_058", "B19037_059",
                              "B19037_060", "B19037_061", "B19037_062",
                              "B19037_063", "B19037_064", "B19037_065",
                              "B19037_066", "B19037_067", "B19037_068",
                              "B19037_069")
income_and_benefits_res <- get_acs_vec(vars_vec = income_and_benefits_vars)
income_and_benefits_total <-  sum(income_and_benefits_res)

table_5_3 <- data.frame(Income.And.Benefits = income_and_benefits, 
                        Number.of.Households = income_and_benefits_res,
                        Percent.of.Total = income_and_benefits_res/income_and_benefits_total *100)

write.csv(table_5_3, "data/table_5_3.csv")

### Table 5.4 Tenure by Year Senior (65+) Householder
year_moved_into_home <- c("2021 or later", "2018 to 2020", "2010 to 2017",
                          "2000 to 2009", "1990 to 1999", "1989 or earlier", "Total")
num_renters_vars <- c("B25128_040", "B25128_041", "B25128_042", "B25128_043",
                      "B25128_044", "B25128_045")
num_owners_vars <- c("B25128_018", "B25128_019", "B25128_020", "B25128_021",
                     "B25128_022", "B25128_023")
num_renters_res <- get_acs_vec(vars_vec = num_renters_vars)
total_renters <- sum(num_renters_res)
num_renters_res <- c(num_renters_res, total_renters)

num_owners_res <- get_acs_vec(vars_vec = num_owners_vars)
total_owners <- sum(num_owners_res)
num_owners_res <- c(num_owners_res, total_owners)

table_5_4 <- data.frame(Year.Moved.Into.Home = year_moved_into_home,
                        Num.Renters = num_renters_res,
                        Percent.Renters = num_renters_res/total_renters * 100,
                        Num.Owners = num_owners_res,
                        Percent.Owners = num_owners_res/total_owners  *100)
write.csv(table_5_4, "data/table_5_4.csv")

### Table 5.5 Percentage of Senior Renters' Income Spent on Housing Costs
status <- c("Paying 30% or more of income in rent", "Total Senior (65+) renters")

# B25072_027 is 30 to 35% cost-burdened
# B25072_028 is over 35% cost-burdened
cost_burdened_vars <- c("B25072_027", "B25072_028", "B25072_023")
cost_burdened_res <- get_acs_vec(vars_vec = cost_burdened_vars)
cost_burdened_res <- c(cost_burdened_res[1] + cost_burdened_res[2], cost_burdened_res[3])

table_5_5 <- data.frame(. = status,
                        Number.of.Housholds = cost_burdened_res,
                        Percent.of.Households = cost_burdened_res/cost_burdened_res[2] *100)
write.csv(table_5_5, "data/table_5_5.csv")

### Table 5.6 Percentage of Senior Owners' Income Spent on Housing Costs
amount_spent_on_housing <- c("Less than 20%", "20.0 to 24.9%", "25.0 to 29.9%",
                             "30.0 to 34.9%", "35.0% or more", "Not computed",
                             "Total Senior (65+) Owners")
amount_spent_on_housing_vars <- c("B25093_024", "B25093_025", "B25093_026",
                                  "B25093_027", "B25093_028", "B25093_029",
                                  "B25093_023")
amount_spent_on_housing_res <- get_acs_vec(vars_vec = amount_spent_on_housing_vars)

table_5_6 <- data.frame(Amount.Spent.On.Housing = amount_spent_on_housing,
                        Number.of.Households = amount_spent_on_housing_res,
                        Percent.of.Households = amount_spent_on_housing_res/amount_spent_on_housing_res[7] * 100)

write.csv(table_5_6, "data/table_5_6.csv")

### Table 5.7 Seniors with Disabilities or Limitations
disability_or_limitation <- c("Hearing Difficulty", "Vision Difficulty",
                              "Cognitive Difficulty", "Ambulatory Difficulty",
                              "Self-Care Difficulty", "Independent Living Difficulty",
                              "Total")

disability_or_limitation_vars <- c("S1810_C02_026", "S1810_C02_036", "S1810_C02_044",
                                   "S1810_C02_052", "S1810_C02_060", "S1810_C02_067")

disability_or_limitation_res <- get_acs_vec(vars_vec = disability_or_limitation_vars)
disability_or_limitation_res <- c(disability_or_limitation_res, sum(disability_or_limitation_res))
total_seniors = get_acs_vec(vars_vec = c("S1810_C01_026"))

table_5_7 <- data.frame(Disability.or.Limitation = disability_or_limitation,
                        Number.of.Seniors = disability_or_limitation_res, 
                        Percent.of.Seniors = disability_or_limitation_res/total_seniors * 100)

write.csv(table_5_7, "data/table_5_7.csv")

### Table 6.1 Residents with Disabilities or Limitations
disability_or_limitation <- c("Hearing Difficulty", "Vision Difficulty", 
                              "Cognitivie Difficulty", "Ambulatory Difficulty", 
                              "Self-Care Difficulty", "Independent Living Difficulty",
                              "Total")
disability_or_limitation_vars <- c("S1810_C02_019", "S1810_C02_029",
                                   "S1810_C02_039", "S1810_C02_047",
                                   "S1810_C02_055", "S1810_C02_063")

disability_or_limitation_res <- get_acs_vec(vars_vec = disability_or_limitation_vars)
disability_or_limitation_res <- c(disability_or_limitation_res, sum(disability_or_limitation_res))

total_population <- get_acs_vec(vars_vec = c("S1810_C01_001"))

table_6_1 <- data.frame(disability.or.limitaton = disability_or_limitation,
                        number = disability_or_limitation_res,
                        percent.of.total.population = disability_or_limitation_res/total_population *100)

write.csv(table_6_1, "data/table_6_1.csv")

### Table 7.1
# 0 homes on Zillow.com that are foreclosed or preforeclosed
# RealtyTrac - 5, all preforeclosure
# 0 on homepath.fanniemae
# did this chart https://docs.google.com/spreadsheets/d/1ezSLgyxrR83BfTii6AoxmCCZ6EHuOGzlYKPCbriWE5o/edit?usp=sharing


### Table 7.2 Assessed Condition of Residences
assessor_data <- read.csv("data/parcel_data_july_24.csv")
assessor_data$USE.CODE <- as.numeric(assessor_data$USE.CODE)

residential <- assessor_data %>% filter(USE.CODE>99) %>% filter(USE.CODE <130)

# Delapitated is misspelled in the data 

assessed_levels <- c("Excellent", "Very Good", "Good-VG",
  "Good", "Avg-Good", "Average", "Fair-Avg",
  "Fair", "Poor", "Very Poor", "Delapitated", "")

residential$CONDITION <- residential$CONDITION %>% str_squish %>% factor
residential$CONDITION <- factor(residential$CONDITION, levels = assessed_levels)

number_of_properties <- table(residential$CONDITION)[1:11]
total_properties <- sum(number_of_properties)
less_than_good <- sum(number_of_properties[5:11])

number_of_properties <- c(number_of_properties,total_properties, less_than_good)

table_7_2 <- data.frame(Assessed.Condition = c(assessed_levels[1:11], "Total", "<Good Condition"),
                        Number.of.Properties = number_of_properties,
                        Percentage.of.Properties = number_of_properties/total_properties *100)

write.csv(table_7_2, "data/table_7_2.csv")

### Figure 7.1 Assessed Condition of Residences
figure_7_1_data <- as.data.frame(number_of_properties[1:11])
rownames(figure_7_1_data)[11] <- "Dilapidated"
colnames(figure_7_1_data) <- c("count")
figure_7_1_data$x <- rownames(figure_7_1_data) %>% factor(levels = rownames(figure_7_1_data))

figure_7_1 <-  ggplot(figure_7_1_data, aes(x =x, y = count)) + geom_col()

### Table 7.3 (number 2) Assessed Condition of Residences by Neighborhood
# We're using residential dataset from the previous section
# We need to get the neighborhood data
neighborhood_key <- read.csv("data/neighborhood_key.csv")
neighborhood_key$MAP_PAR_ID <- str_squish(neighborhood_key$MAP_PAR_ID)

residential$MAP.BLOCK.LOT <- str_squish(residential$MAP.BLOCK.LOT)

merged_residential_neighborhood <- merge(residential, neighborhood_key, by.x ="MAP.BLOCK.LOT",
                                      by.y="MAP_PAR_ID")

table_7_3 <- merged_residential_neighborhood %>% group_by(NHOOD, CONDITION) %>%
  summarize(Freq=n())
table_7_3 <- table_7_3[1:121,]

pivot_wider(table_7_3, names_from = CONDITION, values_from = Freq)
cast(table_7_3, NHOOD~Condition)

table_7_3 <- acast(table_7_3, NHOOD~CONDITION)
table_7_3 <- as.data.frame(table_7_3)
table_7_3$Dilapidated <- table_7_3$Delapitated
table_7_3 <- table_7_3 %>% select(-c(Delapitated, V12))
table_7_3[is.na(table_7_3)] <- 0
table_7_3$Total <- table_7_3$Excellent + table_7_3$`Very Good`+
  table_7_3$`Good-VG`+ table_7_3$Good + table_7_3$`Avg-Good` + table_7_3$Average +
  table_7_3$`Fair-Avg`+ table_7_3$Fair + table_7_3$Poor + table_7_3$`Very Poor` + 
  table_7_3$Dilapidated
table_7_3$Less_Than_Good <- (table_7_3$`Avg-Good` + table_7_3$Average +
  table_7_3$`Fair-Avg`+ table_7_3$Fair + table_7_3$Poor + table_7_3$`Very Poor` + 
  table_7_3$Dilapidated)/ table_7_3$Total * 100
table_7_3$Greater_Than_Eq_Good <- (table_7_3$Excellent + table_7_3$`Very Good`+
                                     table_7_3$`Good-VG`+ table_7_3$Good)/ table_7_3$Total * 100
write.csv(table_7_3, "data/table_7_3.csv")

### Table 8.1 Seasonal Employment
# https://lmi.dua.eol.mass.gov/lmi/employmentandwages
# just pulled manually

table_8_1 <- data.frame( . = c("February Employment", "July Employment", "Percent Difference"),
                         North.Adams.2023 = c(5378,5531, percent_change(5378,5531)),
                         Berkshire.County.2023 = c(56972, 60706, percent_change(56972, 60706)))

write.csv(table_8_1, "data/table_8_1.csv")

### Table 8.2 Seasonal Housing
first_row <- c("Total Housing Units", "Seasonal, Recreational, or Occasional Use",
               "% Seasonal")
housing_vars <- c("DP04_0001", "B25004_006")
north_adams_res <- get_acs_vec(vars_vec =  housing_vars)
north_adams_res <- c(north_adams_res, north_adams_res[2]/north_adams_res[1] *100)

berkshire_county_res <- get_acs_vec(vars_vec =housing_vars, geoid = county_geo_id,
                                    geography = "county")
berkshire_county_res <- c(berkshire_county_res, berkshire_county_res[2]/berkshire_county_res[1] *100)

table_8_2 <- data.frame(. = first_row, North.Adams = north_adams_res,
                        Berkshire.County = berkshire_county_res)

write.csv(table_8_2, "data/table_8_2.csv")