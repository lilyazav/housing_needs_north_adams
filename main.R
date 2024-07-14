library(tidycensus)
library(dplyr)
library(tidyr)
library(ggplot2)

# https://walker-data.com/tidycensus/articles/basic-usage.html

key <- readr::read_file("census_api_key.txt")
census_api_key(key)

## 2. Demographics
### Table 2.1 Population Change, 1960 - 2024

places <- c("North Adams", "Williamstown", "Clarksburg", "Florida", 
            "Savoy", "Adams", "New Ashford", "Berkshire County")

# get 2020 data

population <- get_decennial(geography = "county subdivision", 
                            variables = "P1_001N",
                            state = "MA",
                            county = "Berkshire",
                            year = 2020)

# I manually selected the geo_ids we want from the resuls above 

geo_ids <- c(2500346225, 2500379985, 2500314010, 2500324120, 2500360225, 
             2500300555, 2500344385)

y_2020 <- c()

for(i in 1:length(geo_ids)){
  y_2020 <- c(y_2020, population$value[population$GEOID == geo_ids[i]])
}


population_berkshire <- get_decennial(geography = "county", 
                            variables = "P1_001N",
                            state = "MA",
                            county = "Berkshire",
                            year = 2020)

y_2020 <- c(y_2020, population_berkshire$value)

# Get 2022
# Using 5-year ACS estimates ending in 2022 
# 1 year estimates are only available for places with over 65,000 people, so they must
# have also used 5 -year data

population_22 <- get_acs(geography = "county subdivision", 
                            variables = "DP05_0001",
                            state = "MA",
                            county = "Berkshire",
                            year = 2022)

y_2022 <- c()

for(i in 1:length(geo_ids)){
  y_2022 <- c(y_2022, population_22$estimate[population_22$GEOID == geo_ids[i]])
}

population_22_berk <- get_acs(geography = "county", 
                         variables = "DP05_0001",
                         state = "MA",
                         county = "Berkshire",
                         year = 2022)

y_2022 <- c(y_2022, population_22_berk$estimate)

table_2_1 <- data.frame(location =  places, 
                        y_1960 = c(19905, 7322, 1741, 569, 277, 12391, 165, 142135),
                        y_1970 = c(19195, 8454, 1987, 672, 322, 11772, 183, 149402),
                        y_1980 = c(18063, 8741, 1871, 730, 644, 10381, 159, 145110),
                        y_1990 = c(16964, 8426, 1599, 723, 634, 9445, 192, 139352),
                        y_2000 = c(14691, 8418, 1682, 676, 705, 8809, 247, 134953),
                        y_2010 = c(13708, 7754, 1702, 752, 692, 8485, 228, 131219),
                        y_2020 = y_2020,
                        y_2022 = y_2022)

write.csv(table_2_1,"table_2_1.csv")

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
                                      county = "Berkshire",
                                      year = 2020,
                                     sumfile = "dp")

zero_to_19 <- sum(filter(population_up_to_19, GEOID==2500346225)$value)

population_20_to_34 <- get_decennial(geography = "county subdivision", 
                                     variables = c("DP1_0006C", "DP1_0007C", "DP1_0008C" ),
                                     state = "MA",
                                     county = "Berkshire",
                                     year = 2020,
                                     sumfile = "dp")

twenty_to_34 <- sum(filter(population_20_to_34, GEOID==2500346225)$value)

population_35_to_54 <- get_decennial(geography = "county subdivision", 
                                     variables = c("DP1_0009C", "DP1_0010C", "DP1_0011C", "DP1_0012C"),
                                     state = "MA",
                                     county = "Berkshire",
                                     year = 2020,
                                     sumfile = "dp")

thirty5_to_54 <- sum(filter(population_35_to_54, GEOID==2500346225)$value)

population_55_plus <- get_decennial(geography = "county subdivision", 
                                     variables = c("DP1_0013C", "DP1_0014C", "DP1_0015C", 
                                                   "DP1_0016C", "DP1_0017C", "DP1_0018C", 
                                                   "DP1_0019C"),
                                     state = "MA",
                                     county = "Berkshire",
                                     year = 2020,
                                     sumfile = "dp")

fifty5_plus <- sum(filter(population_55_plus, GEOID==2500346225)$value)

total_pop <- sum(zero_to_19, twenty_to_34, thirty5_to_54, fifty5_plus)

median_age <- get_decennial(geography = "county subdivision", variables = "P13_001N",
                                                  state = "MA",
                                                  county = "Berkshire",
                                                  year = 2020,
                                                  sumfile = "dhc")

median_age_na <- filter(median_age, GEOID==2500346225)$value
tot_2020 = c(zero_to_19, twenty_to_34, thirty5_to_54, fifty5_plus, total_pop, median_age_na)
per_2020 = tot_2020[1:length(tot_2020) -1]/total_pop * 100

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
zero_to_19_2022 <- get_acs(geography = "county subdivision", 
                         variables = c("DP05_0005E", "DP05_0006E", "DP05_0007E", "DP05_0008E"),
                         state = "MA",
                         county = "Berkshire",
                         year = 2022)
zero_to_19_2022_val <- sum(filter(zero_to_19_2022, GEOID==2500346225)$estimate)

# twenty to 34, 2022
twenty_to_34_2022 <- get_acs(geography = "county subdivision", 
                           variables = c("DP05_0009E", "DP05_0010E"),
                           state = "MA",
                           county = "Berkshire",
                           year = 2022)
twenty_to_34_2022_val <- sum(filter(twenty_to_34_2022, GEOID==2500346225)$estimate)

# 35 to 54, 2022 
thirty5_to_54_2022 <- get_acs(geography = "county subdivision", 
                             variables = c("DP05_0011E", "DP05_0012E"),
                             state = "MA",
                             county = "Berkshire",
                             year = 2022)
thirty5_to_54_2022_val <- sum(filter(thirty5_to_54_2022, GEOID==2500346225)$estimate)

over_fifty5_2022 <- get_acs(geography = "county subdivision", 
                            variables = c("DP05_0013E", "DP05_0014E", "DP05_0015E", 
                                          "DP05_0016E", "DP05_0017E"),
                            state = "MA",
                            county = "Berkshire",
                            year = 2022)

over_fifty5_2022_val <- sum(filter(over_fifty5_2022, GEOID==2500346225)$estimate)

tot_22 <- sum(zero_to_19_2022_val, twenty_to_34_2022_val, thirty5_to_54_2022_val, 
              over_fifty5_2022_val)

# Median age, 2022, North Adams

median_age_2022 <- get_acs(geography = "county subdivision", 
                            variables = c("DP05_0018E"),
                            state = "MA",
                            county = "Berkshire",
                            year = 2022)

median_age_2022_val <- filter(median_age_2022, GEOID==2500346225)$estimate

tot_2022 <- c(zero_to_19_2022_val, twenty_to_34_2022_val, thirty5_to_54_2022_val, 
              over_fifty5_2022_val, tot_22, median_age_2022_val)

per_2022 <- tot_2022[1:length(tot_2022) -1]/tot_22 * 100


per_change_20_to_22 <- (tot_2022 - tot_2020)/tot_2020 * 100

age_cohorts <- c("0-19 years old", "20-34 years old",
                 "35-54 years old", "55+ years old")

table_2_2 <- data.frame(age_cohort = c(age_cohorts, "Total Population", "Median Age"),
                        tot_2010 = c(3359, 2945, 3506, 3898, 13708, 38.9),
                        tot_2020 = tot_2020, 
                        per_2020 = c(per_2020, NA), 
                        tot_2022 = tot_2022, 
                        per_2022 = c(per_2022, NA),
                        per_change_20_to_22 = per_change_20_to_22)

write.csv(table_2_2,"table_2_2.csv")

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

write.csv(table_2_3, "table_2_3.csv")

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
berkshire_race <- get_acs(geography = "county subdivision", 
                            variables = race_vars,
                            state = "MA",
                            county = "Berkshire",
                            year = 2022)

north_adams_race <- filter(berkshire_race, GEOID==2500346225)

race_2022 <- c()

for(i in 1:length(race_vars)){
  race_2022 <- c(race_2022, filter(north_adams_race, variable == race_vars[i])$estimate) 
}

race_2022 <- c(race_2022, tot_22)

table_2_4 <- data.frame(race = c("White alone", "Black or African American alone", 
                                 "Hispanic or Latino", "American Indian or Alaskan Native alone",
                                 "Asian alone", "Native Hawaiian or Other Pacific Islander alone",
                                 "Some other race alone", "Two or more races", "Total"),
                        race_2022 = race_2022,
                        per_total = race_2022/tot_22 * 100
)

write.csv(table_2_4, "table_2_4.csv")

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

write.csv(table_2_5, "table_2_5.csv")

### Table 2.6 Unemployment Rates and Labor Force 
# https://lmi.dua.eol.mass.gov/LMI/LaborForceAndUnemployment#
unemployment_data <- read.csv("LURReport.csv")
county_data <- read.csv("county_data.csv")
county_data <- cbind(Area = "Berkshire County", county_data)
# state not seasonally adj
# https://lmi.dua.eol.mass.gov/LMI/LaborForceAndUnemployment/LURResults?A=01&GA=000025&TF=1&Y=&Sopt=N&Dopt=TEXT
state_data <- data.frame(Area = "Massachusetts", Labor.Force = 3800827, 
                         Employed = 3657969, Unemployed = 142858, Area.Rate = 3.8, 
                         Massachusetts.Rate = 3.8)

table_2_6 <- rbind(unemployment_data, county_data, state_data)
write.csv(table_2_6, "table_2_6.csv")


### Table 2.7 Households by Income Level in North Adams
income_vars <- c("DP03_0052", "DP03_0053", "DP03_0054", "DP03_0055", "DP03_0056",
                 "DP03_0057", "DP03_0058", "DP03_0059", "DP03_0060", 
                 "DP03_0061", "DP03_0051")

income_hh_18 <- get_acs_vec(vars_vec = income_vars, year = 2018)
income_hh_22 <- get_acs_vec(vars_vec = income_vars)

income_levels <- c("Less than $10,000", "$10,000 to $14,999", "$15,000 to $24,999",
                   "$25,000 to $34,999", "$35,000 to $49,999", "$50,000 to $74,999",
                   "$75,000 to $99,999", "$100,000 to $149,999", "$150,000 to $199,999",
                   "$200,000 or More", "Total Households")

table_2_7 <- data.frame(Income.Level = income_levels, 
                        Number.of.Households.2018 = income_hh_18, 
                        Number.of.Households.2022 = income_hh_22,
                        Percent.Total.22 = income_hh_22/5816 * 100,
                        Percent.Change = (income_hh_22 - income_hh_18)/income_hh_18  * 100
                        )

write.csv(table_2_7, "table_2_7.csv")

### Figure 2.1 Households by Income in North Adams
fig_2_1 <- data.frame(Income.Level = income_levels, 
                      hh_2018 = income_hh_18, 
                      hh_2022 = income_hh_22)
fig_2_1 <- fig_2_1[1:10, ]
  
fig_2_1_long <- gather(fig_2_1, year, total, hh_2018:hh_2022)
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

write.csv(table_2_8, "table_2_8.csv")

### Table 2.9 Average Weekly Wages Compared to County and State, 2022
NA_avg_weekly = c(960, 984)
Berk_avg_weekly = c(1082, 1254)
MA_avg_Weekly = c(1713, 1826)

table_2_9 <- data.frame(Job.Type = job_types[1:2], 
                        North.Adams.Avg.Weekly = NA_avg_weekly,
                        Berk.Avg.Weekly = Berk_avg_weekly,
                        MA.Avg.Weekly = MA_avg_Weekly,
                        Per.of.MA.Avg.Weekly = NA_avg_weekly/MA_avg_Weekly * 100)

write.csv(table_2_9, "table_2_9.csv")

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

industry_vec_22 <- get_acs_vec(vars_vec = industry_vars)
industry_vec_18 <- get_acs_vec(vars_vec = industry_vars, year = 2018)

table_2_10 <- data.frame(Industry = industry, 
                         acs_2018 = industry_vec_18, 
                         acs_2022 = industry_vec_22, 
                         per_of_jobs_22 = industry_vec_22/industry_vec_22[1] * 100, 
                         per_change = (industry_vec_22 - industry_vec_18)/industry_vec_18 * 100)

write.csv(table_2_10, "table_2_10.csv")

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

write.csv(table_2_11, "table_2_11.csv")

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

write.csv(table_2_12, "table_2_12.csv")

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

write.csv(table_2_13, "table_2_13.csv")

### Figure 2.2 Residential Use - to do later

### Table 2.14 
# 2018 combined 10 - 19 and 20 +, I left them separate
struc_type <- c("1 unit, detached", "1 unit, attached", "2 units", 
                "3 to 4 units", "5 to 9 units", "10 - 19 units", 
                "20+ units", "Mobile Home", 
                "Boat, RV, van, etc", "Total units")
struc_vars <- c("DP04_0007", "DP04_0008", "DP04_0009", "DP04_0010", 
                "DP04_0011", "DP04_0012", "DP04_0013", "DP04_0014", 
                "DP04_0015", "DP04_0006")
struc_vec_18 <- get_acs_vec(vars_vec = struc_vars, year = 2018)
struc_vec_22 <- get_acs_vec(vars_vec = struc_vars)

table_2_14 <- data.frame(Structure.Type = struc_type, 
                         acs.2018 = struc_vec_18,
                         acs.2022 = struc_vec_22, 
                         Percent.Housing.Units = struc_vec_22/struc_vec_22[10]*100)

write.csv(table_2_14, "table_2_14.csv")

### Table 2.15 Housing Stock by Number of Bedrooms
num_bedrooms <- c("Studio, 1 bedroom", "2 bedrooms", "3 bedrooms", 
                  "4 bedrooms", "5+ bedrooms", "Total units")

num_bedrooms_vars <- c("DP04_0039", "DP04_0040", "DP04_0041", 
                       "DP04_0042", "DP04_0043", "DP04_0044", 
                       "DP04_0038")

num_bedrooms_vec_18 <- get_acs_vec(vars_vec = num_bedrooms_vars, year = 2018)
# Need to combine DP04_0039E (no bedrooms), DP04_0040 (1 bedroom)
num_bedrooms_vec_18 <- c(num_bedrooms_vec_18[1] + num_bedrooms_vec_18[2], 
                         num_bedrooms_vec_18[3:length(num_bedrooms_vec_18)])

num_bedrooms_vec_22 <- get_acs_vec(vars_vec = num_bedrooms_vars)
# Need to combine DP04_0039E (no bedrooms), DP04_0040 (1 bedroom)
num_bedrooms_vec_22 <- c(num_bedrooms_vec_22[1] + num_bedrooms_vec_22[2], 
                         num_bedrooms_vec_22[3:length(num_bedrooms_vec_22)])

table_2_15 <- data.frame(Num.Bedrooms = num_bedrooms, 
                         acs_2018 = num_bedrooms_vec_18,
                         acs_2022 = num_bedrooms_vec_22,
                         Percent.Total.Housing.2022 = num_bedrooms_vec_22/num_bedrooms_vec_22[6] * 100,
                         Percent.Change = (num_bedrooms_vec_18 - num_bedrooms_vec_22)/num_bedrooms_vec_18 * 100)

write.csv(table_2_15, "table_2_15.csv")

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

write.csv(table_2_16, "table_2_16.csv")

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
write.csv(table_2_17, "table_2_17.csv")

### Table 2.19 FY 2022 Income Limits, Berkshire County
# https://www.huduser.gov/portal/datasets/il/il2022/2022summary.odn

income_category <- c("extremely low income, 30%", 
                     "very low income, 50%",
                     "low income, 80%")

median_family_income <- 92100
eli_30 <- c(19800,	22600,	25450,	28250,	32470,	37190,	41910,	46630)
vli_50 <- c(32950,	37650,	42350,	47050,	50850,	54600,	58350,	62150)
li_80 <- c(52750,	60250,	67800,	75300,	81350,	87350,	93400,	99400)

table_2_19 <- rbind(c("Income Category", 1:8),
  c(income_category[1], eli_30),  
  c(income_category[2], vli_50), 
  c(income_category[3], li_80))

write.csv(table_2_19, "table_2_19.csv")


### Table 2.18 Percentage of Households by Area Median Income
# It's easier to do this table after 2.19
# I'm copying the methodology from the North Adams Housing Needs Assessment, 2020
# The income buckets don't line up to the HUD ELI/VLI/LI amounts. 
# However, they assume that households are evenly distributed within those buckets,
# and if, for example, the ELI is 53% of the way into a income bracket ($20,300 is 
# roughly 53% of the way between 15k and 25k), then 53% of the households in that 
# bracket are included in the calculation. 

t218_locations <-  c("Berkshire County", places[1:7])
t218_geoids <- c(25003, geo_ids)

eli <- table_2_19[2,3] %>% strtoi
vli <- table_2_19[3, 3] %>% strtoi
li <- table_2_19[4, 3] %>% strtoi

bucket_vars = c("DP03_0052", "DP03_0053", "DP03_0054",
                "DP03_0055", "DP03_0056", "DP03_0057", 
                "DP03_0058", "DP03_0059", "DP03_0060",
                "DP03_0061")

income_buckets <- data.frame( 
  var.names = bucket_vars,
  upper.limts = c(9999, 14999, 24999, 34999, 
                  49999, 74999, 100000, 150000, 
                  199999, Inf))

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


get_2_18_row <- function(i, eli, vli, li, year = 2022){
  loc <- t218_locations[i]
  geo <- t218_geoids[i]
  
  if(loc == "Berkshire County"){
    geography = "county"
  } else {
    geography = "county subdivision"
  }
  
  hh_acs <- get_acs_vec(vars_vec = bucket_vars, geoid = geo, geography = geography, year = year)
  
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

write.csv(table_2_18, "table_2_18.csv")

### Table 2.20 Prevalence of Housing Cost Burden
t2_20_loc <- places
t2_20_geoids <- c(geo_ids, 25003)

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

get_t2_20_row <- function(i, year = 2022){
  if(t2_20_loc[i] == "Berkshire County"){
    geography <- "county"
  } else {
    geography <- "county subdivision"
  }
  
  hh_nums <-  get_acs_vec(geoid = t2_20_geoids[i], 
                          year = year,
                          geography = geography,
                          vars_vec = c(num_renters, num_owners))
  
  renter_cost_burdened <- get_acs_vec(geoid = t2_20_geoids[i], 
                                     geography = geography,
                                     year = year,
                                     vars_vec = renter_vars) %>% sum
    
  owner_cost_burdened <- get_acs_vec(geoid = t2_20_geoids[i], 
                           geography = geography,
                           year = year,
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
  table_2_20 <- rbind(table_2_20, get_t2_20_row(i, 2022))
}

names(table_2_20) <- c("Location", "Number of Rental Households", 
                       "Renters Burdened", "% Renters Burdened", "Number of Owner Households", "Owners Burdened", "% Owners Burdened")

### Figure 2.4 Prevalence of Housing Cost Burden
write.csv(table_2_20, "table_2_20.csv")

fig_2_4 <- table_2_20 %>% select(Location, '% Renters Burdened', '% Owners Burdened')

fig_2_4_long <- gather(fig_2_4, type, percent, '% Renters Burdened':'% Owners Burdened')

plot <- ggplot(fig_2_4_long, aes(factor(Location, level = t2_20_loc), as.numeric(percent), fill=type))
plot <- plot + geom_bar(stat = "identity", position = 'dodge') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
plot



##
##
##
## Skipping - to go back to

### Table 3.1

housing_type <- c("Owner-Occupied", "Renter-Occupied", "Total Occupied Units")
t_3_1_vars <- c("S2504_C03_001", "S2504_C05_001", "S2504_C01_001")

num_18 <- get_acs_vec(vars_vec = t_3_1_vars, year = 2018)
num_22 <- get_acs_vec(vars_vec = t_3_1_vars)

table_3_1 <- data.frame(Housing.Type = housing_type,
                        Units.2018 = num_18, 
                        Percent.2018 = num_18/(num_18[3]) * 100,
                        Units.2022 = num_22, 
                        Percent.2022 = num_22/(num_22[3]) * 100,
                        Per.Change = percent_change(num_18, num_22))

write.csv(table_3_1, "table_3_1.csv")

### Table 3.2 Number of Rental Units in Each Buildings
