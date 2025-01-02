# pl_var <- load_variables(2020, dataset = c("pl"))
# dhc_var <- load_variables(2020, dataset = c("dhc"))
# dp_var <- load_variables(2020, dataset = c("dp"))
# acs_var <- load_variables(year, dataset = c("acs5"))
# dhc_var[grep("Median", dhc_var$label, ignore.case =T),]
# s1701 <- acs_var[grep("S1701", acs_var$name, ignore.case =T),]

get_acs_vec <- function(geoid = geo_id, vars_vec, dataset = "acs5", geography= "county subdivision",
                        yr = year, cnty = county) {
  
  acs_vec <- c()
  
  if(dataset == "decennial") {
    census_res <- get_decennial(geography = geography, variables = vars_vec, state = "MA", county = cnty, year = yr)
  } else if (geography == "state"){
    census_res <- get_acs(geography = "state", variables = vars_vec,
                          state = "MA", year = yr)
  } else {
    census_res <- get_acs(geography = geography, variables = vars_vec,
                          state = "MA", county = cnty, year = yr)
  } 

  location_res <- filter(census_res, GEOID==geoid)
  
  if(dataset == "decennial"){
    for(i in 1:length(vars_vec)){
      acs_vec <- c(acs_vec, filter(location_res, variable == vars_vec[i])$value) 
    }
  } else {
    for(i in 1:length(vars_vec)){
      acs_vec <- c(acs_vec, filter(location_res, variable == vars_vec[i])$estimate) 
    }
  }
  
  return (acs_vec)
}

percent_change = function(old, new) {
  return((new - old)/old * 100)
}


# Getting var names
# B vars
# https://api.census.gov/data/2022/acs/acs5/groups/B25118.html
# S vars
# https://api.census.gov/data/2022/acs/acs5/subject/groups/S2504.html
# DP vars
# https://api.census.gov/data/2022/acs/acs5/profile/groups/DP04.html