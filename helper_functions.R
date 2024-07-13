pl_var <- load_variables(2020, dataset = c("pl"))
dhc_var <- load_variables(2020, dataset = c("dhc"))
dp_var <- load_variables(2020, dataset = c("dp"))
acs_var <- load_variables(2022, dataset = c("acs5"))

dhc_var[grep("Median", dhc_var$label, ignore.case =T),]

s1701 <- acs_var[grep("S1701", acs_var$name, ignore.case =T),]

get_acs_vec <- function(geoid = 2500346225, vars_vec, dataset = "acs5", geography= "county subdivision",
                        year = 2022) {
  acs_vec <- c()
  if (geography == "county subdivision"){
    census_res <- get_acs(geography = "county subdivision", variables = vars_vec,
                          state = "MA", county = "Berkshire", year = year)
  }
  
  if (geography == "county") {
    census_res <- get_acs(geography = "county", variables = vars_vec,
                          state = "MA", 
                          county = "Berkshire",
                          year = year)
  }
  
  location_res <- filter(census_res, GEOID==geoid)
  
  for(i in 1:length(vars_vec)){
    acs_vec <- c(acs_vec, filter(location_res, variable == vars_vec[i])$estimate) 
  }
  
  return (acs_vec)
}

percent_change = function(old, new) {
  return((new - old)/old * 100)
}
