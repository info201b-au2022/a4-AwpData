library("tidyverse")

# For section 6 table
get_southern_populations <- incarceration_df %>% 
  filter(state %in% c("TN", "MS", "SC")) %>% 
  filter(year == "2018") %>% 
  select(county_name, state, total_pop, black_pop_15to64, white_pop_15to64) %>% 
  filter(county_name %in% c("Coahoma County", "Tunica County", "Shelby County", "Clarendon County")) %>% 
  rename("County" = "county_name") %>% 
  rename("State" = "state") %>% 
  rename("Total Population" = "total_pop") %>% 
  rename("Black Population (15 - 64)" = "black_pop_15to64") %>% 
  rename("White Population (15 - 64)" = "white_pop_15to64")


get_southern_populations[["Total Population"]] <- format(get_southern_populations[["Total Population"]], big.mark = ",")
get_southern_populations[["Black Population (15 - 64)"]] <- format(get_southern_populations[["Black Population (15 - 64)"]], big.mark = ",")
get_southern_populations[["White Population (15 - 64)"]] <- format(get_southern_populations[["White Population (15 - 64)"]], big.mark = ",")
