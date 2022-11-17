library("tidyverse")

# Section 2, Value 1  : Which state has highest average number of black jail population as of 2018?
highest_avg_black_jail <- incarceration_df %>% 
  group_by(state) %>% 
  filter(year == 2018) %>% 
  summarize(avg_black_jail = sum(black_jail_pop, na.rm = TRUE) / n()) %>% 
  select(state, avg_black_jail) %>% 
  arrange(-avg_black_jail) %>% 
  head(1)

highest_avg_black_jail$avg_black_jail <- format(highest_avg_black_jail$avg_black_jail, 
                                                big.mark = ",")

# Supporting calculation to show highest average white jail population in 2018
highest_avg_white_jail <- incarceration_df %>% 
  group_by(state) %>% 
  filter(year == 2018) %>% 
  summarize(avg_white_jail = sum(white_jail_pop, na.rm = TRUE) / n()) %>% 
  select(state, avg_white_jail) %>% 
  arrange(-avg_white_jail) %>% 
  head(1)

highest_avg_white_jail$avg_white_jail <- format(highest_avg_white_jail$avg_white_jail,
                                                digits = 3)
  
# Supporting calculation to find the average white jail population in DC in 2018
DC_avg_white_jail <- incarceration_df %>% 
  filter(year == 2018) %>% 
  filter(state == "DC") %>% 
  summarize(avg_white_jail = sum(white_jail_pop, na.rm = TRUE) / n()) %>% 
  arrange(-avg_white_jail)

# Table created to showcase the total population, black population, and white population in D.C. in 2018
DC_information <- incarceration_df %>% 
  filter(year == 2018) %>% 
  filter(state == "DC") %>% 
  group_by(state) %>% 
  summarize(across(
    c(total_pop_15to64, black_pop_15to64, white_pop_15to64), 
    sum, 
    na.rm = TRUE)) %>% 
  select(state, total_pop_15to64, black_pop_15to64, white_pop_15to64) %>% 
  rename("State" = "state") %>% 
  rename("Total Population (15 - 64)" = "total_pop_15to64") %>% 
  rename("Black Population (15 - 64)" = "black_pop_15to64") %>% 
  rename("White Population (15 - 64)"  = "white_pop_15to64")

DC_information[["Total Population (15 - 64)"]] <- format(DC_information[["Total Population (15 - 64)"]], big.mark = ",")
DC_information[["Black Population (15 - 64)"]] <- format(DC_information[["Black Population (15 - 64)"]], big.mark = ",")
DC_information[["White Population (15 - 64)"]] <- format(DC_information[["White Population (15 - 64)"]], big.mark = ",")

# Section 2, Value 2: Which county has the highest percentage of population as prisoners?

highest_perc_prisoners <- incarceration_df %>% 
  filter(year == 2018) %>% 
  group_by(county_name, state) %>% 
  summarize(across(c(total_pop, total_jail_pop), sum, na.rm = TRUE), .groups = "keep") %>% 
  mutate(percentage_prisoners = total_jail_pop / total_pop * 100) %>% 
  select(county_name, state, total_pop, total_jail_pop, percentage_prisoners) %>% 
  arrange(-percentage_prisoners) %>% 
  head(1)

highest_perc_prisoners$total_pop <- format(highest_perc_prisoners$total_pop, big.mark = ",")
highest_perc_prisoners$percentage_prisoners <- format(highest_perc_prisoners$percentage_prisoners, digits = 3)
# Section 2, Value 3: What year(s) had the highest Asian American prison population and where?

year_highest_aapi_pop <- incarceration_df %>% 
  group_by(year, county_name, state) %>% 
  summarize(highest_asian_pop = max(aapi_jail_pop, na.rm = TRUE), .groups = "keep") %>% 
  filter(is.finite(highest_asian_pop) == TRUE) %>% 
  select(year, county_name, state, highest_asian_pop) %>% 
  arrange(-highest_asian_pop) %>% 
  head(1)


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
