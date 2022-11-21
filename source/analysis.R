library(tidyverse)
library(plotly)
library(maps) # Used in section 6 to match county names for mapping

# The functions might be useful for A4
source("../source/a4-helpers.R")

incarceration_df <- get_data()

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Calculations for the three values in section 2 plus some table code to help
# explain why some results are what they are
#----------------------------------------------------------------------------#
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

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Functions to produce the graph shown in section 3 of index.Rmd
#----------------------------------------------------------------------------#

# This function will return a data frame that will be visualized in the below
# function plot_jail_pop_for_us()
get_year_jail_pop <- function() {
  year_jail_pop <- incarceration_df %>% 
    # Group by each year
    group_by(year) %>%
    # Get the sum of all the jail populations across each state (for a year)
    summarize(total_jail_pop_this_year = sum(total_jail_pop, na.rm = TRUE)) %>% 
    # Select only the year (x variable) and the total jail population (y variable)
    select(year, total_jail_pop_this_year) %>% 
    rename("Year" = year, "Total U.S. Jail Population" = total_jail_pop_this_year)
  
  year_jail_pop[["Total U.S. Jail Population"]] <-
    trunc(year_jail_pop[["Total U.S. Jail Population"]])
  
  return(year_jail_pop)   
} 

# This function will graph the data (year, total jail population) which is 
# calculated in the get_year_jail_pop() function above.
plot_jail_pop_for_us <- function()  {
  # Create a ggplot with the function above as the dataset and x and y variables
  # being the year and total jail population
  plot <- ggplot(get_year_jail_pop(), aes(x = Year, y = `Total U.S. Jail Population`)) +
    # Make it a bar graph (label used for plotly hover info)
    geom_col() +
    # Scale the y so that the labels show comma notation instead of scientific
    scale_y_continuous(labels = scales::comma) +
    # Label the graph with axis titles, a graph title, and a source caption
    labs(
    x = "Year",
    y = "Total Jail Population",
    title = "Increase of Jail Population in U.S. (1970 - 2018)",
    caption = "Source: Vera Institute"
  ) +
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      axis.title.x = element_text(size = 16, face = "bold", vjust = -2),
      axis.title.y = element_text(size = 16, face = "bold", vjust = 4),
    )
  plot <- ggplotly(plot)
  return(plot)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Functions to produce line graph of jail population in different states
#----------------------------------------------------------------------------#

# Function that retrieves jail populations for the given states in the states
# vector. 
get_jail_pop_by_states <- function(states) {
  # Initial check to make sure the required number of states for analysis is in range
  if (length(states) > 10 || length(states) < 3) {
    print("Error: State vector must have between 3 and 10 states")
    return()
  } else {
    # If number of states good, then filter by those states and sum the jail pop.
    state_jail_pop <- incarceration_df %>% 
      filter(state %in% states) %>% 
      group_by(state, year) %>% 
      summarize(total_jail_pop_state = sum(total_jail_pop, na.rm = TRUE), .groups = "keep")
    return(state_jail_pop)
  }
}

# Function that plots line graphs for the given states in the states vector
plot_jail_pop_by_states <- function(states) {
  jail_pop <- get_jail_pop_by_states(states)
  if (is.data.frame(jail_pop)) {
    plot <- ggplot(
      data = jail_pop, 
      aes(
        x = year, 
        y = total_jail_pop_state, 
        group = jail_pop$state, 
        color = jail_pop$state)) +
      geom_line(linewidth = 1.5) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        x = "Year",
        y = "Total Jail Population",
        title = "Jail Population of Selected States (1970 - 2018)",
        caption = "Source: Vera Institute",
        color = "States"
      ) +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        plot.caption = element_text(size = 14),
        legend.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold", vjust = -2),
        axis.title.y = element_text(size = 20, face = "bold", vjust = 2),
        axis.text.y = element_text(size = 10)
      )
    return(plot)
  }
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Create a stacked bar chart showing per division the percentage of each race
# in prison
#----------------------------------------------------------------------------#

# Get the total black jail population in each division
get_per_division <- incarceration_df %>% 
  group_by(division) %>% 
  filter(year == 2018) %>% 
  summarize(across(c(black_jail_pop, black_pop_15to64, white_jail_pop, white_pop_15to64), sum, na.rm = TRUE)) %>% 
  mutate(black_prop_in_jail = black_jail_pop / black_pop_15to64 * 100) %>% 
  mutate(white_prop_in_jail = white_jail_pop / white_pop_15to64 * 100) %>% 
  select(division, 
         black_pop_15to64, 
         white_pop_15to64,
         black_prop_in_jail,
         white_prop_in_jail) 

# Below code formats the results usch as adding commas and rounding percents
get_per_division$black_pop_15to64 <- 
  format(get_per_division$black_pop_15to64, big.mark = ",")

get_per_division$white_pop_15to64 <- 
  format(get_per_division$white_pop_15to64, big.mark = ",")

get_per_division$black_prop_in_jail <- 
  round(get_per_division$black_prop_in_jail, digits = 2)

get_per_division$white_prop_in_jail <- 
  round(get_per_division$white_prop_in_jail, digits = 2)

# Function to create a scatterplot for each division showing the respective
# black/white proportions of each population that are in jail
create_scatterplot_graph <- function() {
  # options(repr.plot.width = 9, repr.plot.height = 9)
  plot <- ggplot(
    data = get_per_division, 
    aes(x = white_prop_in_jail, 
        y = black_prop_in_jail,
        color = division,
        text = paste("White Percentage:", white_prop_in_jail, "%",
                     "\nBlack Percentage:", black_prop_in_jail, "%",
                     "\nDivision:", division))) +
    geom_point(
      data = get_per_division,
      size = 5,
    ) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = "Percentage of Black/White Populations in Jail in Each U.S. Division; 2018",
      caption = "Source: Vera Institute",
      x = "Percentage of White People in Jail",
      y = "Percentage of Black People in Jail",
      color = "Division"
    ) + 
    theme(
      plot.title = element_text(size = 14, face = "bold", vjust = 3),
      plot.caption = element_text(size = 12),
      legend.title=element_text(size = 16, face = "bold"), 
      legend.text=element_text(size = 14),
      legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
      axis.title.x = element_text(size = 14, face = "bold", vjust = -2),
      axis.title.y = element_text(size = 14, face = "bold", vjust = 5),
      axis.text.x = element_text(size = 15.5),
      axis.text.y = element_text(size = 18),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    ) 
  plot <- ggplotly(plot, tooltip = c("text"))
  return(plot)
}

# Create a table of each race's population in each division to show below
# the above graph
get_pop_race <- get_per_division %>% 
  select(division, black_pop_15to64, white_pop_15to64) %>% 
  rename("Black Population (15 - 64)" = "black_pop_15to64") %>% 
  rename("White Population (15 - 64)" = "white_pop_15to64")

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Section 6 contains the code used to produce the map to address inequalities
# between white and black jail populations
#----------------------------------------------------------------------------#

# The county names in the incarceration dataset are not formatted the same as
# the ones that map_data("county") uses, so I will use a county dataset provided
# by maps package and then later join this with the incarceration dataset which
# makes it easier to map
county_lookup <- tibble(county.fips) %>% 
  # I will later join the map_data("county") by its fips code, but I must separate
  # the state and county into their own columns using tidyr
  separate(polyname, into = c("State", "county"), sep = ",")

# get_data used to wrangle our dataset for graphing below
get_data <- function() {
  data <- incarceration_df %>% 
    filter(year == 2018) %>%  # Only the most recent year allowed
    filter(region == "South") %>%  # Only southern United States
    # Have to join on county_lookup to make county name the same as county map data
    inner_join(county_lookup, by = c("fips" = "fips")) %>%
    select(fips, county, State, region, black_jail_pop, white_jail_pop) %>% 
    mutate(black_jail_pop = ifelse(is.na(black_jail_pop), 0, black_jail_pop)) %>% 
    mutate(white_jail_pop = ifelse(is.na(white_jail_pop), 0, white_jail_pop)) %>% 
    mutate(black_to_white_ratio = black_jail_pop / white_jail_pop) %>%  # Ratio calculation
    filter(is.nan(black_to_white_ratio) == FALSE) # Remove missing calculations
  return(data)
}

# Function used to create the map to show an inequality
create_inequality_map <- function() {
  
  # Get the county map_data only including counties from regions we want
  county_shape <- map_data("county") %>% 
    rename("county" = "subregion") %>% 
    inner_join(get_data(), by = c("county" = "county", "region" = "State"))
  
  # Get the state map_data so that we can create the state outlines but filter
  # it so that it only contains state outlines from the desired region
  state_shape <- map_data("state") %>%
    inner_join(get_data(), by = c("region" = "State"))
  
  plot <- ggplot(county_shape) +
    geom_polygon( # Polygons that represent each county
      mapping = aes(x = long, y = lat, group = group, fill = black_to_white_ratio),
      color = "black",
      linewidth = 0.3
    ) +
    geom_polygon( # Polygons used to outline the borders of each state (no fill)
      data = state_shape,
      mapping = aes(x = long, y = lat, group = group),
      fill = NA,
      color = "black",
      linewidth = 1
    ) +  
    scale_fill_gradientn( # Scaling the fill with red hue instead of blue
      colors = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59", "#EF6548",
                 "#D7301F", "#B30000", "#7F0000"),
      # Rescale the colors so it more closely matches the calculated results
      values = scales::rescale(c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 
                                 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 16, 18, 20,
                                 30, 40, 50, 60, 70, 80, 90, 100))
    ) +
    coord_quickmap() +
    labs(
      title = "2018, Black to White Jail Population Ratio Per County; Southern U.S.",
      subtitle = "Calculated by taking black jail population and dividing it by white jail population for each county",
      fill = "Black to White Ratio",
      caption = "Source: Vera Institute"
    ) +
    theme_void() + # Removes all axis, backgrounds, tick marks, and grid lines.
    theme(
      plot.title = element_text(size = 36, face = "bold"),
      plot.subtitle = element_text(size = 24),
      plot.caption = element_text(size = 24),
      legend.title=element_text(size = 24), 
      legend.text=element_text(size = 18),
      legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"), # Add a margin around it
      legend.background = element_rect(fill = "#C6C6C6") # Set the background color for the legend
    ) +
    guides(fill = guide_colorbar(barheight = 20)) # Make the gradient bar in the legend longer
  return(plot)
}

# For section 6 table (list populations from select population)
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

