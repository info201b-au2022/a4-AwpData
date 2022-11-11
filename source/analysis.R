library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

incarceration_df <- get_data()

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

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
    select(year, total_jail_pop_this_year)
  return(year_jail_pop)   
} 

# This function will graph the data (year, total jail population) which is 
# calculated in the get_year_jail_pop() function above.
plot_jail_pop_for_us <- function()  {
  # Create a ggplot with the function above as the dataset and x and y variables
  # being the year and total jail population
  plot <- ggplot(get_year_jail_pop(), aes(x = year, y = total_jail_pop_this_year)) +
    # Make it a bar graph
    geom_col() +
    # Scale the y so that the labels show comma notation instead of scientific
    scale_y_continuous(labels = scales::comma) +
    # Label the graph with axis titles, a graph title, and a source caption
    labs(
    x = "Year",
    y = "Total Jail Population",
    title = "Increase of Jail Population in U.S. (1970 - 1978)",
    caption = "Source: Vera Institute"
  )
  return(plot)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_jail_pop_by_states <- function(states) {
  if (length(states) > 10 || length(states) < 3) {
    print("Error: State vector must have between 3 and 10 states")
    return()
  } else {
    state_jail_pop <- incarceration_df %>% 
      filter(state %in% states) %>% 
      group_by(state, year) %>% 
      summarize(total_jail_pop_state = sum(total_jail_pop, na.rm = TRUE), .groups = "keep")
    return(state_jail_pop)
  }
}

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
      )
    
    return(plot)
  }
}


## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


