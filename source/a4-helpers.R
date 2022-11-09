library(tidyverse)

# Data access ----
#---------------------------------------------------------------------------#
# This function returns the incarceration data as a dataframe
# Note: The CSV file is stored on my local machine to speed load times
#---------------------------------------------------------------------------#
get_data <- function(num_records=-1) {
  fname <- "~/Documents/info201/data/incarceration_trends.csv"
  df <- read.csv(fname, nrows=num_records)
  return(df)
}

# Processing places ----
# NOTE: For these functions to work, the dataframe `incarceration_df` must 
#       be initialized
#----------------------------------------------------------------------------#
# Return the list of states in a region.  The regions are: 
#    Midwest, Northeast, South, West
#----------------------------------------------------------------------------#
states_in_region <- function(p_region) {
  the_states <- incarceration_df %>%
    filter(region == p_region) %>%
    distinct(state) %>%
    pull(state)
  return(the_states)
}

#----------------------------------------------------------------------------#
# Return the list of divisions in a region. The regions are: 
# Midwest, Northeast, South, West
#----------------------------------------------------------------------------#
divisions_in_region <- function(p_region) {
  the_divisions <- incarceration_df %>%
    filter(region == p_region) %>%
    distinct(division) %>%
    pull(division)
  return(the_divisions)
}

#----------------------------------------------------------------------------#
# Return the list of states in a region.  The divisions are: 
#    East North Central
#    East South Central
#    Middle Atlantic
#    Mountain
#    New England
#    Pacific
#    South Atlantic
#    West North Central
#    West South Central
#----------------------------------------------------------------------------#
states_in_division <- function(p_division) {
  the_states <- incarceration_df %>%
    filter(division == p_division) %>%
    distinct(state) %>%
    pull(state)
  return(the_states)
}

#----------------------------------------------------------------------------#
# Returns TRUE if the place is a state 
#----------------------------------------------------------------------------#
is_state <- function(p_place) {
  the_states <- incarceration_df %>%
    filter(state == p_place) %>%
    distinct(state) %>%
    pull(state)
  if (length(the_states > 0)) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

#----------------------------------------------------------------------------#
# Returns a list of states for region or division. If place is a state
# just return it.
#----------------------------------------------------------------------------#
states_in_region_or_division <- function(place) {
  states <- c()
  if (place %in% c("Midwest", "Northeast", "South", "West")) {
    states <- states_in_region(place)
  } else {
    states <- states_in_division(place)
    if (length(states)==0) {
      if(is_state(place)) {
        return(c(place))
      }
      else {
        stop(paste0("Invalid place name (\"",place, "\")"))
      }
    }
  }
  return(states)
}

# Useful queries ----
#----------------------------------------------------------------------------#
# Do some states have no jail populations? If so, which states?
#----------------------------------------------------------------------------#
states_with_no_jail_pop <- function(df) {
  t <- incarceration_df %>%
    group_by(state) %>%
    summarise(p = sum(total_jail_pop, na.rm = TRUE)) %>%
    filter(p == 0) %>%
    select(state, p) %>%
    pull(state)
  return(t)
}

# Basic info ----
#----------------------------------------------------------------------------#
# Format some region information (currently, only the divisions)
#----------------------------------------------------------------------------#
format_region_info <- function(region) {
  t <- ""
  t <- paste0(t, "  in '", region, "': ")
  d <- divisions_in_region(region)
  t_divisions <-  paste0(d, collapse = " | ")
  t <- paste0(t, t_divisions)
  return(t)
}

#----------------------------------------------------------------------------#
# Show some basic information about the dataset
#----------------------------------------------------------------------------#
get_basic_info <- function(df) {
  t <- "Basic Info on `incarceration_trends`\n"
  t <- paste0(t, "  No. regions               ", length(unique(df$region)), "\n")
  t <- paste0(t, "  No. divisions             ", length(unique(df$division)), "\n")
  t <- paste0(t, "  No. states                ", length(unique(df$state)), "\n")
  t <- paste0(t, "  No. counties              ", length(unique(df$county_name)), "\n")
  t <- paste0(t, "  No. fips (county IDs)     ", length(unique(df$fips)), "\n")
  t <- paste0(t, "  No. different urbanicity  ", length(unique(df$urbanicity)), "\n")
  
  t <- paste0(t, "Divisions ...\n")
  t <- paste0(t, format_region_info("Midwest"), "\n")
  t <- paste0(t, format_region_info("Northeast"), "\n")
  t <- paste0(t, format_region_info("South"), "\n")
  t <- paste0(t, format_region_info("West"), "\n")
  
  cat(t) 
}

# Main ----
#----------------------------------------------------------------------------#
# Basic tests of the helper functions
# Comment or uncomment 
#----------------------------------------------------------------------------#
# ## Very important: You must initialize `incarceration_df`
# incarceration_df <- get_data()
# 
# ## Demonstrate use of the functions
# ## Each of these functions returns a vector of states
# states_in_region("South")
# states_in_division("Pacific")
# states_in_region_or_division("South")        
# states_in_region_or_division("Mountain")  
# states_in_region_or_division("OR")          # returns c("OR")
# 
# ## Returns the divisions that makeup a region 
# divisions_in_region("West")
# 
# ## Returns basic information about the dataset
# get_basic_info(incarceration_df)
# 
# ## 
# states_with_no_jail_pop()
