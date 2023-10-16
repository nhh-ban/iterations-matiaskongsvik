library(tidyverse)

setwd("/Users/matiaskongsvik/iterations-matiaskongsvik")

# Create function which passes one argument
transform_metadata_to_df <- function(df) {
  # select content of list 1.
  df[[1]] %>%
    #  Traverse all the entries in list and transform sub-lists to a data frame.
    map(as_tibble) %>%
    #  Bind all list elements and entries in sub-lists by row
    list_rbind() %>%
    # Create new columns in data frame based on entries in sub-lists. Using map functions specifying data type, use targeted column name,
    # first entry in list and NA-value if error is returned.
    mutate(
      latestData = map_chr(latestData, 1, .default = NA_character_)
    ) %>%
    # specidy datatime and timezone
    mutate(
      latestData = as_datetime(latestData, tz = "UTC")
    ) %>%
    # unnest column location to sublists instead
    unnest_wider(location) %>% 
    unnest_wider(latLon)
}



to_iso8601 <- function(variable, offset = 0) {
  # Set offset argument as days using function from lubridate.
  variable <- as_datetime(variable)
  offset <- days(offset)

  
  # use function from lubridate to format variable input to iso8601 and add day offset. Add chr "Z" at the end of datatime output.
  formatted_to_iso_8601 <- format_ISO8601(variable + offset, "Z", sep ="" )
  
  return(formatted_to_iso_8601)
}


transform_volumes <- function(data) {
# unsure how to solve. Need to gather data from edges as seen in query_gql in vol_qry script.
# Should store data from edges in a data-frame with columns to, from and volume.
# This should in theory enable Final volume query in iteration script to show the plot for volume calls.
# After that its just a matter of beautifying the plot as I´ve demonstrated in previous assignments.
}

