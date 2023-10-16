library(tidyverse)
library(glue)
library(purrr)

vol_qry <- function(id, from, to){
  # Use glue package to get identifier values id, from and to from traffic data
  query_gql <- glue(
    '{
      trafficData(trafficRegistrationPointId: "[id]") {
        volume {
          byHour(from: "[from]", to: "[to]") {
            edges {
              node {
                from
                to
                total {
                  volumeNumbers {
                    volume
                  }
                }
              }
            }
          }
        }
      }
    }',
    # specify delimiters for where values occur
    .open = "[", .close = "]"
  )
  return(query_gql)
}

GQL(
  vol_qry(
    id=stations_metadata_df$id[1], 
    from=to_iso8601(stations_metadata_df$latestData[1],-4),
    to=to_iso8601(stations_metadata_df$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)

# looks alright