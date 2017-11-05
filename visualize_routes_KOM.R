# VISUALIZE OUR ROUTES  
### Plot Our Segments 

# Define function for leaflet plot of only a country
leaf_country <- function(df, location,
                          limit = NULL,
                          city = FALSE){ # or NULL

  if(city){
    sub_data <- df %>%
      filter(city == location)
  } else {
    sub_data <- df %>%
      filter(country == location)
  }
  
  
  l <- leaflet() %>%
    addProviderTiles(provider = 'CartoDB.DarkMatter',
                     options = providerTileOptions(opacity = 0.95))
  captured_activities <- sub_data %>%
    group_by(segment_id) %>%
    tally %>%
    .$segment_id
  if(!is.null(limit)){
    if(limit < length(captured_activities)){
      captured_activities <- sample(captured_activities,
                                    limit)
    }
  }
  the_alpha <- ifelse(length(captured_activities) > 50,
                      0.4,
                      ifelse(length(captured_activities) > 100,
                             0.3,
                             0.5))
  for (i in 1:length(captured_activities)){
    message(i)
    this_segment_id <- captured_activities[i]
    sub_sub_data <- sub_data %>%
      filter(segment_id == this_segment_id)
    l <- l %>%
      addPolylines(data = sub_sub_data, lng = ~lon, lat = ~lat,
                   color = 'red',
                   opacity = 1 - the_alpha,
                   weight = 1)
  }
  return(l)
}

chicago <- leaf_country(out$points, location = 'Chicago', city = TRUE)
saveWidget(chicago, paste0(getwd(), '/widgets/Chicago.html'),
           selfcontained = TRUE)

