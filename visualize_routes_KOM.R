# VISUALIZE OUR ROUTES  
### Plot Our Segments 

library(leaflet)
library(htmlwidgets)

setwd('~/git_repos/KQOM')

########################
# UTILITY FUNCTIONS
########################

# load data 
load_segments_and_points <- function( ){
  
  db <- dbConnect(SQLite(), dbname="chicago_strava.sqlite")
  count        = read.table('data/count.txt', sep="\t")
  count        = count$x
  
  api_count        = read.table('data/api_count.txt', sep="\t")
  api_count        = api_count$x
  
  all_segments = dbReadTable(db, 'all_segments')
  all_segments$activity_id = NULL
  all_segments$south_west_box_id = NULL 
  
  points = dbReadTable(db, 'points')
  points = unique(points)
  
  city = rep("Chicago", dim(points)[1])
  points$city = city
  
  
  out = list()
  out$all_segments = as.data.frame(all_segments)
  out$points = points
  out$count = count 
  out$api_count = api_count
  
  dbDisconnect(db)
  
  return(out)
}

########################
# PLOTTING FUNCTION 
########################
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


########################
# PLOT THE DATA 
########################

out2 = load_segments_and_points()
message( paste0( 'Number of Segments = ', out2$count))
chicago <- leaf_country(out2$points, location = 'Chicago', city = TRUE)
saveWidget(chicago, paste0(getwd(), '/widgets/Chicago.html'),
           selfcontained = TRUE)

