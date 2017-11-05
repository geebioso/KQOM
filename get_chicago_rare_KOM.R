library(tidyverse)
# https://github.com/fawda123/rStrava
library(rStrava)
library(yaml)
library(feather)
library(gepaf)
options(scipen = '999')


########################

# This script will query strava for bike segments in chicago as a function of bounding box. It iterates over bounding boxes to search for rare segments (strava returns segments withing a box according to popularity so we have to do a fine-grained search to find rare routes) 

# Data is saved in the following files: 
#   data/backups/points[system time].feather: contains the lat/lon pairs for all points in all segments 
#   data/backups/[system time].feather: contains the segment ids and metadata for all segments 

# Chicago bounding box: 42.023131 South Latitude: 41.644335 East Longitude: -87.523661 West Longitude: -87.940267
# box computed at https://www.mapdevelopers.com/geocode_bounding_box.php

########################
# UTILTIY FUNCTIONS 
########################

# get poly line 
polyate <- function(points_now){
  decoded_polyline <- gepaf::decodePolyline(points_now)
  return(decoded_polyline)
}
########################
# CREDENTIALS 
########################

setwd('~/git_repos/KQOM')

# Get access / authentication token for Strava's API
credentials <- yaml::yaml.load_file(input = 'credentials/credentials.yaml')

# create the authentication token
# (on first run, will open browswer - thereafter, will rely on saved file)
setwd('credentials')
if(!".httr-oauth" %in% dir(all.files = TRUE)){
  stoken <- httr::config(token = strava_oauth(app_name = credentials$category, 
                                              app_client_id = credentials$client_id, 
                                              app_secret = credentials$client_secret,
                                              cache = TRUE))
} else {
  stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])
}
setwd('..')

# For using google's API (elevation, etc.), get key
# save the key, do only once
# only need to run the below once:
# cat(paste0("google_key=", 
#            credentials$google_key,
#            "\n"),
#     file=file.path(normalizePath("~/"), ".Renviron"),
#     append=TRUE)
mykey <- Sys.getenv("google_key")

########################
# MAIN FUNCTION 
########################
# function to get chicago segments 
get_rare_chicago_segments <- function( boxsizes = c(0.5, 0.75, 1, 1.5, 2, 3, 4), sleep = 0, start_dist = 1/70){
  
  chi_bnds  = c(41.644335, -87.940267, 42.023131, -87.523661) # bounds for all of chicago (S, W, N, E)
  south_lat = chi_bnds[1]
  west_lon = chi_bnds[2]
  north_lat = chi_bnds[3]
  east_lon = chi_bnds[4]
  
  # get all the starting points for south latitude and west longitude 
  south_starts = seq(from = south_lat, to = north_lat, by = (north_lat - south_lat)*start_dist)
  west_starts = seq(from = west_lon, to = east_lon, by = (east_lon - west_lon)*start_dist)
  
  # Constants 
  NB = length(boxsizes)
  NSS = length(south_starts)
  NWS = length(west_starts)
  
  # constants to keep track of how many boxes we have gone through 
  Nnow = 1 # which box we are on now
  NTOT = NB*NWS*NSS # total number of boxes
  
  print( paste( NSS*NWS, "starts,", NB, "boxes  => ", NTOT, "total boxes"))
  
  count = 0 # track number of segments we have queried from API 
  all_segments = data_frame() # stores all segments across all queries 
  points = data_frame()       # stores the lat/lon pairs from the all queries
  assign('points',
         points,
         envir = .GlobalEnv)
  
  # Iterate over starting locations and box sizes   
  for ( south_lat in south_starts ){
    for (west_lon in west_starts){
      
      for (boxsize in boxsizes){
        message(paste0(Nnow, ' of ', NTOT, ' boxes '))
        Nnow = Nnow + 1
        # print(paste("boxsize =", toString(boxsize)))
        dlat = boxsize*1/70 # 1/70 of a degree is approximately one mile 
        dlon = boxsize*1/70
        
        bnds = c(south_lat, west_lon, south_lat + dlat, west_lon + dlon)
        
        # get chicago segments 
        segments = get_explore(stoken, toString(bnds), activity_type = "riding", max_cat = NULL, min_cat = NULL)
        
        NS = length(segments$segments)
        # get id, lat, and long and put in a data frame 
        if (NS > 0){
          short_segments = data.frame(t(sapply( segments$segments, function(x) c( x$id, x$start_latlng[1],
                                                                                  x$start_latlng[2], x$end_latlng[1], x$end_latlng[2]))))
          
          colnames(short_segments) = c('id', 'st_lat', 'st_lon', 'end_lat', 'end_lon')
          
          short_segments = transform( short_segments,
                                      id = as.numeric(id), 
                                      st_lat = as.numeric(st_lat),
                                      st_lon = as.numeric(st_lon),
                                      end_lat = as.numeric(end_lat),
                                      end_lon = as.numeric(end_lon))
          
          # check popularity of each segment 
          id = short_segments$id
          NI = length(id) # number of segments returned 
          tokeep = rep(FALSE, NI)
          numleads = array(0,NI) 
          numentries = array(0, NI)
          activity_id = array(0, NI)
          for(i in 1:NI){
            # number of leaders 
            lead = get_segment(stoken, id = id[i], request = 'leaderboard')
            # number of efforts (multiple efforts can be made by each leader)
            efforts = get_segment(stoken, id = id[i], request = 'all_efforts')
            #cat('\t')
            #print(paste('id=', id[i], ': num lead=', length(lead$entries), ', num effort =', length(efforts)  ))
            numleads[i] = length(lead$entries)
            numentries[i] = length(efforts)
            activity_id[i] = lead$entries[[1]]$activity_id
            if( length(lead$entries) < 6){
              tokeep[i] = TRUE
            }
          }
          
          short_segments$numleads = numleads
          short_segments$numentries = numentries 
          short_segments$activity_id = activity_id
          
          short_segments = short_segments[tokeep,]
          id = id[tokeep]
          segments$segments = segments$segments[tokeep]
          
          NS = dim(short_segments)[1]
          # get polyline 
          if (NS > 1){
            for (i in 1:NS){
              # message(paste0(i, ' of ', NS))
              segmentnow = segments$segments[[i]]
              left <- data.frame(segment_id = segmentnow$id)
              tried <- try({
                right <- polyate(points_now = segmentnow$points) %>%
                  mutate(segment_id = segmentnow$id)
              })
              if(class(tried) == 'try-error'){
                right <- data.frame(lat = NA,
                                    lon = NA,
                                    segment_id = segmentnow$id)
                if(grepl('429', tried[1])){
                  # Sleep 15 minutes then break
                  message('Sleeping for 1 minute beginning at ',
                          Sys.time())
                  Sys.sleep(60)
                  # break
                }
              } 
              # out_list[[i]] <- out
              
              # done gets a list of all the lat lon pairs in a segment
              done <- left_join(x = left,
                                y = right,
                                by = 'segment_id')
              
              points = rbind(points, done )
              # Every 5000, save a new backup
              if(length(unique(points$segment_id)) %% 5000 == 0){
                write_feather(points, paste0('data/backups/points', Sys.time(), '.feather'))
              }
              Sys.sleep(sleep)
            }
          } 
          all_segments = rbind( all_segments, short_segments)
          
          if( count %% 5000 == 0 ){
            write_feather(all_segments, paste0('data/backups/', Sys.time(), '.feather'))
          }
          count = count + NS
        }
      }
    }
  }
  
  
  
  all_segments = unique(all_segments)
  city = rep("Chicago", dim(points)[1])
  points$city = location
  rownames(all_segments) = all_segments[, "id"]
  write_feather(all_segments, paste0('data/all_segments.feather'))
  write_feather(done, paste0('data/points.feather'))
  
  out = list()
  out$all_segments = all_segments
  out$points = points
  out$count = count
  
  return( out )
  
}

########################
# RUN EVERYTHING 
########################

starttime = Sys.time() 
start_dist = 1/2
boxsizes = c(0.5, 0.75) # c(0.5, 0.75, 1, 1.5, 2, 3, 4)
out = get_rare_chicago_segments(boxsizes = boxsizes, sleep = 0, start_dist) # 8 seconds for one iteration of default boxsizes
endtime = Sys.time()

print( endtime - starttime)
