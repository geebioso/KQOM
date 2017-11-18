library(tidyverse)
# https://github.com/fawda123/rStrava
library(rStrava)
library(yaml)
library(feather)
library(gepaf)
library(sqldf)
library(XLConnect)
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
# SET UP DATABASE 
########################

# create database 
db <- dbConnect(SQLite(), dbname="chicago_strava.sqlite")

dbSendQuery(conn = db,
            "CREATE TABLE IF NOT EXISTS points
            (segment_id INTEGER,
            lat NUMERIC,
            lon NUMERIC
            )"
)

dbSendQuery(conn = db,
            "CREATE TABLE IF NOT EXISTS all_segments
            (id INTEGER,
            st_lat NUMERIC,
            st_lon NUMERIC, 
            end_lat NUMERIC, 
            end_lon NUMERIC, 
            numleads INTEGER, 
            numentries INTEGER, 
            activity_id INTEGER,
            south_west_box_id INTEGER 
            )"
)

## DROP TABLES if we screwed something up and restart idx
# dbSendQuery(conn = db, "DROP TABLE all_segments")
# dbSendQuery(conn = db, "DROP TABLE points")
# startidx = 1
# write.table( startidx, 'data/last_south_west_box_idx.txt', sep = "\t")


########################
# MAIN FUNCTION 
########################
# function to get rare chicago segments 
get_rare_chicago_segments <- function( boxsizes = c(0.5, 0.75, 1, 1.5, 2, 3, 4), sleep = 0, start_dist = 1/70, num_leaders = 10){
  
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
  NTOT = NB*NWS*NSS # total number of boxes
  
  print( paste( NSS*NWS, "starts,", NB, "boxes  => ", NTOT, "total boxes"))
  
  count = 0 # track number of segments we have stored 
  api_count = 0 # track number of segments we have recently queried from API 
  api_tot = 0 # track total number of requests
  all_segments = data_frame() # stores all segments across all queries 
  all_points = data_frame()       # stores the lat/lon pairs from the all queries
  assign('points',
         points,
         envir = .GlobalEnv)
  
  
  # get all pairs of south starts, west starts, and box sizes 
  south_west_box_tupes = expand.grid(south_starts, west_starts, boxsizes)
  
  startidx = read.table('data/last_south_west_box_idx.txt', sep = "\t")
  startidx = startidx$x
  
  # Iterate over starting locations and box sizes
  for (n in c(startidx: NTOT)){
    tup = south_west_box_tupes[n,]
    south_lat = tup[1]
    west_lat = tup[2]
    boxsize = tup[3]
    
    # print(paste("boxsize =", toString(boxsize)))
    dlat = boxsize*1/70 # 1/70 of a degree is approximately one mile 
    dlon = boxsize*1/70
    
    bnds = c(south_lat, west_lon, south_lat + dlat, west_lon + dlon)
    
    # get chicago segments 
    segments = get_explore(stoken, toString(bnds), activity_type = "riding", max_cat = NULL, min_cat = NULL)
    NS = length(segments$segments)
    message(paste0(Sys.time(), ': ', n, ' of ', NTOT, ' boxes, api_count = ', api_count, ', segments returned = ', NS))
    api_count = api_count + 1
    
    # check to see if we need to limit requests
    if (api_count > 550){
      message( paste0('api count = ', api_count, ' sleeping for ', sleep , ' seconds'))
      Sys.sleep(sleep)
      api_tot = api_tot + api_count
      api_count = 0 
    }
    
    
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
        api_count = api_count + 2
        
        # check to see if we need to limit requests
        if (api_count > 550){
          message( paste0('api count = ', api_count, ' sleeping for ', sleep , ' seconds'))
          Sys.sleep(sleep)
          api_tot = api_tot + api_count
          api_count = 0 
        }
        
        #cat('\t')
        #print(paste('id=', id[i], ': num lead=', length(lead$entries), ', num effort =', length(efforts)  ))
        
        numleads[i] = length(lead$entries)
        numentries[i] = length(efforts)
        
        activity_id[i] = lead$entries[[1]]$activity_id
        if( length(lead$entries) < num_leaders){
          tokeep[i] = TRUE
        }
      }
      
      short_segments$numleads = numleads
      short_segments$numentries = numentries 
      short_segments$activity_id = activity_id
      short_segments$south_west_box_id = rep(n, length(activity_id))
      
      short_segments = short_segments[tokeep,]
      id = id[tokeep]
      segments$segments = segments$segments[tokeep]
      
      NS = dim(short_segments)[1]
      # get polyline 
      if (NS > 0){
        points = data_frame() # points data frame for this request
        count = count + NS
        message( paste0('\tSegments kept = ', NS, ', total kept = ', count))
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
        }
        dbWriteTable(db, "all_segments", short_segments, append = TRUE )
        dbWriteTable(db, "points", points, append = TRUE )
        write.table( n, 'data/last_south_west_box_idx.txt', sep = "\t")
      } 
      
      # put everything together 
      all_segments = rbind( all_segments, short_segments)
      all_points = rbind(all_points, points)
    }
  }
  
  
  
  # all_segments = unique(all_segments)
  # rownames(all_segments) = all_segments[, "id"]
  # write_feather(all_segments, paste0('data/all_segments.feather'))
  # write_feather(points, paste0('data/points.feather'))
  # write.table(count, 'data/count.txt', sep="\t")
  # write.table(api_count, 'data/api_count.txt', sep="\t")
  
  out = list()
  out$all_segments = all_segments
  out$points = points
  out$count = count
  out$api_count = api_count
  
  return( out )
}

########################
# RUN EVERYTHING 
########################

starttime = Sys.time() 
start_dist = 1/70 # 70 # 1/70
sleep = 1200 # sleep for 20 minutes if we go above the API request count. Should be conservative
boxsizes = c(0.5, 0.75, 1, 1.5, 2, 3, 4) # c(0.5, 0.75) 
num_leaders = 10 # keep any routes that have 10 or less leaders
out = get_rare_chicago_segments(boxsizes = boxsizes, sleep = sleep, start_dist, num_leaders = num_leaders) # 8 seconds for one iteration of default boxsizes
endtime = Sys.time()

print( endtime - starttime)

# We make ~ 600 queries every 6 seconds. Changed the code to sleep for 20 minutes if number of API queries is above 550
