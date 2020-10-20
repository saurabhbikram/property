library(leaflet)
library(tidyverse)
library(stringr)
library(ggplot2)
library(glue)
library(lubridate)
library(spatialrisk)
aws.signature::use_credentials(profile="sb")

## LOAD DATA ----
load_data <- function(local_file="SW11"){
  
  local_file <- paste0(toupper(local_file),".rds")
  file_exists <- file.exists(local_file)
  file_exists_s3 <- aws.s3::object_exists(paste0("preprocess/",local_file), "ukpd")
  
  if(file_exists) prices_data <- readRDS(local_file) else if (file_exists_s3) {
    prices_data <- aws.s3::s3readRDS(paste0("preprocess/",local_file), "ukpd")
    saveRDS(prices_data, local_file)
  } else {stop("Data file does not exist.")}
  
  bed_predictor <- readRDS("../models/bed_prediction/bed_predictor_prod.rds")
  source("../models/bed_prediction/bed_pred_fn.R")
  predict_beds <- function(data, model) {
    out_df <- data.frame()
    testdf <- make_model_df(data %>% filter(is.na(beds)))
    if(nrow(testdf) > 0) {
    featsdf <- create_feat_df(testdf)
    xmat <- featsdf[,bed_predictor[[1]]$feature_names] %>% as.matrix
    testdf$predicted_beds <- predict_ensemble(xmat, bed_predictor)
    out_df <- testdf %>% select(property_key, predicted_beds)
    }
    return(out_df)
  }
  apply_predicted_beds <- function(data, prediction) {
    data %>% 
      left_join(prediction %>% select(property_key, predicted_beds), by="property_key") %>% 
      mutate(
        beds = ifelse(is.na(beds), predicted_beds, beds),
        predicted_beds = !is.na(predicted_beds)
      )
  }
  # predict beds when not available
  bed_predictions <- predict_beds(prices_data, bed_predictor)
  prices_data2 <- apply_predicted_beds(prices_data, bed_predictions)
  return(prices_data2)
}

## PROPERTY MAP ----
get_local_properties <- function(prices_data2, POSTCODE, DIST) {
  post_lat_lon <- prices_data2 %>% filter(str_detect(toupper(postcode),toupper(POSTCODE))) %>% select(longitude,latitude) %>% na.omit %>% summarise_all(mean)  %>% data.frame()
  distinct_lats <- prices_data2 %>% select(longitude, latitude, property_key) %>% group_by(property_key) %>% summarise_all(mean) %>% data.frame()
  row.names(distinct_lats) <- distinct_lats$property_key
  distinct_lats$property_key <- NULL
  distances <- geosphere::distGeo(distinct_lats, post_lat_lon %>% unlist)
  distinct_lats_keep <- distinct_lats[distances<=DIST,]
  prop_in_radius <- data.frame(property_key=row.names(distinct_lats_keep)) %>% distinct()
}

apply_property_filter <- function(prices_data2, postcode, beds, dist, transdate, for_sale, map_bounds=NULL) {
  
  transdate <- as.Date(transdate)
  
  if(postcode!="")
    prop_in_radius <- get_local_properties(prices_data2, postcode, dist)
  else if(!is.null(map_bounds))
    prop_in_radius <- filter(prices_data2, between(latitude, map_bounds$south, map_bounds$north ) & between(longitude, map_bounds$west, map_bounds$east)) %>% select(property_key) %>% distinct()
  else return(data.frame())
  
  if(for_sale) {
    for_sale <- prices_data2 %>%
      semi_join(prop_in_radius, by="property_key") %>%
      filter(
        ( between(beds,!!beds[1],!!beds[2]) | is.na(beds)) & 
          status_clean == "For Sale" & 
          trans_date > transdate) %>%
      arrange(desc(trans_date)) %>% filter(!duplicated(property_key))
    
    # find properties which have been sold and join them with FS properties
    already_sold <- inner_join(prices_data2 %>%
                                 filter(status_clean=="Sold") %>%
                                 select(property_key, trans_date),
                               for_sale %>% select(property_key,fs_saledate=trans_date),
                               by="property_key") %>% distinct %>%
      # we have the most recent FS date of a property from `for_sale`
      # if this sale date is less than last sold date, we can remove this
      filter(trans_date > fs_saledate) %>% select(property_key) %>% distinct()
    
    house_data <- anti_join(for_sale, already_sold)
  } else {
    house_data <- prices_data2 %>%
      semi_join(prop_in_radius, by="property_key") %>% 
      filter((between(beds,!!beds[1],!!beds[2]) | is.na(beds)) & 
               status_clean == "Sold" & 
               trans_date > transdate) %>%
      arrange(desc(trans_date)) %>% filter(!duplicated(property_key))
  }
  
  prop_history <- prices_data2 %>% 
    filter(property_key %in% house_data$property_key & !is.na(price_num)) %>% 
    select(property_key, status_clean, price_num, trans_date) %>% 
    arrange(desc(trans_date)) %>% 
    distinct(property_key, status_clean,price_num,.keep_all = T)
  
  prop_history <- anti_join(prop_history, house_data, by=c("property_key","trans_date"))
  
  house_data <- left_join(house_data, 
                          prop_history %>% group_by(property_key) %>% arrange(desc(trans_date))  %>% 
                            summarise(sale_hist = paste0(glue("{status_clean}, {format(trans_date,'%b %Y')}, £{ifelse(price_num>10e3, paste0(round(price_num/1e3,0),'k'), price_num)}"),collapse = "</br>")
                            ),
                          by="property_key"
  ) %>% 
    mutate(
      listing_url_desc = ifelse(is.na(listing_url),"",glue("</br><a href = '{listing_url}' target='_blank'>Listing</a>")),
      property_key_url = ifelse(str_detect( property_key,"^l"),"",glue("</br><a href = 'https://www.zoopla.co.uk/property/{property_key}' target='_blank'>Property {property_key}</a>"))
    )
  
  return(house_data)
  
}
make_pretty_df <- function(house_data) {
  
  ldata <- house_data %>%
    filter(!is.na(longitude)) %>% 
    mutate(
      price_pretty=paste0(round(price_num/1000,0),"k"),
      bed_msg = ifelse(!is.na(beds),glue("{beds} beds"),""),
      bed_msg = ifelse(predicted_beds & !is.na(predicted_beds),paste0(bed_msg,"*"),bed_msg),
      area_msg = ifelse(!is.na(area),glue(" {area} sq ft"),""),
      sale_hist = ifelse(is.na(sale_hist),"",paste0("</br>",sale_hist)),
      msg = glue("{address}, {postcode}</br>{bed_msg}{area_msg}
             </br>{status_clean}, {format(trans_date,\"%b %Y\")}, £{price_pretty}
             {sale_hist}
             {listing_url_desc} {property_key_url}"),
      scale_clor = scale(log(price_num/area))[,1],
      scale_clor = scales::rescale(scale_clor),
      bg_color = scales::colour_ramp(c("blue","red"))(scale_clor)
    ) %>%
    arrange(price_num) %>% 
    group_by(paste0(longitude,latitude)) %>% 
    summarise(
      longitude = longitude[1],
      latitude = latitude[1],
      price_pretty = ifelse(length(price_pretty) > 2, paste0(price_pretty[1],"..",price_pretty[length(price_pretty)], glue(" ({length(price_pretty)})")) ,paste(price_pretty,collapse=", ")),
      msg = paste(msg,collapse="<p>-------<p>"), 
      bg_color = bg_color[1],
    )
}
pretty_map <- function(ldata) {
  
  p <- leaflet(ldata) %>% addTiles() %>% addCircleMarkers(lng = ~longitude,lat = ~latitude, 
                                                      label=~price_pretty, popup=~msg, color=~bg_color,
                                                      clusterOptions = markerClusterOptions(),
                                                      labelOptions = labelOptions(noHide = T, textsize = "12px",direction = "bottom"))
  return(p)
}

get_map <- function(prices_data,
                    POSTCODE = "EC1M 6AD",
                    BEDS = c(0,2),
                    DIST = 1000,
                    DATE = '2020-01-01',
                    FS = FALSE) {
  
  house_data <- apply_property_filter(prices_data, POSTCODE, BEDS, DIST, DATE, FS)
  if(nrow(house_data)==0) return(NULL)
  ldata <- make_pretty_df(house_data)
  pretty_map(ldata)
}

# prices_data <- load_data("SW11")
# (p <- get_map(prices_data, DIST=2000, DATE='2020-08-01',BEDS = c(3,3),
#               POSTCODE = sample(prices_data$postcode)[1],FS = T))

# htmlwidgets::saveWidget(p, file="map.html")
