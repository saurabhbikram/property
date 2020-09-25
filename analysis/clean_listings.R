library(tidyverse)

clean_listings <- function (listings){

  listings %>% mutate(
    listing_id = ifelse(is.na(listing_id), str_extract(listing_url,"\\d+$"),listing_id),
    property_key = ifelse(is.na(property_id),paste0("l",listing_id), property_id) %>% as.factor,
    listing_key = ifelse(is.na(listing_id),paste0("p",property_id), listing_id) %>% as.factor,
    # transaction date is saledate, then listing date and then date scraped (if a listing expired)
    trans_date = ifelse(!is.na(saledate), saledate, 
                        ifelse(!is.na(listing_date) & !listing_status %in% "Listing expired", listing_date,
                               date_scraped)
    ),
    trans_date = as.Date(trans_date, origin='1970-01-01'),
    status_clean = tolower(listing_status),
    status_clean = case_when(
      str_detect(status_clean,"sold|last sold") ~ "Sold",
      str_detect(status_clean,"let") ~ "Rented",
      str_detect(listing_url,"to-rent") & status_clean == "listing expired" ~ "Rent Expired",
      str_detect(listing_url,"for-sale") & status_clean == "listing expired" ~ "Sale Expired",
      str_detect(listing_url,"to-rent") ~ "To Rent",
      str_detect(listing_url,"for-sale|new-homes") ~ "For Sale",
      str_detect(listing_url,"property-history") & str_detect(price,"pcm") ~ "Rent NA",
      str_detect(listing_url,"property-history")  ~ "For Sale"
    ),
    status_clean = factor(status_clean, levels=c("Sold","For Sale","Sale Expired","Rented","To Rent","Rent Expired","Rent NA")),
    price_num = str_replace_all(price,"[£,]","") %>% str_extract("\\d+") %>% as.numeric,
    new_home = str_detect(listing_url,"new-home") & !is.na(listing_url)
  ) %>% 
    return

}

create_property_info <- function(clist) {
  
  property_info <- clist %>% filter(!str_detect(listing_key,"^p")) %>% group_by(property_key, listing_key) %>% 
    arrange(desc(date_scraped), desc(listing_id)) %>% 
    summarise(
      address = na.omit(c(address,streetAddress))[1],
      postcode = na.omit(c(postcode,postcode_prop))[1],
      beds = na.omit(c(beds,beds_prop))[1],
      amenities = na.omit(c(amenities))[1],
      longitude = na.omit(c(longitude))[1],
      latitude = na.omit(c(latitude))[1],
      area=na.omit(area)[1]
    )
  
  property_info2 <- clist %>% group_by(property_key) %>% 
    arrange(desc(date_scraped), desc(as.numeric(as.character(listing_id)))) %>% 
    summarise(
      address = na.omit(c(address,streetAddress))[1],
      postcode = na.omit(c(postcode,postcode_prop))[1],
      beds = na.omit(c(beds,beds_prop))[1],
      amenities = na.omit(c(amenities))[1],
      longitude = na.omit(c(longitude))[1],
      latitude = na.omit(c(latitude))[1],
      area=na.omit(area)[1]
    )
  
  property_info3 <- full_join(property_info, property_info2, suffix=c("","_prop"), by="property_key")
  property_info3$listing_key <- as.character(property_info3$listing_key)
  property_info3$property_key <- as.character(property_info3$property_key)
  
  property_info_full <- property_info3 %>% select(property_key, listing_key)
  for(col in setdiff(names(property_info),c("property_key","listing_key"))){
    
    property_info_full[[col]] <- ifelse(is.na(property_info3[[col]]),
                                        property_info3[[paste0(col,"_prop")]],
                                        property_info3[[col]])
    
  }
  
  property_info_full$longitude <- as.numeric(property_info_full$longitude)
  property_info_full$latitude <- as.numeric(property_info_full$latitude)
  property_info_full$beds <- as.numeric(property_info_full$beds)
  
  lat_lon_map <- property_info_full %>% ungroup %>% select(postcode, longitude,latitude) %>% distinct %>% na.omit
  
  property_info_full <- property_info_full %>% left_join(lat_lon_map, by="postcode", suffix=c("","_ps")) %>% 
    mutate(
      longitude = ifelse(is.na(longitude), longitude_ps,longitude),
      latitude = ifelse(is.na(latitude), latitude_ps,latitude),
      address = str_replace_all(address, "Property history of ","")
    ) %>% select(-latitude_ps, -longitude_ps) %>% ungroup %>% arrange(
      desc(as.numeric(as.character(listing_key)))
    )
  
  return(property_info_full)
  
}