setwd("~/property/analysis")
library(leaflet)
library(tidyverse)
library(stringr)
library(ggplot2)
library(glue)
library(lubridate)
library(furrr)
library(kableExtra)
#aws.signature::use_credentials(profile="sb")

source('~/propertydata/data/landregistry/price_paid_data.R')
source('~/property/analysis/clean_listings.R')

# made from pre_processing/pre_process.R
listings <- aws.s3::s3readRDS("preprocess/EC1.rds","ukpd")
clist <- clean_listings(listings)
property_info_full <- create_property_info(clist)

# get and match land registry sales 
lr_sales <- get_lr_sales(unique(property_info_full$postcode))# %>% filter(category=="Standard price paid transaction")
epc <- aws.s3::s3readRDS("epc_process/EC1.rds","ukpd")
matched_lr_sales <- match_lr_sales(property_info_full, lr_sales, epc)

summary(matched_lr_sales$match_type %>% as.factor) %>% print

matched_lr_sales$epc_match_type[is.na(matched_lr_sales$epc_match_type)] <- "no match"
summary(matched_lr_sales$epc_match_type %>% as.factor) %>% print

#t <- matched_lr_sales %>% filter(match_type == "approx_multi") %>% sample_n(1)
#t
#property_info_full %>% filter(property_key %in% unlist(t$property_key)) %>% 
#  select(property_key, address) %>% mutate(address = str_replace_all(address,", London.*","")) %>% distinct() %>% data.frame

matched_lr_sales$raw_property_key <- matched_lr_sales$property_key

w <- matched_lr_sales$match_type %in% c("approx_regex","exact")

matched_lr_sales$property_key <- NA
matched_lr_sales$property_key[w] <- map_chr(matched_lr_sales$raw_property_key, function(x) if(!is.null(x)) if(length(x)==1) x[1] else NA else NA)[w]

summary(is.na(matched_lr_sales$property_key))

head(property_info_full)

lr_sales_all <- select(matched_lr_sales, -raw_property_key) %>% 
  left_join(epc %>% select(epc_id=property_id, area_sqm, energy_use, date_report), by="epc_id") %>% mutate(epc_are_sq_ft = round(10.7639*area_sqm,1))

# add lat, lon, beds etc
lr_sales_all2 <- left_join(lr_sales_all,
                           property_info_full %>% 
                             filter(!duplicated(postcode)) %>% 
                             select(postcode, latitude, longitude), by="postcode") %>% 
                 left_join(property_info_full %>% 
                             select(property_key, beds, area, amenities) %>% 
                             filter(!duplicated(property_key)), by="property_key")

# default prop, listing keys
lr_sales_all2$property_key <- as.character(lr_sales_all2$property_key)
lr_sales_all2$property_key <- ifelse(is.na(lr_sales_all2$property_key),paste0("lr",1:nrow(lr_sales_all2)), lr_sales_all2$property_key)
lr_sales_all2$listing_key <- lr_sales_all2$property_key
lr_sales_all2 <- rename(lr_sales_all2,trans_date=saledate)
lr_sales_all2$status_clean <- "Sold"
lr_sales_all2$longitude <- as.numeric(lr_sales_all2$longitude)
lr_sales_all2$latitude <- as.numeric(lr_sales_all2$latitude)
lr_sales_all2$trans_date <- as.Date(lr_sales_all2$trans_date)
lr_sales_all2$price_num <- as.numeric(lr_sales_all2$price)
lr_sales_all2$area <- ifelse(is.na(lr_sales_all2$area), lr_sales_all2$epc_are_sq_ft,lr_sales_all2$area)

property_info_full <- property_info_full %>% left_join(lr_sales_all2 %>% 
                                   select(property_key, energy_use, date_report, epc_are_sq_ft) %>% filter(!is.na(property_key)) %>% distinct, 
                                 by="property_key")

property_info_full$area <- ifelse(is.na(property_info_full$area), property_info_full$epc_are_sq_ft,property_info_full$area)
property_info_full$epc_are_sq_ft <- NULL


prices_data_sum <- clist %>% 
  arrange(status_clean, desc(trans_date)) %>% 
  distinct(property_key, listing_key, year(trans_date),.keep_all = T) %>% 
  select(property_key, listing_key,trans_date, listing_url, price_num, status_clean) %>% 
  left_join(property_info_full %>% ungroup %>% select(-property_key), by="listing_key")

prices_data_sum <- bind_rows(prices_data_sum, 
                             lr_sales_all2 %>% select(property_key, trans_date, price_num, 
                                                      status_clean, address=street_address, postcode,
                                                      beds, area, amenities,listing_key)
                             ) %>% 
  distinct(property_key, year(trans_date), price_num, status_clean, .keep_all = T)


summary(prices_data_sum)

POSTCODE <- "EC1"
BEDS <- c(0,20)

for_sale <- prices_data_sum %>% 
  filter(str_detect(postcode,POSTCODE) & ( between(beds,BEDS[1],BEDS[2]) | is.na(beds)) & status_clean == "For Sale" & trans_date > '2019-01-01') %>% 
  arrange(desc(trans_date)) %>% filter(!duplicated(property_key))

# find properties which have been sold and join them with FS properties
already_sold <- inner_join(prices_data_sum %>% 
                             filter(status_clean=="Sold") %>% 
                             select(property_key, trans_date), 
                           for_sale %>% select(property_key,fs_saledate=trans_date),
                           by="property_key") %>% distinct %>% 
  # we have the most recent FS date of a property from `for_sale`
  # if this sale date is less than last sold date, we can remove this 
  filter(trans_date > fs_saledate) %>% select(property_key) %>% distinct()

house_data <- anti_join(for_sale, already_sold)

house_data <- prices_data_sum %>%
  filter(str_detect(postcode,POSTCODE) & 
           (between(beds,BEDS[1],BEDS[2]) | is.na(beds)) & 
           status_clean == "Sold" & 
           trans_date > '2019-01-01') %>%
  arrange(desc(trans_date)) %>% filter(!duplicated(property_key))

prop_history <- prices_data_sum %>% 
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

ldata <- house_data %>% #filter(!is.na(area)) %>% 
  mutate(
  price_pretty=paste0(round(price_num/1000,0),"k"),
  bed_msg = ifelse(!is.na(beds),glue("{beds} beds"),""),
  area_msg = ifelse(!is.na(area),glue(" {area} sq ft"),""),
  sale_hist = ifelse(is.na(sale_hist),"",paste0("</br>",sale_hist)),
  msg = glue("{address}, {postcode}</br>{bed_msg}{area_msg}
             </br>{status_clean}, {format(trans_date,\"%b %Y\")}, £{price_pretty}
             {sale_hist}
             {listing_url_desc} {property_key_url}"),
  scale_clor = scale(log(price_num))[,1],
  scale_clor = scales::rescale(scale_clor),
  bg_color = scales::colour_ramp(c("blue","red"))(scale_clor)
) 

w <- is.na(ldata$longitude)

unique_postcodes <- unique(ldata$postcode[w])
library(PostcodesioR)
pst_lat_lon <- map_df(unique_postcodes,postcode_lookup)
w2 <- match(ldata$postcode[w],pst_lat_lon$postcode)
ldata$longitude[w] <- pst_lat_lon$longitude[w2]
ldata$latitude[w] <- pst_lat_lon$latitude[w2]

ldata3 <- ldata %>%  filter(!is.na(longitude)) %>% ungroup %>% 
  arrange(price_num) %>% 
  group_by(paste0(longitude,latitude)) %>% 
  summarise(
    longitude = longitude[1],
    latitude = latitude[1],
    price_pretty = paste(price_pretty,collapse=", "),
    msg = paste(msg,collapse="<p>-------<p>"),
    bg_color = bg_color[1],
  )


p <- leaflet(ldata3) %>% addTiles() %>% addCircleMarkers(lng = ~longitude,lat = ~latitude, 
                                                        label=~price_pretty, popup=~msg, color=~bg_color,
                                                        labelOptions = labelOptions(noHide = T, textsize = "12px",direction = "bottom"))

library(htmlwidgets)
saveWidget(p, file="ec1m.html")

