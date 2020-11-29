make_model_df <- function(df) {
  
  lmdf <- df %>% 
    group_by(property_key) %>% 
    arrange(desc(trans_date), desc(date_scraped)) %>% 
    mutate(
      amenities=ifelse(amenities=="",NA,amenities),
      features=ifelse(features=="",NA,features),
      desc=ifelse(desc=="",NA,desc),
      ) %>% 
    summarise(
      address=na.omit(address)[1],
      postcode=na.omit(postcode)[1],
      area=na.omit(area)[1],
      beds=na.omit(beds)[1],
      longitude=na.omit(longitude)[1],
      latitude=na.omit(latitude)[1],
      energy_use=na.omit(energy_use)[1],
      amenities=tolower(na.omit(amenities)[1]),
      features=tolower(na.omit(features)[1]),
      epc_raw_text=na.omit(epc_raw_text)[1],
      desc=na.omit(desc)[1]
    ) %>% 
    filter(!is.na(area)) %>% 
    mutate(
      epc_dwelling = epc_raw_text %>% tolower  %>% str_extract("dwelling type.*ref|dwelling type:.*") %>% str_replace_all("dwelling type:|ref","") %>% str_trim(),
      typeof = case_when(
        str_detect(epc_dwelling, 'flat|apartment') ~ "flat",
        str_detect(epc_dwelling, 'house') ~ "house",
        str_detect(epc_dwelling, 'maisonette') ~ "maisonette",
        str_detect(amenities, 'flat|apartment') ~ "flat",
        str_detect(amenities,'house') ~ "house",
        str_detect(amenities,'maisonette') ~ "maisonette",
        str_detect(features,'flat|apartment') ~ "flat",
        str_detect(features,'house') ~ "house",
        str_detect(features,'maisonette') ~ "maisonette",
        str_detect(tolower(address),'flat|apparment') ~ "flat",
        str_detect(desc,'flat|apartment') ~ "flat",
        str_detect(desc,'house') ~ "house",
        str_detect(desc,'maisonette') ~ "maisonette",
      ),
      beds = ifelse(beds > 3, 4,beds),
      beds = as.numeric(beds)
    )
  
  # postcode does not contain building information
  # so create address that can identify building within a postcode
  lmdf$small_address <- lmdf$address %>% tolower %>% str_replace_all("^,","") %>% str_split(",") %>% map_chr(function(x) x  %>% unlist %>% str_trim %>%  str_subset("flat|appartment|apartment|^london|^[ew]c1|^[\\d -]+$|^,$",negate=T) %>% head(2) %>% str_trim() %>% paste(collapse=", "))
  
  #print(paste(length(unique(lmdf$small_address))," unique small addresses"))
  #table(lmdf$small_address) %>% sort %>% tail(20)
  
  df2 <- df %>% ungroup %>% 
    filter(status_clean %in% c("For Sale","Sold") & !is.na(price_num)) %>% 
    arrange(desc(trans_date)) %>% 
    filter(!duplicated(listing_key)) %>% 
    group_by(property_key) %>% 
    summarise(last_price = price_num[1],
              last_date = as.numeric(trans_date)[1],
              avg_price=as.numeric(mean(price_num)),
              avg_date=as.numeric(mean(trans_date))
    )
  
  lmdf <- left_join(lmdf, df2, by="property_key")
  
  return(lmdf)
}

# for each model we get a matrix of probs
# average the prob across models
predict_ensemble <- function(data,ensemble_models=ensemble_models) {
  
  pred_list <- map(ensemble_models, function(x) {
    probs <- predict(x, data)
    prob_mat <- matrix(probs, nrow=nrow(data), byrow = T)
    return(prob_mat)
  })
  
  # convert to 3 dim array
  pred <- array(unlist(pred_list), dim=c( dim(pred_list[[1]]), length(pred_list))) %>% 
    # take mean across modeks
    apply(c(1,2), mean) %>% 
    # take max index
    apply(1,which.max)
  
  return(pred-1) # 0 bed is first
}


# get avg beds, min meds, max beds, sd etc
address_feats <- function(df, min_samples=3) {
  
  df %>% 
    summarise(
      avg_beds = mean(beds), 
      sd_avg_beds=sd(beds), 
      num_prop=length(beds),
      min_beds = min(beds),
      max_beds = max(beds),
      avg_area = mean(area)
    ) %>% 
    filter(num_prop >= min_samples)
  
  
  
}

create_feat_df <- function(df) {
  
  traindf <- df %>% select(beds, area, longitude, latitude, small_address, postcode, energy_use,last_price, last_date, avg_price, avg_date, typeof) %>% ungroup
  traindf$typeof <- factor(traindf$typeof, levels=c("flat","house","maisonette")) %>% as.numeric()
  
  # fold is by postcode, so will be no leakage here
  avg_bed_info <- traindf %>% ungroup %>% group_by(small_address,postcode) %>% address_feats
  avg_bed_info_postcode <- traindf %>% ungroup %>% group_by(postcode) %>% address_feats
  
  traindf_ab <- traindf %>% 
    left_join(avg_bed_info, by=c("small_address","postcode")) %>% 
    left_join(avg_bed_info_postcode, by="postcode",suffix=c("","_post"))
  
  return(traindf_ab)
  
}
