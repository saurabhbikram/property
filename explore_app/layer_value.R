library(sp)

make_df_for_spatial <- function(raw_data) {
  raw_data %>% 
    mutate(value=price_num/area) %>% 
    filter(str_detect(status_clean,"Sold|For Sale") & trans_date > Sys.Date()-365*3 & !is.na(value)) %>% 
    select(longitude, latitude, value)
}

get_spatial_df <- function(dfs, breaks=15) {
  dfs <- mutate(dfs, 
           lon_cut=cut(longitude,breaks,include.lowest = T),
           lat_cut=cut(latitude,breaks,include.lowest = T)) %>% 
    group_by(lat_cut, lon_cut) %>% 
    summarise(value=mean(value)) %>% 
    mutate(l_start = str_replace_all(lat_cut,"[\\(\\[]|,.*","") %>% as.numeric, 
           l_end = str_replace_all(lat_cut,"[\\(\\[]|.*,|\\]","") %>% as.numeric, 
           lo_start = str_replace_all(lon_cut,"[\\(\\[]|,.*","") %>% as.numeric, 
           lo_end = str_replace_all(lon_cut,"[\\(\\[]|.*,|\\]","") %>% as.numeric
    )
  
  ps <- map(1:nrow(dfs), function(i){
    crds <- cbind(x=c(dfs$lo_start[i],dfs$lo_start[i],dfs$lo_end[i],dfs$lo_end[i]), 
                  y=c(dfs$l_start[i],dfs$l_end[i],dfs$l_end[i],dfs$l_start[i]))
    p <- Polygon(crds)
    p <- Polygons(list(p),i)
    return(p)  
  })
  sps = SpatialPolygons(ps)
  proj4string(sps) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  df <- data.frame(value=dfs$value)
  SPDF <- SpatialPolygonsDataFrame(sps, df)
  
  return(SPDF)
  
}

apply_spatial_values <- function(map, SPDF2) {
  
  map %>% addPolygons(
    data=SPDF2,
    color = NA, 
    weight = 1, smoothFactor = 0.5,
    opacity = 1.0, fillOpacity = 0.5,
    fillColor = ~colorQuantile("YlOrRd", value)(value),
    highlightOptions = highlightOptions(),
    label = ~sprintf("£%.0f per sq ft",value),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  )
  
}

adjusted_values_model <- function(raw_data, make_new=FALSE) {
  # model oriented
  fname <- "value_model.rds"
  model_exists <- file.exists(fname)
  if(!model_exists | make_new) {
  library(xgboost)
  xgbdf <- raw_data %>% filter(str_detect(status_clean,"Sold|For Sale")) %>% 
    mutate(value=price_num/area) %>% 
    select(value,trans_date, longitude, latitude) %>% distinct() %>% 
    mutate(trans_date=as.numeric(lubridate::year(trans_date))) %>% filter(!is.na(value))
  
  xmat <- xgbdf %>% select(-value) %>% as.matrix()
  summary(xmat)
  ymat <- xgbdf %>% pull(value) %>% as.numeric() %>% log
  
  params = list(eta=0.3, objective="reg:squarederror")
  print("fitting xgboost model to raw data")
  
  res <- xgb.cv(params=params,data=xmat,
                nrounds=1000, label=ymat,
                nfold=5,
                early_stopping_rounds = 5,
                print_every_n = 50,
                maximize=FALSE, verbose=TRUE)
  
  
  model <- xgboost(params=params,data=xmat, nrounds=res$best_iteration, label=ymat,verbose=F)
  #xgb.importance(model$feature_names,model)
  saveRDS(model, fname)
  } else {
    model <- readRDS(fname)
  }
  return(model)
}

get_adjusted_model <- function(raw_data, model) {
  library(xgboost)
  pred_df <- raw_data %>% distinct(longitude,latitude) %>% 
    mutate(trans_date = 2020, beds=2)
  pred_df$value <- predict(model, as.matrix(pred_df[,model$feature_names])) %>% exp
  return(select(pred_df, longitude, latitude, value))
}

## Testing ----

#raw_data <- load_data("ec")
#map <- leaflet(raw_data %>% na.omit()) %>% addTiles()
# raw_data %>% make_df_for_spatial %>% get_spatial_df %>% apply_spatial_values(map,.)
#model <- adjusted_values_model(raw_data)
#get_adjusted_model(raw_data, model) %>% get_spatial_df %>% apply_spatial_values(map, .)
