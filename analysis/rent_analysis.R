# Analyse the rent situation post COVID in London
# See post https://sbikram.com/posts/propertydata/property-rents-london/
# and https://sbikram.com/posts/propertydata/covid_lnd_rents/

library(tidyverse)
library(lubridate)
library(glmnet)
library(doMC)
library(mapview)
library(ggrepel)

doMC::registerDoMC(4)
dat <- readRDS("ALL.rds")
summary(dat)

table(is.na(dat$listing_id))
table(is.na(dat$property_key))

# Get London postcode zones, etc
# download below file from https://www.doogal.co.uk/london_postcodes.php 
zones_raw <- read_csv("~/Downloads/London postcodes.csv")
zones <- zones_raw %>% select(postcode_district=`Postcode district`, zone=`London zone`) %>% distinct()
table(zones$postcode_district)

dat$postcode_area <- str_extract(dat$postcode,"^[A-Z]+\\d+")
dat$postcode_district <- str_extract(dat$postcode,"[^ .]+") %>% str_trim()

table(dat$postcode_area)
table(dat$postcode_district)

dat <- left_join(dat, zones %>% select(postcode_district, zone), by="postcode_district")

# Filter all data to just rent listings
rent_dat <- dat %>% 
  filter(str_detect(status_clean,"Rent") & zone < 4) %>% 
  mutate(
    sale_year=year(trans_date),
    sale_month=month(trans_date, label=T)
  ) %>% filter(!is.na(price_num))  %>% arrange(desc(trans_date)) %>% 
  arrange(price_num) %>% 
  distinct(listing_key, sale_month, sale_year,.keep_all = T) %>% 
  select(listing_key, sale_month, sale_year, price_num, zone, postcode_area,postcode_district) %>% 
  arrange(sale_month, sale_year) %>% 
  filter(sale_year > 2014) %>% 
  mutate(
    sale_year = as.factor(ifelse(sale_year < 2020, "2015-2019 avg", "2020"))
  )

table(rent_dat$sale_year[!duplicated(rent_dat$listing_key)])

summary(rent_dat)

top_dist <- rent_dat %>% ungroup %>%
  group_by(postcode_district) %>% 
  count() %>% filter(n>1000) #%>% head(10)

pdata <- rent_dat %>% ungroup %>%
  filter(zone < 4) %>% 
  mutate(
    postcode_area = ifelse(postcode_district %in% top_dist$postcode_district,postcode_district,postcode_area),
    postcode_area = str_extract(postcode_area, "[A-Z]+\\d+")
  ) %>% 
  group_by(sale_month, sale_year, zone) %>%
  summarise(
    #meanp = mean(price_num),
    medianp = mean(price_num)
  ) %>% 
  mutate(
    sale_month = factor(sale_month, levels=levels(unique(rent_dat$sale_month)))
  ) %>% filter(!is.na(zone))

pdata <- mutate(pdata, zone = paste0("Zone ", zone))

p <- ggplot(pdata, aes(x=as.numeric(sale_month), y=medianp, color=sale_year)) +
  #geom_line() +
  stat_smooth(se=T, size=0.75, alpha=0.1) +
  scale_x_continuous("",breaks=c(1:12), labels=c("j","f","m","a","m","j","j","a","s","o","n","d")) +
  scale_y_log10("Rent in £", limits=c(1500,8000), breaks=c(1500,2000,3000,5000,7500)) +
  theme_minimal() + facet_wrap("zone", ncol=3) +
  geom_hline(yintercept = 1, size=0.5, colour="grey80") +
  geom_vline(xintercept=4, size=0.5, linetype="solid", color="grey90") +
  annotate("text", x=4,y=1500,label="WFH starts", hjust=0, vjust=1.5, angle=90, color="grey80",size=3) +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(linetype="solid",size=0.2, color="grey80"),
    panel.grid.minor.y = element_line(linetype="solid",size=0.1, color="grey80"),
    strip.text = element_text(size=12,hjust=0),
    legend.position = "top"
  ) +
  scale_color_brewer("", type="qual", palette = "Set2")

p
ggsave("londzone.svg",p, height=5, width=8,scale = 1)

pdata <- rent_dat %>% ungroup %>%
  filter(zone < 2 & !postcode_area %in% c("CM") & !postcode_district %in% c("W8")) %>% 
  mutate(
    postcode_area = ifelse(postcode_district %in% top_dist$postcode_district,postcode_district,postcode_area),
    postcode_area = str_extract(postcode_area, "[A-Z]+\\d*")
  ) %>% 
  group_by(sale_month, sale_year, postcode_area) %>%
  summarise(
    #meanp = mean(price_num),
    medianp = mean(price_num)
  ) %>% 
  bind_rows(
    data.frame(sale_month=c("Nov","Dec"), sale_year=c("2020","2020"), meanp=c(0,0), medianp=c(0,0), zone=c(1,1))
  ) %>% 
  mutate(
    sale_month = factor(sale_month, levels=levels(unique(rent_dat$sale_month)))
  ) %>% filter(!is.na(postcode_area))

unique_pcodes <- unique(pdata$postcode_area)
unique_pcodes <- unique_pcodes[order(str_extract(unique_pcodes,"[A-Z]"), as.numeric(str_extract(unique_pcodes,"\\d+")))]
unique_pcodes

pdata$postcode_area <- factor(pdata$postcode_area, levels = unique_pcodes)

pdc <- pdata %>% spread(sale_year, medianp) %>% mutate(diff = `2020`/`2015-2019 avg`)

pdc$postcode_area <- factor(pdc$postcode_area, levels = pdc %>% filter(sale_month=="Oct") %>% arrange(diff) %>% pull(postcode_area))

p <- ggplot(pdc, aes(x=as.numeric(sale_month), y=diff)) +
  
  scale_x_continuous("",breaks=c(1:12), labels=c("j","f","m","a","m","j","j","a","s","o","n","d")) +
  scale_y_continuous("% change in rent",labels = function(x) scales::percent(x-1,0.1), limits = c(0.2,2)) +
  theme_minimal() + facet_wrap("postcode_area", ncol=3) +
  geom_hline(yintercept = 1, size=0.5, colour="grey80") +
  geom_vline(xintercept=4, size=0.5, linetype="solid", color="grey90") +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(linetype="solid",size=0.2, color="grey90"),
    axis.text.y = element_text(size=6),
    strip.text = element_text(size=8,hjust=0)
  ) +
  stat_smooth(se=F, size=0.75, color="navy")

p
ggsave("londpostz1.svg",p, height=5, width=7,scale = 1)

##--- Modeling ----
# We want to build a rent price index to see change in rent prices
# See https://sbikram.com/posts/propertydata/covid_lnd_rents/

library(ggplot2)
library(rgdal)
library(maptools)

# Read the downloaded Shapefile from disk
postal <- sf::st_read("~/Downloads/London-wards-2018/London-wards-2018_ESRI/London_Ward.shp")

# get ward code for each postcode
postcode_ward <-zones_raw %>% 
  filter(`In Use?`=="Yes") %>% 
  select(postcode=Postcode, 
         GSS_CODE=`Ward Code`, zone=`London zone`) %>% distinct()

# create data for model
mdf <- dat %>% 
  inner_join(postcode_ward %>% filter(zone==1),by="postcode") %>% 
  filter(str_detect(status_clean,"Rent") & price_num > 500 & price_num < 10e3) %>% 
  select(property_key, price_num, trans_date, postcode_area, postcode_district, postcode, GSS_CODE) %>% 
  na.omit() %>% 
  distinct() %>% 
  mutate(
    sale_year=year(trans_date),
    sale_month=month(trans_date, label=T)
  )  %>% 
  # distinct properties/listings per sale year and month
  arrange(desc(trans_date),price_num) %>% 
  distinct(property_key, sale_month, sale_year,.keep_all = T)

mdf %>% distinct(GSS_CODE) %>% inner_join(postal,.) %>% mapview()


# only pick properties with more than 1 year of listings
mdfr <- semi_join(
  mdf,
  mdf %>% group_by(property_key) %>% filter(!duplicated(sale_year)) %>% count() %>% filter(n>1),
  by="property_key"
) %>% mutate(
  postcode_region = str_extract(postcode_area,"[A-Z]+")
) %>% select(-trans_date)

# only pick postcode districts with more than n properties.
top_districts <- mdfr %>% group_by(postcode_district) %>% distinct(property_key) %>% 
  count() %>% arrange(n)
top_districts
summary(top_districts)
top_districts <- filter(top_districts, n>25)

# only pick postcode areas with more than 100 properties
top_areas <- mdfr %>% group_by(postcode_area) %>% distinct(property_key) %>% 
  count() %>% arrange(n)
top_areas
summary(top_areas)
top_areas <- filter(top_areas, n>50)
top_regions <- unique(mdfr$postcode_region)
properties <- unique(mdfr$property_key)
sale_years <- unique(mdfr$sale_year)

featurise_postcode <- function(mdfr) {
  
  mdfr %>% mutate(
    postcode_district = ifelse(postcode_district %in% top_districts$postcode_district,
                               postcode_district,
                               "other"
    ),
    postcode_area = ifelse(postcode_area %in% top_areas$postcode_area,
                           postcode_area,
                           "other"),
    postcode_district = factor(postcode_district, levels=c("other",top_districts$postcode_district)),
    postcode_area = factor(postcode_area, levels=c("other",top_areas$postcode_area)),
    postcode_region = factor(postcode_region, levels=top_regions),
    sale_year = factor(sale_year, levels=sale_years),
    property_key = factor(property_key, levels=properties)
  )
  
}

lmdf <- featurise_postcode(mdfr)

dim(lmdf)
length(unique(lmdf$property_key))
summary(lmdf)

length(unique(lmdf$postcode_district))
length(unique(lmdf$postcode_area))

summary(lmdf$postcode_district)

summary(lmdf %>% select(price=price_num,postcode_area,postcode_district,postcode_region))

# time series of random 8 properties
p2 <- lmdf %>% filter(property_key %in% sample(unique(mdfr$property_key),8)) %>% 
  mutate(
    sale_year = as.numeric(as.character(sale_year))
  ) %>% 
  group_by(property_key) %>% 
  mutate(
    price_num = 100*(price_num/mean(price_num))
  ) %>% 
  ggplot(aes(sale_year, price_num, colour=property_key)) + 
  geom_point(alpha=1) + 
  geom_line() + theme_minimal() + scale_x_continuous("year", breaks=seq(2010,2020,2)) +
  scale_y_continuous("Rent compared to average rent of the property") +
  theme(legend.position = "top") + scale_color_brewer("Property ID", type="qual", palette = "Set2")
p2
#ggsave("londPropHist.svg",p2, height=5, width=7,scale = 1)

p3 <-  lmdf %>% filter(property_key %in% sample(unique(mdfr$property_key),200)) %>% 
  mutate(
    sale_year = as.numeric(as.character(sale_year))
  ) %>% 
  group_by(property_key) %>% 
  mutate(
    price_num = 100*(price_num/mean(price_num))
  ) %>% filter(between(price_num, 50,200) & sale_year > 2010) %>% 
  ggplot(aes(sale_year, price_num, group=property_key)) + 
  geom_point(alpha=0.1) + 
  geom_line(alpha=0.25) + 
  #stat_smooth(method="lm",formula=y~x,se=F,size=0.1,alpha=0.1) +
  theme_minimal() + scale_x_continuous("year", breaks=seq(2010,2020,2)) +
  scale_y_continuous("Normalised rent") +
  theme(legend.position = "top") + scale_color_brewer("Property ID", type="qual", palette = "Set1") 
p3
ggsave("londPropHistAll.svg",p3, height=3, width=7,scale = 1)


fom <-as.formula(log(price_num) ~ 
             property_key + postcode_district + postcode_area + sale_year + sale_month + postcode_region +
             postcode_region:sale_year +
             postcode_district:sale_year +
             postcode_area:sale_year - 1)

xmat <- Matrix::sparse.model.matrix(fom, lmdf)
cnames <- colnames(xmat)
str_subset(cnames,"property|sale_year",negate = T)
set.seed(1)
mod <- cv.glmnet(xmat, y=log(mdfr$price_num),alpha=0,nfolds=4,parallel = TRUE)
plot(mod)
min(mod$cvm)

base_df <- mdfr %>%
  featurise_postcode %>% 
  filter(!is.na(property_key)) %>% 
  distinct(GSS_CODE,postcode_district,postcode_area,postcode_region,.keep_all = T)

summary(base_df)


x2019 <- Matrix::sparse.model.matrix(fom,
            mutate(base_df,
              sale_year=factor(2019, levels=(sale_years)), 
              sale_month=factor("May", levels=levels(lmdf$sale_month)))
)

dim(x2019)

dim(na.omit(x2019))

x2020 <- Matrix::sparse.model.matrix(fom,mutate(base_df,
                                         sale_year=factor(2020, levels=sale_years), 
                                         sale_month=factor("May", levels=levels(lmdf$sale_month)))
)

gss_chg <- base_df %>% 
  select(GSS_CODE) %>% 
  mutate(
    pred_2019=predict(mod, x2019, lambda=mod$lambda.min)[,1] %>% exp(),
    pred_2020=predict(mod, x2020, lambda=mod$lambda.min)[,1] %>% exp(),
    chg = 100*(pred_2020/pred_2019 - 1)
  ) %>% group_by(GSS_CODE) %>% summarise(`pctchange`=round(mean(chg),2))
mapviewOptions(fgb = FALSE)
mp <- inner_join(postal, gss_chg, by=c("GSS_CODE")) %>% 
  mapview(zcol='pctchange')

mapshot(mp,"rentchg.html",
        remove_controls=c("zoomControl", "layersControl", "homeButton"), selfcontained=FALSE)

htmlwidgets::saveWidget(mp@map, "sw.html", selfcontained=TRUE)

