#libs
library(tidyverse)
library(sf)
library(janitor)

zip_codes <- read_csv('data\\zip_code_database.csv')

zip_shp <- read_sf('data\\cb_2013_us_zcta510_500k.shp')

submitted_orders <- read_csv("data\\Submitted Orders - Frozen Delivery - Form Responses 1.csv",col_types = cols(`Zip Code` = col_character())) %>%
  clean_names()

submitted_orders_q <- submitted_orders %>%
  group_by(zip_code) %>%
  summarise(total = n())

zips_chicago <- zip_shp %>% 
  inner_join(submitted_orders_q,by = c('ZCTA5CE10' = 'zip_code' ))


ggplot(zips_chicago)+
  geom_sf(aes(fill = total))+
  scale_fill_viridis_c()

filtbbox <- st_bbox(obj = c(xmin = -89, xmax = -87, ymin = 41, ymax = 42.45),crs = st_crs(zips_chicago)) %>%
  st_as_sfc(.)

filt_data <- st_within(zips_chicago, filtbbox)
filt_data <- zips_chicago[which(lengths(filt_data) != 0), ]

st_write(filt_data,'data\\filtdata.shp',)

ggplot(filt_data)+
  geom_sf(aes(fill = total))+
  scale_fill_viridis_c()

summ <- filt_data %>%
  summarise(total = n())

ggplot(st_simplify(summ,dTolerance = 0.1)) +
  geom_sf()


new_poli <- st_read('data\\new_poli.shp') %>%
  filter(ZCTA5CE10 == 'totals')

selected_zips <- st_within(zip_shp, new_poli, sparse = F)

df_zips <- zip_shp %>% filter(selected_zips)

ggplot(df_zips)+
  geom_sf()

grupos <-  st_read('data\\grupos.shp') %>% st_transform(crs =  6454)

filt_data <- st_within(zip_shp  %>% st_transform(crs =  6454), grupos %>% filter(ZCTA5CE10 == 'total'))
filt_data <- zip_shp[which(lengths(filt_data) != 0), ] %>% st_transform(crs =  6454)


intersects <- st_intersects(filt_data, grupos %>% filter(ZCTA5CE10 != 'total'))

unig1 <- st_intersection(filt_data, grupos %>% filter(ZCTA5CE10 == 'G1') )

ggplot(unig1)+
  geom_sf()

## assignate single occurences

filt_data$group <- NA_integer_

for(i in 1:length(intersects)){
  inter <- intersects[[i]]
  if(length(inter) == 1){
    filt_data$group[i] <- inter
  } 
  else if(length(inter) == 0){
    filt_data$group[i] <- 6
  }
}



ggplot(filt_data)+
  geom_sf(aes(fill = group))


resto <- filt_data %>% filter(is.na(group)) %>%
  st_intersection(grupos %>%
                    filter(ZCTA5CE10 != 'total')) %>% 
  mutate(area = st_area(.)) %>%
  group_by(ZCTA5CE10) %>%
  filter(area == max(area)) %>%
  ungroup()

filt_data[is.na(filt_data$group),'group'] <- resto$ZCTA5CE10.1 %>% str_extract(pattern = '[:digit:]') %>% as.numeric()

ggplot(filt_data)+
  geom_sf(aes(fill = as.factor(group)))+
  geom_sf(data = grupos, aes(fill = ZCTA5CE10), alpha = .2) 
  

filt_data %>% st_drop_geometry() %>% write_csv('out\\zips_grouped.csv')


#add second layer

points_df <- data.frame(
  local = c('Homewood', 'La Grange', 'Riverside'),
  lat = c(41.561150435874005, 41.81452909413291,41.82800880086439 ),
  lon = c(-87.66453788220142,-87.87064915680024, -87.8191135721436)
) %>% st_as_sf(coords = c('lon', 'lat'), crs = 4326)%>%
  st_transform(st_crs(filt_data)) %>% 
  st_buffer(8046.72)



ggplot(filt_data)+
  geom_sf( fill  = 'red')+
  geom_sf(data = grupos, alpha = .2) +
  geom_sf(data = points_df, fill = 'steelblue', alpha = .6)

# new bbox
filtbbox2 <- st_bbox(obj = c(xmin = -125.0011, xmax = -66.9326, ymin = 22, ymax = 49.5904),crs = 4326) %>% 
  st_as_sfc(.) %>% 
  st_transform(st_crs(zip_shp)) 

filt_data2 <- st_within(zip_shp %>% st_transform(crs =  6454), filtbbox2 %>% st_transform(crs =  6454))
filt_data2 <- st_transform(zip_shp,crs =  6454)[which(lengths(filt_data2) != 0), ]

ggplot(filt_data2)+geom_sf()


df_codes <- left_join(filt_data2 %>% st_drop_geometry(), filt_data %>% st_drop_geometry(),
                      by = c("ZCTA5CE10", "AFFGEOID10", "GEOID10", "ALAND10", "AWATER10"))

df_codes$group[is.na(df_codes$group)] <- 7

df_codes %>% write_csv('out\\zips_grouped.csv')
