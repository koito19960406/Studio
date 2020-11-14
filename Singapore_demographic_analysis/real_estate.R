packages = c('tidyverse','scales','ggalt','svglite','viridis','forcats','reshape','gcookbook','hrbrthemes','colorspace','ggmap')

# Loop through the above list, and check if the package is installed locally.

for (p in packages){
  if(!require(p,character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

# Condo
condo1<- read_csv("Data/12112020141123_1_NUSlibrary_137.132.123.69 203.118.39.146 203.118.39.146 50848_7077573_625eb5c3d9769a28-4EA8364B-9841-B6C9-91E847FFD1F20EE2.csv")
condo2<- read_csv("Data/12112020141123_2_NUSlibrary_137.132.123.69 203.118.39.146 203.118.39.146 50848_7077573_625eb5c3d9769a28-4EA8364B-9841-B6C9-91E847FFD1F20EE2.csv")
condo3<- read_csv("Data/12112020141123_3_NUSlibrary_137.132.123.69 203.118.39.146 203.118.39.146 50848_7077573_625eb5c3d9769a28-4EA8364B-9841-B6C9-91E847FFD1F20EE2.csv")
condo4<- read_csv("Data/12112020141123_4_NUSlibrary_137.132.123.69 203.118.39.146 203.118.39.146 50848_7077573_625eb5c3d9769a28-4EA8364B-9841-B6C9-91E847FFD1F20EE2.csv")
condo5<- read_csv("Data/12112020141123_5_NUSlibrary_137.132.123.69 203.118.39.146 203.118.39.146 50848_7077573_625eb5c3d9769a28-4EA8364B-9841-B6C9-91E847FFD1F20EE2.csv")
condo6<- read_csv("Data/12112020141123_6_NUSlibrary_137.132.123.69 203.118.39.146 203.118.39.146 50848_7077573_625eb5c3d9769a28-4EA8364B-9841-B6C9-91E847FFD1F20EE2.csv")
condo7<- read_csv("Data/12112020141123_7_NUSlibrary_137.132.123.69 203.118.39.146 203.118.39.146 50848_7077573_625eb5c3d9769a28-4EA8364B-9841-B6C9-91E847FFD1F20EE2.csv")

condo<-bind_rows(condo1, condo2, condo3, condo4, condo5, condo6, condo7)
condo$country<-"Singapore"
condo$city<-"Singapore"

test <- mutate_geocode(condo, Address)

write.csv(condo,"Data/condo.csv")

#HDB
hdb<-read_csv("Data/resale-flat-prices-based-on-registration-date-from-jan-2017-onwards.csv")
hdb<-hdb %>% 
  filter(grepl('2020', month))
hdb<-hdb %>% 
  mutate(unit_price=resale_price/floor_area_sqm)

write.csv(hdb,'Data/hdb_2020.csv')




  
