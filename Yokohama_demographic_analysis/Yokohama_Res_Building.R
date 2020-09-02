## ----setup, include=FALSE---------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---------------------------------------------------------------------------------------------------------
packages = c('tidyverse','scales','ggalt','svglite','viridis','forcats')

#Loop through the above list, and check if the package is installed locally.
# Code goes in here!

for (p in packages){
  if(!require(p,character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}


## ---------------------------------------------------------------------------------------------------------
yokohama_res_building<-read_csv("/Users/koichiito/Documents/NUS/Academic Matter/2020 Fall/DEP5103 Urban and Regional Planning/Studio/Data/Yokohama_residential_building.csv")


## ---------------------------------------------------------------------------------------------------------
yokohama_res_building$under_construction <-
  recode(yokohama_res_building$under_construction,'－'='0')
yokohama_res_building$under_construction <- as.numeric(yokohama_res_building$under_construction)


## ---------------------------------------------------------------------------------------------------------
yokohama_res_building %>% 
  select(!(X10)) %>% 
  gather('residents_yes_no','residential_building',total_with_residents:under_construction) %>% 
  filter((district=='神奈川区')&((residents_yes_no=='total_with_residents')|(residents_yes_no=='total_without_residents'))) %>% 
  ggplot(data=.,mapping = aes(x=year,y=residential_building,fill=residents_yes_no))+
  geom_bar(stat= 'identity',position="stack")+
  coord_flip()+
  theme_minimal()+
  scale_y_continuous(labels=comma)+
  theme(panel.grid = element_blank())+
  labs(
    x='Year',
    y='Residential Buildings',
    title = 'The Number of Residential Buildings in Kanagawa District',
    caption='Data: Yokohama City',
    fill=''
  )

ggsave("Residential_Kanagawa_District.png",dpi=400)


## ---------------------------------------------------------------------------------------------------------
yokohama_res_building %>% 
  select(!(X10)) %>% 
  gather('residents_yes_no','residential_building',total_with_residents:under_construction) %>% 
  filter((district=='西区')&((residents_yes_no=='total_with_residents')|(residents_yes_no=='total_without_residents'))) %>% 
  ggplot(data=.,mapping = aes(x=year,y=residential_building,fill=residents_yes_no))+
  geom_bar(stat= 'identity',position="stack")+
  coord_flip()+
  theme_minimal()+
  scale_y_continuous(labels=comma)+
  theme(panel.grid = element_blank())+
  labs(
    x='Year',
    y='Residential Buildings',
    title = 'The Number of Residential Buildings in Nishi District',
    caption='Data: Yokohama City',
    fill=''
  )

ggsave("Residential_Nishi_District.png",dpi=400)


## ---------------------------------------------------------------------------------------------------------
yokohama_res_building %>% 
  select(!(X10)) %>% 
  gather('residents_yes_no','residential_building',total_with_residents:under_construction) %>% 
  filter((district=='中区')&((residents_yes_no=='total_with_residents')|(residents_yes_no=='total_without_residents'))) %>% 
  ggplot(data=.,mapping = aes(x=year,y=residential_building,fill=residents_yes_no))+
  geom_bar(stat= 'identity',position="stack")+
  coord_flip()+
  theme_minimal()+
  scale_y_continuous(labels=comma)+
  theme(panel.grid = element_blank())+
  labs(
    x='Year',
    y='Residential Buildings',
    title = 'The Number of Residential Buildings in Naka District',
    caption='Data: Yokohama City',
    fill=''
  )

ggsave("Residential_Naka_District.png",dpi=400)

