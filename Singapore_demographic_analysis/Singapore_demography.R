# load packages
packages = c('tidyverse','scales','ggalt','svglite','viridis','forcats','reshape')

for (p in packages){
  if(!require(p,character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

# load data
from2000to2010<- read_csv("/Users/koichiito/Documents/NUS/Academic Matter/2020 Fall/DEP5103 Urban and Regional Planning/Studio/Data/respopagesextod2000to2010.csv")
from2010to2019<- read_csv("/Users/koichiito/Documents/NUS/Academic Matter/2020 Fall/DEP5103 Urban and Regional Planning/Studio/Data/respopagesextod2011to2019.csv")

# combine data
sgpop<-bind_rows(from2000to2010,from2010to2019)


# feature engineering
sgpop$AG<-recode(sgpop$AG,
                 "5_to_9"="05_to_09",
                 "0_to_4"="00_to_04")

sgpop$AG_mid <- recode(sgpop$AG,
                       '00_to_04' = 2.5,
                       '05_to_09' = 7.5,
                       '10_to_14' = 12.5,
                       '15_to_19' = 17.5,
                       '20_to_24' = 22.5,
                       '25_to_29' = 27.5,
                       '30_to_34' = 32.5,
                       '35_to_39' = 37.5,
                       '40_to_44' = 42.5,
                       '45_to_49' = 47.5,
                       '50_to_54' = 52.5,
                       '55_to_59' = 57.5,
                       '60_to_64' = 62.5,
                       '65_to_69' = 67.5,
                       '70_to_74' = 72.5,
                       '75_to_79' = 77.5,
                       '80_to_84' = 82.5,
                       '85_to_89' = 87.5,
                       '90_and_over' = 92.5
)


# visualize the data
## let's do singapore as a whole
sgpop %>%
  group_by(Time) %>%
  mutate(total = sum(Pop)) %>% 
  mutate(share=100*Pop/total) %>%
  group_by(Time, AG_mid) %>%
  summarise(AG_share=sum(share)) %>% 
  # filter(((Time==200)|(Time==2005)|(Time==2010)|(Time==2015)|(Time==2019))) %>% 
  ggplot(data=., mapping=aes(x=AG_mid, y=Time, fill=AG_share))+
  geom_tile(colour="white", size=.2) +
  labs(
    x="Age groups",
    y="",
    title="Age composition in Singapore 2000-2019",
    fill = "Share of age group",
    caption="Data: Singapore Government, 2020"
  ) +
  theme(legend.position='bottom',
        panel.grid=element_blank(),
        legend.key.width = unit(4,"line"),
        legend.key.height = unit(.6,"line")
  ) +
  scale_fill_viridis(discrete=FALSE, option="B", labels = comma)

ggsave("Age_Heatmap_Singapore.png",dpi=400)

## let's do central region
central_region<-c('Queenstown',
                  'Bukit Timah',
                  'Novena','Bishan',
                  'Toa Payoh',
                  'Geylang',
                  'Kallang',
                  'Marine Parade',
                  'Sourthern Islands',
                  'Downtown Core',
                  'Straits View',
                  'Marina South',
                  'Marina East',
                  'Rochor',
                  'Newton',
                  'Tanglin',
                  'Museum',
                  'Outram',
                  'River Valley',
                  'Singapore River'
)

sgpop %>%
  filter(PA %in% central_region) %>%
  group_by(Time) %>%
  mutate(total = sum(Pop)) %>% 
  mutate(share=100*Pop/total) %>%
  group_by(Time, AG_mid) %>%
  summarise(AG_share=sum(share)) %>% 
  ggplot(data=., mapping=aes(x=AG_mid, y=Time, fill=AG_share))+
  geom_tile(colour="white", size=.2) +
  labs(
    x="Age groups",
    y="",
    title="Age composition in Central Region 2000-2019",
    fill = "Share of age group",
    caption="Data: Singapore Government, 2020"
  ) +
  theme(legend.position='bottom',
        panel.grid=element_blank(),
        legend.key.width = unit(4,"line"),
        legend.key.height = unit(.6,"line")
  ) +
  scale_fill_viridis(discrete=FALSE, option="B", labels = comma)

ggsave("Age_Heatmap_Central_Region.png",dpi=400)

## let's do Bukit Merah
sgpop %>%
  filter(PA == 'Bukit Merah') %>%
  group_by(Time) %>%
  mutate(total = sum(Pop)) %>% 
  mutate(share=100*Pop/total) %>%
  group_by(Time, AG_mid) %>%
  summarise(AG_share=sum(share)) %>% 
  ggplot(data=., mapping=aes(x=AG_mid, y=Time, fill=AG_share))+
  geom_tile(colour="white", size=.2) +
  labs(
    x="Age groups",
    y="",
    title="Age composition in Bukit Merah Planning Area 2000-2019",
    fill = "Share of age group",
    caption="Data: Singapore Government, 2020"
  ) +
  theme(legend.position='bottom',
        panel.grid=element_blank(),
        legend.key.width = unit(4,"line"),
        legend.key.height = unit(.6,"line")
  ) +
  scale_fill_viridis(discrete=FALSE, option="B", labels = comma)

ggsave("Age_Heatmap_Bukit_Merah.png",dpi=400)

## let's do central region
sgpop %>%
  filter(SZ=='Maritime Square') %>%
  group_by(Time) %>%
  mutate(total = sum(Pop)) %>% 
  mutate(share=100*Pop/total) %>%
  group_by(Time, AG_mid) %>%
  summarise(AG_share=sum(share)) %>% 
  ggplot(data=., mapping=aes(x=AG_mid, y=Time, fill=AG_share))+
  geom_tile(colour="white", size=.2) +
  labs(
    x="Age groups",
    y="",
    title="Age composition in Maritime Square Subzone 2000-2019",
    fill = "Share of age group",
    caption="Data: Singapore Government, 2020"
  ) +
  theme(legend.position='bottom',
        panel.grid=element_blank(),
        legend.key.width = unit(4,"line"),
        legend.key.height = unit(.6,"line")
  ) +
  scale_fill_viridis(discrete=FALSE, option="B", labels = comma)

ggsave("Age_Heatmap_Maritime_Square.png",dpi=400)
