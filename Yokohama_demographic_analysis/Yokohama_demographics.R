## ----setup, include=FALSE---------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE----------------------------------------------------------------------------------------
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
Kanagawa_Age_Population <- read_csv("/Users/koichiito/Documents/NUS/Academic Matter/2020 Fall/DEP5103 Urban and Regional Planning/Studio/Data/Kanagawa_Age_Population.csv")


## ---------------------------------------------------------------------------------------------------------
str(Kanagawa_Age_Population)


## ---------------------------------------------------------------------------------------------------------
Kanagawa_Age_Population$Age<-as.factor(Kanagawa_Age_Population$Age)
Kanagawa_Age_Population$District<-as.factor(Kanagawa_Age_Population$District)
Kanagawa_Age_Population$Population<-as.numeric(gsub(",","",Kanagawa_Age_Population$Population))

## ---------------------------------------------------------------------------------------------------------
str(Kanagawa_Age_Population)

## ---------------------------------------------------------------------------------------------------------
unique(Kanagawa_Age_Population$Age)


## ---------------------------------------------------------------------------------------------------------
Kanagawa_Age_Population<-Kanagawa_Age_Population[!is.na(Kanagawa_Age_Population$Age),]


## ---------------------------------------------------------------------------------------------------------
unique(Kanagawa_Age_Population$Age)


## ---------------------------------------------------------------------------------------------------------
unique(Kanagawa_Age_Population$Year)
unique(Kanagawa_Age_Population$District)


## ---------------------------------------------------------------------------------------------------------
Kanagawa_Age_Population$Population[is.na(Kanagawa_Age_Population$Population)] <- 0


## ---------------------------------------------------------------------------------------------------------
unique(Kanagawa_Age_Population$District)

## ---------------------------------------------------------------------------------------------------------
Kanagawa_Age_Population$District_en <- recode_factor(Kanagawa_Age_Population$District,
              '鶴見区' = 'Tsurumi District',
              '神奈川区' = 'Kanagwa District',
              '西区' = 'Nishi District',
              '中区' = 'Naka District',
              '南区' = 'Minami District',
              '港南区' = 'Konan District',
              '保土ケ谷区' = 'Hodograya Disctrict',
              '旭区' = 'Asahi District',
              '磯子区' = 'Isogo District',
              '金沢区' = 'Kanazawa District',
              '港北区' = 'Kohoku District',
              '緑区' = 'Midori District',
              '青葉区' = 'Aoba District',
              '都筑区' = 'Tsuduki District',
              '戸塚区' = 'Tozuka District',
              '栄区' = 'Sakae District',
              '泉区' = 'Izumi District',
              '瀬谷区' = 'Seya District'
              )


## ---------------------------------------------------------------------------------------------------------
Districts_near_site<-Kanagawa_Age_Population %>% 
  filter(District=='神奈川区'|District=='西区'|District=='中区') %>% 
  group_by(District_en,Year) %>% 
  summarise(total=sum(Population))

Districts_far_from_site<-Kanagawa_Age_Population %>% 
  filter(!(District=='神奈川区'|District=='西区'|District=='中区')) %>% 
  group_by(District_en,Year) %>% 
  summarise(total=sum(Population))


## ---------------------------------------------------------------------------------------------------------
p1<-ggplot()
p1+geom_line(data=Districts_far_from_site,aes(x=Year,y=total,group=District_en),colour="grey", alpha=0.5)+
  geom_line(data=Districts_near_site,aes(x=Year,y=total,colour=District_en),size=1)+
  geom_vline(xintercept=2000,
                linetype=4, colour="black",
             show.legend = TRUE)+
  theme_minimal()+
  theme(panel.grid = element_blank())+
  theme_set(theme_bw(base_family = "HiraKakuProN-W3"))+
  guides(fill = guide_legend(reverse = TRUE))+
  scale_y_continuous(labels = comma)+
  # scale_linetype_manual(name = 'Note',
  #                       values = c('非常事態宣言'),
  #                       guide = guide_legend(override.aes = list(colour = c('black'))))+
  labs(
    x='Year',
    y='Total Population',
    title = 'Changes in the Population in Yokohama by District',
    caption='Data: Yokohama City',
    fill=''
  )

ggsave("ChangeinPopulation.png",dpi=400)


## ---------------------------------------------------------------------------------------------------------
unique(Kanagawa_Age_Population$Age)


## ---------------------------------------------------------------------------------------------------------
Kanagawa_Age_Population$Age <- gsub('　','',Kanagawa_Age_Population$Age)

Kanagawa_Age_Population$Age_Mean<-recode(Kanagawa_Age_Population$Age,
'０～４歳' = 2.5,
'５～９' = 7.5,
'10～14' = 12.5,
'15～19' = 17.5,
'20～24' = 22.5,
'25～29' = 27.5,
'30～34' = 32.5,
'35～39' = 37.5,
'40～44' = 42.5,
'45～49' = 47.5,
'50～54' = 52.5,
'55～59' = 57.5,
'60～64' = 62.5,
'65～69' = 67.5,
'70～74' = 72.5,
'75～79' = 77.5,
'80～84' = 82.5,
'85～89' = 87.5,
'90～94' = 92.5,
'95～99' = 97.5,
'100歳以上' = 102.5
)


## ---------------------------------------------------------------------------------------------------------
Kanagawa_Age_Population %>%
  group_by(District,Year) %>%
  mutate(district_total = sum(Population)) %>% 
  mutate(share=100*Population/district_total) %>%
  group_by(District, Year, Age_Mean) %>%
  summarise(AG_district_share=sum(share))


## ---------------------------------------------------------------------------------------------------------
Kanagawa_Age_Population %>%
  group_by(District,Year) %>%
  mutate(district_total = sum(Population)) %>% 
  mutate(share=100*Population/district_total) %>%
  group_by(District, Year, Age_Mean) %>%
  summarise(AG_district_share=sum(share)) %>% 
  filter(((Year==1990)|(Year==2000)|(Year==2010))) %>% 
  ggplot(data=., mapping=aes(x=Age_Mean, y=Year, fill=AG_district_share))+
  geom_tile(colour="white", size=.2) +
  labs(
    x="Age groups",
    y="",
    title="Age composition in Yokohama 1990-2010",
    fill = "Share of age group",
    caption="Data: Yokohama City"
    ) +
  theme(legend.position='bottom',
        panel.grid=element_blank(),
        legend.key.width = unit(4,"line"),
        legend.key.height = unit(.6,"line")
        ) +
  scale_fill_viridis(discrete=FALSE, option="B", labels = comma)

ggsave("Age_Heatmap_Yokohama.png",dpi=400)


## ---------------------------------------------------------------------------------------------------------
Kanagawa_Age_Population %>%
  group_by(District,Year) %>%
  mutate(district_total = sum(Population)) %>% 
  mutate(share=100*Population/district_total) %>%
  group_by(District, Year, Age_Mean) %>%
  summarise(AG_district_share=sum(share)) %>% 
  filter((District=='神奈川区')&((Year==1990)|(Year==2000)|(Year==2010))) %>% 
  ggplot(data=., mapping=aes(x=Age_Mean, y=Year, fill=AG_district_share))+
  geom_tile(colour="white", size=.2) +
  labs(
    x="Age groups",
    y="",
    title="Age composition in Kanagawa District 1990-2010",
    fill = "Share of age group",
    caption="Data: Yokohama City"
    ) +
  theme(legend.position='bottom',
        panel.grid=element_blank(),
        legend.key.width = unit(4,"line"),
        legend.key.height = unit(.6,"line")
        ) +
  scale_fill_viridis(discrete=FALSE, option="B", labels = comma)

ggsave("Age_Heatmap_Kanagawa_District.png",dpi=400)


## ---------------------------------------------------------------------------------------------------------
Kanagawa_Age_Population %>%
  group_by(District,Year) %>%
  mutate(district_total = sum(Population)) %>% 
  mutate(share=100*Population/district_total) %>%
  group_by(District, Year, Age_Mean) %>%
  summarise(AG_district_share=sum(share)) %>% 
  filter((District=='西区')&((Year==1990)|(Year==2000)|(Year==2010))) %>% 
  ggplot(data=., mapping=aes(x=Age_Mean, y=Year, fill=AG_district_share))+
  geom_tile(colour="white", size=.2) +
  labs(
    x="Age groups",
    y="",
    title="Age composition in Nishi District 1990-2010",
    fill = "Share of age group",
    caption="Data: Yokohama City"
    ) +
  theme(legend.position='bottom',
        panel.grid=element_blank(),
        legend.key.width = unit(4,"line"),
        legend.key.height = unit(.6,"line")
        ) +
  scale_fill_viridis(discrete=FALSE, option="B", labels = comma)
ggsave("Age_Heatmap_Nishi_District.png",dpi=400)


## ---------------------------------------------------------------------------------------------------------
Kanagawa_Age_Population %>%
  group_by(District,Year) %>%
  mutate(district_total = sum(Population)) %>% 
  mutate(share=100*Population/district_total) %>%
  group_by(District, Year, Age_Mean) %>%
  summarise(AG_district_share=sum(share)) %>% 
  filter((District=='中区')&((Year==1990)|(Year==2000)|(Year==2010))) %>% 
  ggplot(data=., mapping=aes(x=Age_Mean, y=Year, fill=AG_district_share))+
  geom_tile(colour="white", size=.2) +
  labs(
    x="Age groups",
    y="",
    title="Age composition in Naka District 1990-2010",
    fill = "Share of age group",
    caption="Data: Yokohama City"
    ) +
  theme(legend.position='bottom',
        panel.grid=element_blank(),
        legend.key.width = unit(4,"line"),
        legend.key.height = unit(.6,"line")
        ) +
  scale_fill_viridis(discrete=FALSE, option="B", labels = comma)

ggsave("Age_Heatmap_Naka_District.png",dpi=400)

