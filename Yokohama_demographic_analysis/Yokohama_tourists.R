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
Yokohama_tourist<-read_csv("/Users/koichiito/Documents/NUS/Academic Matter/2020 Fall/DEP5103 Urban and Regional Planning/Studio/Data/keizaikanko-kanko.csv")


## ---------------------------------------------------------------------------------------------------------
Yokohama_tourist %>% 
  ggplot(data=.,aes(x=`年（西暦）`,y=`総数［人］`))+
  geom_line(color = "#00AFBB")+
  geom_vline(xintercept=2000,
              linetype=4, colour="black",
           show.legend = TRUE)+
  theme_minimal()+
  theme(panel.grid = element_blank())+
  theme_set(theme_bw(base_family = "HiraKakuProN-W3"))+
  guides(fill = guide_legend(reverse = TRUE))+
  scale_y_continuous(labels = comma)+
  labs(
    x='Year',
    y='Total Number of Tourists',
    title = 'Changes in the Number of Tourists in Yokohama',
    caption='Data: Yokohama City',
    fill=''
  )

ggsave("ChangeinTourists.png",dpi=400)

