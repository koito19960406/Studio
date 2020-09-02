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
yokohama_employment<-read_csv("/Users/koichiito/Documents/NUS/Academic Matter/2020 Fall/DEP5103 Urban and Regional Planning/Studio/Data/rodo-kyujin.csv")


## ---------------------------------------------------------------------------------------------------------
p1<-ggplot()
p1+geom_line(data=yokohama_employment,aes(x=`年度（西暦）`,
                                          y=`月間有効求職者数（年度計）［人］`,
                                          colour="blue"), alpha=0.8)+
  geom_line(data=yokohama_employment,aes(x=`年度（西暦）`,
                                         y=`月間有効求人数（年度計）［人］`,
                                         colour="red"), alpha=0.8)+
  geom_vline(xintercept=2000,
            linetype=4, colour="black",
            show.legend = TRUE)+
  scale_color_identity(name = "Employment",
                       breaks = c("blue","red"),
                       labels = c("People who are looking for jobs", "Available jobs"),
                       guide = "legend")+
  theme_minimal()+
  theme(
    panel.grid = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal")+
  theme_set(theme_bw(base_family = "HiraKakuProN-W3"))+
  guides(fill = guide_legend(reverse = TRUE))+
  scale_y_continuous(labels = comma)+
  labs(
    x='Year',
    y='Demand and Supply',
    title = 'Demand and Supply of Employment in Yokohama',
    caption='Data: Yokohama City',
    fill=''
  )

ggsave("ChangeinEmployment.png",dpi=400)

