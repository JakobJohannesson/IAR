library(borsdata)
library(stringr)
library(dplyr)

# Api key


year<-fetch_year(106,key = key)
r12<-fetch_r12(106,key = key)
kvartal<-fetch_quarter(106,key = key)



# städar upp datan
for(i in 1:33){
  year[,i]<-rev(year[,i])
  r12[,i]<-rev(r12[,i])
  kvartal[,i]<-rev(kvartal[,i])
  
}
year[11,]<-r12[10,]
year<-year[-1,]


cagr<-(year$revenues[10]/year$revenues[1])^(1/length(year$year))-1

# Visualiserar
library(ggplot2)

skr<-data.frame(year=year$year,revenue=year$revenues,
                ebit=year$profit_Before_Tax,ebit_marg=year$profit_Before_Tax/year$revenues)


plot_labels <- data.frame(label = round(cagr,digits = 3)*100,
                          x = seq(2009.5,2018.5,1),
                          y = year$revenues[1:10]*1.3)
ggplot(data =  skr, aes(x=year))+ 
  geom_bar(stat = "identity", 
           fill = "dark green",   
           colour = "black", aes(y=skr$revenue)) + theme_bw()+ labs(
             x = "År",
             y = "Omsättning\n MKR",
             title = "IAR omsättning och rörelsemarginal mellan 2010 och 2019",
             caption = "Källa: Börsdata"
           )+ 
  theme(
    plot.title = element_text(hjust = 0.5)
  )+ theme(
    panel.grid.major.x =
      element_blank(),
    panel.grid.minor.x =
      element_blank(),
    panel.grid.major.y =
      element_line(color = "grey")
  )+
  scale_x_continuous(breaks=seq(2009,2019,1))+ 
  geom_text(data = plot_labels,
            aes(x = 2014, y = 370,
                label = paste("cagr:",label)), size=5, parse = TRUE)+
  annotate("segment", x = 2010, xend = plot_labels$x[10],
           y = year$revenues[2]*1.02,
           yend = year$revenues[10]*1.11,
           arrow = arrow(length = unit(1, "lines")), colour = "grey50")+
  geom_line(aes(y=round(ebit_marg, digits=3)*1000)) +
  scale_y_continuous(sec.axis = sec_axis(~.*0.001,
                                         name = "Rörelsemarginal [%]",
                                         labels = seq(0,0.4,0.1)*100,
                                         breaks = seq(0,0.4,0.1)))+
  geom_text(aes(x = year, y = revenue,
                label=round(skr$ebit/skr$revenue, digits=3)*100),
            vjust=1.5, color="white", size=3.5)




#### DCF #### 

library(stringr)
library(readxl)
library(dplyr)
library(lubridate)


namn<-"data/novotek.xlsx" # namn på excel filen





#### Kurs utveckling - veckodata ####

figur<-function(namn){
  library(xts)
  kurs<- read_excel(namn, 
                    sheet = "PriceWeek")
  kurs<-kurs[-1,]
  kurs<-data.frame(kurs$Date,kurs$Openprice)
  x<-kurs
  rownames(kurs)<-kurs$kurs.Date
  kurs<-as.matrix(kurs)
  kurs<-kurs[,-1]
  kurs<-as.matrix(kurs)
  kurs<-as.data.frame(kurs)
  rownames(kurs)<-x$kurs.Date
  colnames(kurs)<-c("Price")
  kurs$Price<-as.numeric(as.character(kurs$Price))
  MTG_Kurs <- as.xts(kurs, descr='my new xts object')
  figur<-plot.xts(MTG_Kurs, main = namn)
  return(figur)
}
figur(namn=namn, skr = skr)



