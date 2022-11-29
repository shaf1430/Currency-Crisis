
library(gt)
library(tidyverse)
library(glue)
library(gtExtras)
library("sf")
library("ggplot2")
library(maps)
library("rnaturalearth")
library("rnaturalearthdata")
library(ggrepel)
library(extrafont)
options(scipen = 999)
library(webshot2) # saving high quality images of gt tables
library(readxl)

#data can be downloaded from this address: https://www.bis.org/statistics/xrusd.htm?m=2675
BIS<-read.csv("BIS.csv")

##########################################################################################data preparation 
##keep only years between 1970 and 2021, and keep only annual rate and end of period 
x<-seq(1970,2021)
x<-paste0("X",x)
y<-names(BIS[,c(1:16)])
z<-c(y,x)

df<-BIS[which(BIS$FREQ=="A" & BIS$COLLECTION=="E"),] %>% select(z)

##convert data to a long format
df_long<-gather(df,year,rate,X1970:X2021,factor_key=TRUE)
df_long$year<-as.character(df_long$year)
df_long$year<-substr(df_long$year,2,nchar(df_long$year))
df_long$year<-as.numeric(df_long$year)

##keep required columns
df_long<-df_long[,c("REF_AREA","Reference.area","CURRENCY","Currency","year","rate")]

##original data shows rate of a currency v.s. one dollar. We calculate how much dollar a currency could buy at each period, then we calculate the Percentage appreciation/depreciation 
df_long$invrate<- 1/(df_long$rate)
df_long<- df_long %>% group_by(REF_AREA) %>% mutate(pct_change = (invrate/lag(invrate) - 1) * 100)

##to determine the year currency crisis started: if the currency fell at least 30% v.s. dollar in the first year and then at least 5% in the next 5 years 
p<-(-5)
df_long<- df_long %>% mutate(depr=ifelse((pct_change) <= (-30) & lead(pct_change,n=1L)<=p & lead(pct_change,n=2L)<=p  & lead(pct_change,n=3L)<=p &
                                           lead(pct_change,n=4L)<=p & lead(pct_change,n=5L)<=p,1,0))

##determine the beginning of the period that crisis started
xx<-filter(df_long,depr==1)
xx<- xx %>% group_by(REF_AREA) %>% mutate(lyear=year - lag(year,n=1L))
xx<-filter(xx,is.na(lyear)==T | lyear!=1)
xx$lyear<-"Yes"

df_long<-left_join(df_long,xx) 

##name of columns
colnames(df_long)<-c("Area","Country","Currency Abb","Currency","Year","Rate","Dollar","Pct_change","Currency Crisis","Start of Crisis")

df_long<-as.data.frame(df_long)
df_long$Country<-as.character(df_long$Country)

##add symbol currencies
symbol<-read_excel("symbol.xlsx",sheet="Sheet1")
symbol<-as.data.frame(symbol)
symbol<-symbol[,c("Country","Symbol")]

df_long<-left_join(df_long,symbol)

##drop if the symbol was not avaialble
df_long<-filter(df_long,is.na(Symbol)==FALSE)
df_long[which(df_long$Country=="Serbia"),"Country"]<-"Republic of Serbia"

#########prepare data for plot

#determine the a year before crisis until five years later for plot
df_long<-arrange(df_long,Area)
index<-rownames(df_long[which(df_long$`Start of Crisis`=="Yes"),])
index<-as.numeric(index)

datalist<-list()

for (j in 1:length(index)) {
  
  df2<-df_long[((index[j]-1):(index[j]+5)),]
  df2$Pct_change1<-0
  df2[2:nrow(df2),"Pct_change1"]<-df2[2:nrow(df2),"Pct_change"]
  df2$timeline<-c(0:6)
  df2$endperiod<-as.character(df2[2,"Year"])
  
  datalist[[j]]<-df2
  
}

data_plot=bind_rows(datalist)

#add label column for labels in plots
data_plot$label<-NA 
data_plot$label[which(data_plot$timeline == 1)]<- data_plot$endperiod[which(data_plot$timeline == 1)]

#########prepare data for map

world <- ne_countries(scale = "medium", returnclass = "sf")

excluding<-c("Antarctica","Greenland","French Southern and Antarctic Lands")
'%!in%' <- function(x,y)!('%in%'(x,y))
world <- filter(world, admin %!in% excluding)

geo_countries <- map_data("world")
geo_countries$Country<-geo_countries$region

# Color palette 
pal<-c("#d11141","#00b159","#00aedb","#f37735", "#fcc425")

##crisis plot function

chart<-function (data){
  
  ncol<-nrow(data %>% group_by(label) %>% count() %>% filter(is.na(label)==F))
  
  pch<- ggplot(data,aes(y=Pct_change1,x=timeline,col=endperiod))+
    
    # x-axis
    annotate(geom='segment',x=1,xend=1,y=-100,yend=-95,lty='solid',size=2)+
    annotate(geom='segment',x=6,xend=6,y=-100,yend=-95,lty='solid',size=2)+
    annotate(geom='text',x=1,y=-105,label='1st Yr',hjust=0.5,vjust=0.6,size=12,family='Courier New')+
    annotate(geom='text',x=6,y=-105,label='5th Yr',hjust=0.5,vjust=0.6,size=12,family='Courier New')+
    # y-axis
    annotate(geom='segment',x=-2.3,xend=7,y=-100,yend=-100,size=2)+
    annotate(geom='segment',x=0,xend=6,y=0,yend=0,lty='dotted',size=2)+
    annotate(geom='segment',x=0,xend=6,y=-25,yend=-25,lty='dotted',size=2)+
    annotate(geom='segment',x=0,xend=6,y=-50,yend=-50,lty='dotted',size=2)+
    annotate(geom='segment',x=0,xend=6,y=-75,yend=-75,lty='dotted',size=2)+
    annotate(geom='text',x=-1,y=0,label='0',hjust=0,size=13,family='Courier New',fontface = "bold")+
    annotate(geom='text',x=-2.3,y=-20,label='-25 %',hjust=0,size=13,family='Courier New',fontface = "bold")+
    annotate(geom='text',x=-2.3,y=-45,label='-50 %',hjust=0,size=13,family='Courier New',fontface = "bold")+
    annotate(geom='text',x=-2.3,y=-70,label='-75 %',hjust=0,size=13,family='Courier New',fontface = "bold")+
    
    geom_point(size=6)+
    geom_line(linewidth=2.5)+ #Warning message:Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.Please use `linewidth` instead
    scale_color_manual(values=pal[1:ncol])+
    scale_y_continuous(limits=c(-105,0))+
    theme_void()+
    theme(plot.margin=margin(0,0,0,0,"cm"))+
    theme(legend.position="top",
          legend.title = element_blank(),
          legend.key.size = unit(2, 'cm'),
          legend.text = element_text(size=17))
  
  return(pch)
}

##crisis plots

crisis_chart_column <-
  data_plot %>%
  group_by(Country) %>%
  nest() %>%
  mutate(chart = purrr::map(data,chart)
  ) %>%
  select(Country,chart)

##currency v.s. a dollar function

chart2<-function (data){
  
  dollar<- ggplot(data,aes(x=Year,y=Dollar))+
    geom_line(size=2.5,color="#fcc425")+
    geom_point(size=6)+
    labs(y="",x="",title=unique(data$Currency)) +
    theme_minimal()+
    theme(axis.text = element_text(family="Courier New",size=35,face="bold",colour = "black"))+
    scale_x_continuous(limits = c(2005, 2022),breaks=c(2006,2020))+
    theme(plot.margin=margin(0.1,0,0,0.5,"cm"),
          plot.title = element_text(size=28,family="Courier New",face="bold"))
  
  return(dollar)
}

##a currency v.s. dollar plots (from 2006 t0 2020)
nlist<-unique(data_plot$Country)

dollar_chart_column <-
  df_long %>%
  filter(Country %in% nlist & Year>2005) %>%
  group_by(Country,Symbol) %>%
  nest() %>%
  mutate(chart2 = purrr::map(data,chart2)
  ) %>%
  select(Country,Symbol,chart2)

##map function

map<-function (data){
  
  map<- ggplot()+
    geom_sf(world,mapping=aes(),fill="antiquewhite")+
    geom_sf(data,mapping=aes(fill="#f00a13"),color = "black",show.legend=FALSE)+
    labs(title = data$sovereignt)+
    theme(plot.title = element_text(size=28,family="Courier New",face="bold"),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.ticks.length = unit(0, "pt"), #length of tick marks
          legend.position = "none",
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank(), 
          plot.margin = unit(c(0,0,0,0),"mm"))+
    theme(panel.grid.major = element_line(color = gray(.1), linetype = "dashed", linewidth = 0.1), panel.background = element_rect(fill = "#F0F8FF"))+
    coord_sf(crs = 3035,expand=FALSE) + #crs: Coordinate Reference Systems 
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) 
  
  return(map)
}

country_map_column <-
  world %>%
  filter(sovereignt %in% nlist) %>%
  group_by(admin) %>%
  nest() %>%
  mutate(map = purrr::map(data,map)
  ) %>%
  select(admin,map)

colnames(country_map_column)<-c("Country","map")


##final data for the table
df<-left_join(crisis_chart_column,dollar_chart_column)
df<-left_join(df,country_map_column)

currency_list<-df_long %>% group_by(Country,Currency) %>% count() %>% select(-n)

df<-left_join(df,currency_list)

df$col1<-NA
df$col3<-NA
df$col4<-NA

df1<-df[,c("col1","Symbol","col3","col4")] 

#####################################################################make table
df1 %>%
  gt() %>%
  # assign values to each column
  text_transform(
    locations = cells_body(columns=col3),
    fn = function(x){
      purrr::map(
        df$chart, ggplot_image, height = px(100), aspect_ratio = 1.2
      )
    }
  ) %>%
  text_transform(
    locations = cells_body(columns=col4),
    fn = function(x){
      purrr::map(
        df$chart2, ggplot_image, height = px(100), aspect_ratio = 1.2
      )
    }
  ) %>%
  text_transform(
    locations = cells_body(columns=col1),
    fn = function(x){
      purrr::map(
        df$map, ggplot_image, height = px(100), aspect_ratio = 2.1
      )
    }
  ) %>%
  # Title and subtitle
  tab_header(
    title = "Currency Crisis",
    subtitle = md("The table shows the currency crisis around the world. Here, a currency crisis definition is a nominal depreciation of the currency by at least 30%  versus
                  the U.S Dollar in the first year, followed by at least a 5% decrease compared to the past year for the next five years. The third column of the table shows
                  the end-of-period of the year the crisis started. Data for the official nominal bilateral U.S Dollar exchange rate is from the Bank for International Settlements(BIS).")
  ) %>%
  # Table column names
   cols_label(
    col1 = '',
    Symbol = "Symbol",
    col3 = "Crisis Period",
    col4 =" Exchange Rate"
  )%>%
  
  # Style
  # Set columns size
  cols_width(
    col1 ~ px(250),
    Symbol ~ px(100),
    col3 ~ px(210),
    col4 ~px(210)
  )%>%
  # Source note
  tab_source_note(
    source_note = md("**Data:** BIS | **Visualization:** @HamidShaf | **Publication Date:** Dec 2022" )
  ) %>%
  # Title style
  tab_style(
    locations = cells_title(groups = 'title'),
    style = list(
      cell_text(
        font= 'Courier New', 
        size='xx-large',weight='bold',align='left',
        color='#d11141'
      )))%>%
  # Subtitle style
  tab_style(
    locations = cells_title(groups = 'subtitle'),
    style = list(
      cell_text(
        font= 'Courier New', 
        size='small',align='left'
      )))%>%
  # columns name style
  tab_style(
    style = list(
      cell_text(
        font="Courier New", 
        align = "center",v_align = "middle",size='medium',color='black',weight='bold')),
    locations = cells_column_labels(c(Symbol,col3,col4))
  )%>%
  tab_style(
    style = list(
      cell_text(
        font="Courier New", 
        align = "center",v_align = "middle",size='small',color='black')),
    locations = cells_column_labels(col1)
  )%>%
  tab_style(
    style = list(
      cell_text(font= "Courier New",align = 'center',weight='bold',
      )),
    locations = cells_body(columns = Symbol
    )
  )%>%
  # Add footnotes
  tab_footnote(
    footnote = md("The plots show how much of the U.S. Dollar each currency could buy from 2006 to 2020 each end-year-period."),
    locations = cells_column_labels(columns = col4)
  )%>%
  # Footnote style
  tab_style(
    style = list(
      cell_text(font="Courier New",size='small')),
    locations = cells_footnotes()
  )%>%
  # Source note
  tab_style(
    style = list(
      cell_text(font= "Courier New",size='small')),
    locations = cells_source_notes()
  ) %>%
  # General table options
  tab_options(
    data_row.padding = px(0),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table_body.hlines.style = "hidden",
    table_body.border.top.style = "solid",
    column_labels.border.bottom.style = "solid"
  ) %>%
  tab_options(table.background.color = "#FFF1E0") %>%
  
  gtsave("Currency Crisis.png")
