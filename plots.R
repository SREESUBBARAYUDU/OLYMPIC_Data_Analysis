library(ggplot2)
library(dplyr)

## Medal Distribution for diff countries over the year on world map
mapdata = map_data("world")
load('GDP_final_data.Rdata')
GDP_data_updated = GDP_data
GDP_data_updated$Country_Name[which(GDP_data_updated$Country_Name == 'United States')] = 'USA'  
GDP_data_updated$Country_Name[which(GDP_data_updated$Country_Name == 'Great Britain')] = 'UK'

map_ind = match(mapdata$region, GDP_data_updated[GDP_data_updated$Year == 2016,]$Country_Name)

mapdata$map_medal = GDP_data_updated[GDP_data_updated$Year == 2016,]$Medals[map_ind] 




ggplot(mapdata, aes(long, lat, fill = map_medal, group = group))+
  geom_polygon(colour = 'gray') + 
  scale_fill_continuous(low = "#f2d340",
                        high = "#6c1615",
                        guide = "colorbar")+
  labs(fill = 'No. of medals',
       title = 'Medal distribution of countries (Rio 2016)')



## Plot of medal distribution in different events

load('all_data_list.Rdata')
g <- ggplot(all_data[[10]], aes(Events)) + scale_fill_brewer(palette = "Spectral")

g + geom_histogram(aes(fill=Medal), 
                   stat = "count",
                   col="black", 
                   size=.1)+
  labs(y = "Medal",
       x='') + 
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))+
  labs(title = 'Plot of medal distribution in different events (Tokyo 2020)')


## Plot of gender distribution among medal winners in different events 

g_hist <- ggplot(all_data[[9]][all_data[[9]]$Type == 'individual' & is.na(all_data[[9]]$Gender) == F,], aes(Events)) + scale_fill_brewer(palette = "Spectral")

g_hist + geom_histogram(aes(fill=Gender), 
                   binwidth = .1,
                   stat = "count",
                   col="black", 
                   size=.1)+
  labs(y = "count",
       x ='',
       title = 'Plot of gender distribution among medal winners in different events (Rio 2016)') + 
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))


## Pair Plot of association among age, height and weight of medal winners
library(GGally)
load('full_df.Rdata')
ggpairs(data1, columns = c("Age", "Height", "Weight"), mapping=aes(colour=Medal, alpha = 0.7))+
  labs(title = 'Plot of association among age, height and weight of medal winners in Olympics')




library("readr")
dataOlympics <- read_csv("athleteEvents.csv", col_types = cols(
  ID = col_character(),
  Name = col_character(),
  Sex = col_factor(levels = c("M","F")),
  Age =  col_integer(),
  Height = col_double(),
  Weight = col_double(),
  Team = col_character(),
  NOC = col_character(),
  Games = col_character(),
  Year = col_integer(),
  Season = col_factor(levels = c("Summer","Winter")),
  City = col_character(),
  Sport = col_character(),
  Event = col_character(),
  Medal = col_factor(levels = c("Gold","Silver","Bronze"))
)
)



numbers = dataOlympics[dataOlympics$Year > 1983 & dataOlympics$Year < 2020 & dataOlympics$Season == 'Summer' & is.na(dataOlympics$Medal) == F,]

f = function(x){
  mean(x,na.rm = T)
}
summer_age = tapply((numbers$Age),numbers$Sport,f)
summer_age = sort(summer_age, decreasing = TRUE)

## Dotchart for average age of an athlete
dotchart(summer_age, pch = 21, bg = "purple1",
         xlab="Average age of athletes",
         main = "What is the average age of an athlete \nwinning medals in the summer Olympics?",
         cex = 0.6,
         pt.cex = 1.2,
         cex.main = 1.7)


## Sex-Ratio in Olympics over the years 1984-2020
gender_distn = data1 %>% 
  group_by(year, Events,Gender) %>% 
  summarize(count = length(Medal),.groups = 'drop') %>% 
  na.omit() %>% 
  tidyr::pivot_wider(names_from = Gender, 
                     values_from = count) 

gender_distn$Female[is.na(gender_distn$Female) == T] = 0
gender_distn$Male[is.na(gender_distn$Male) == T] = 0

gender_distn_final = gender_distn%>% 
  mutate(Ratio = Female/Male)

ggplot(gender_distn_final[gender_distn_final$Events == 'Wrestling',],aes(year,Ratio))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = seq(1984,2020,4))+
  ylim(0,1)+
  labs(title = "Wrestling",
       y = 'Sex Ratio (F/M)')

## 'Plot of number of medal winners vs GDP of a country
load('GDP_final_data.Rdata')
new_dat1 <- subset(GDP_data, GDP_data$Year == 2020)
ggplot(new_dat1,aes(x = GDP,y = Medals, colour = Country))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  theme(legend.position = 'none')+
  labs(title = 'Tokyo 2020')


## Boxplot of Age distribution

d1 = numbers[numbers$Sport == 'Equestrianism'|
               numbers$Sport == 'Shooting'|
               numbers$Sport == 'Golf',]

ggplot(d1)+
  geom_boxplot(aes(x = Sport, y = Age, fill = Sport))+
  labs(title = 'Boxplot of Age distribution',
       subtitle = 'Events having higher median age')


## Boxplot of BMI (kg/m2) distribution
library(dplyr)
numbers = numbers %>% 
  mutate(BMI = Weight/(Height/100)^2)
summer_BMI = tapply((numbers$BMI),numbers$Sport,f)
summer_BMI = sort(summer_BMI, decreasing = TRUE)
d3 = numbers[numbers$Sport == 'Rhythmic Gymnastics'|
               numbers$Sport == 'Weightlifting',]

ggplot(d3)+
  geom_boxplot(aes(x = Sport, y = BMI, fill = Sport))+
  labs(title = 'Boxplot of BMI (kg/m2) distribution',
       subtitle = 'For events having Extreme average BMI ')+
  ylim(10,50)



new_dat <- subset(data1, data1$Events == 'Athletics')


dom = names(sort(table(new_dat$Country), decreasing = T)[1:5])
dom

data3 = new_dat[new_dat$Country %in% dom == T,][c('year', 'Events', 'Country')]

data4 = data3 %>% group_by(year, Country) %>% 
  summarise(count = n())


#(head(data4))

##Plot of total number of medals won by top 5 dominating countries \n Athletics 1984-2020

ggplot(data4, aes(year, count, col = Country))+
  geom_line(size = 1)+
  geom_point(size = 2)+
  labs(title = 'Plot of total number of medals won by top 5 dominating countries \n Athletics (1984-2020)')

