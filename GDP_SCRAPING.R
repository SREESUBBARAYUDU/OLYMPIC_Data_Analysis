library(tidyverse)
library(rvest)


## Make the all_dat list
all_data = list(los_Angeles_1984,
                Seoul_1988,
                Barcelona_1992,
                Atlanta_1996,
                Sydney_2000,
                Athina_2004,
                Beijing_2008,
                London_2012,
                Rio_de_janeiro_2016,
                Tokyo_2020)

save(all_data,file = "all_data_list.Rdata")
GDP_data = data.frame(NULL)
l = 0
for(i in 1 : 10){
  GDP_data = rbind(GDP_data, table(all_data[[i]]$Country) %>% as.data.frame())
  l[i] = all_data[[i]]$Country %>% table() %>% length()
}

## Make the list of the years
year_rep = seq(1984,2020,4) %>% rep(times = l)
## Make the GDP dataframe
GDP_data = data.frame(year = year_rep, GDP_data)
colnames(GDP_data) = c('Year', 'Country', 'Medals')


 # Go to the gDP website
A = read_html("https://www.macrotrends.net/countries/ranking/gdp-gross-domestic-product")
## Extract the country codes
B  <- substr(A %>% html_elements('td a'),21,24)
## Extract the url's for each country
C <-  A %>% html_elements('td a') %>% html_attr('href')
## Find the country names
Countries <- A %>% html_elements('td a') %>% html_text()
## Make the final url's
url = paste('https://www.macrotrends.net',C,sep = "")

## loop to collect data of each url
GDP_countries = list(NULL)
for(i in 1:length(url))
{
  paste('url',i) %>% print()
  tmp = ((read_html(url[i]) %>% html_table())[[2]])[,c(1,2)]
  colnames(tmp) = c('year', 'gdp')
  tmp2 = tmp[tmp$year == 2020|
               tmp$year == 2016| 
               tmp$year == 2012|
               tmp$year == 2008|
               tmp$year == 2004|
               tmp$year == 2000|
               tmp$year == 1996|
               tmp$year == 1992|
               tmp$year == 1988|
               tmp$year == 1984,]
  GDP_countries[[i]] = tmp2
  
}
GDP_Country_code = gsub("/",'',B)



for(i in 1 : length(GDP_countries)){
  GDP_countries[[i]] = as.data.frame(GDP_countries[[i]])
  GDP_countries[[i]][,2] = (GDP_countries[[i]])[,2] %>% str_remove_all(pattern = '[$,B]')
  for(j in 1:2){
    GDP_countries[[i]][,j] = GDP_countries[[i]][,j] %>% as.numeric()
  }
}
## Take the code of each country from wiki and then match them with the GDP country code
R = (read_html("https://en.wikipedia.org/wiki/List_of_IOC_country_codes") %>% html_table())[[1]][,c(1,2)]

## Made some changes
R$Code[1] = "AFG"

L = names(GDP_countries)

M = Countries[which(Countries == "United Kingdom")] = "Great Britain"
ind = match( Countries , R$`National Olympic Committee`)

for(i in 1 : length(GDP_countries)){
  if(is.na(ind[i]) == FALSE)
    GDP_Country_code[i] =  (R$Code)[ind[i]]
}

names(GDP_countries) = GDP_Country_code
save(GDP_countries,file = "GDP_COUNTRY_CODE.Rdata")

GDP = NA
ind2 = NA

## Find the matches
ind1 =  match(GDP_data$Country, names(GDP_countries)) 

for(i in 1 : nrow(GDP_data)){
  ind2[i] = match(GDP_data$Year[i],GDP_countries[[ind1[i]]]$year)
  if(is.na(ind2[i])==F){
    GDP[i] = (GDP_countries[[ind1[i]]]$gdp)[ind2[i]]
  }
}
## Final GDP_data and save it

GDP_data = data.frame(GDP_data,GDP = c(GDP,NA))

save(GDP_data, file = 'GDP_final.Rdata')


mapdata = map_data("world")
# View(mapdata)

save(GDP_data,file = "GDP_final_data.Rdata")

map_list = list(NULL)
year = seq(1984, 2020, 4)


GDP_data_updated = GDP_data


GDP_data_updated$Country_Name[which(GDP_data_updated$Country_Name == 'United States')] = 'USA'  
GDP_data_updated$Country_Name[which(GDP_data_updated$Country_Name == 'Great Britain')] = 'UK'  


for(i in 1 : 10){
  map_ind = match(mapdata$region, GDP_data_updated[GDP_data_updated$Year == year[i],]$Country_Name)
  
  mapdata$map_medal = GDP_data_updated[GDP_data_updated$Year == year[i],]$Medals[map_ind] 
  
  map_list[[i]] = mapdata
}


ggplot(map_list[[1]], aes(long, lat, fill = map_medal, group = group))+
  geom_polygon(colour = 'gray') + 
  scale_fill_continuous(low = "#f2d340",
                        high = "#6c1615",
                        guide = "colorbar")
