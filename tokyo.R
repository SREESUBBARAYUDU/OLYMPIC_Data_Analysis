library(rvest)
library(tidyverse)
library(lubridate)
library(eeptools)
library(stringi)

#main page 
html1 = read_html('http://www.olympedia.org/editions/medals')

#ids for modern olymic summer games
id = (html1 %>% html_elements('.table.table-striped a') %>% html_attr('href'))[299 : 332]

#creating url for individual games
url = paste('http://www.olympedia.org',id,sep = '')

#url for tokyo olympic 2020
tokyo = url[34]

#reading html file for tokyo
html_tokyo = read_html(url[34])

#full table for tokyo
data_Tokyo = (html_tokyo %>% html_table())[[1]]

View(data_Tokyo)



#trying to find the event names equating first and the second column

### First find the index of the main event name of each game
index = which(data_Tokyo$X1 == data_Tokyo$X2) 
### Then find the name of that event using those indices
tokyo_events = data_Tokyo$X1[index]



#arranging the event names to add to the dataframe, repeating each name suitable number of times
index_1 = c(index, nrow(data_Tokyo) + 1)


## Create a empty vector to repeat the name of the main events
events = c()
for(i in 1 : length(index)){
  events = c(events, rep(tokyo_events[i], diff(index_1)[i] - 1))
}

#rest of the dataframe containing the subevents details
tokyo_subevents = data_Tokyo[-c(1,index),]
View(tokyo_subevents)

#all countries which have won atleast one medal in any olympic
html_country = read_html('http://www.olympedia.org/statistics/medal/country')

## List all the country name with their corresponding codes
country_with_codes = (html_country %>% html_elements('td a') %>% html_text())

## Name of the countries
country = country_with_codes[seq(1,length(country_with_codes),2)]

# In the 2nd,4th and 6th column of tokyo_Subevents dataframe, some countries
# are followed by some numbers which needs to be removed
tokyo_subevents$X2 = gsub("[[:digit:]]", "", tokyo_subevents$X2) %>% str_trim()
tokyo_subevents$X4 = gsub("[[:digit:]]", "", tokyo_subevents$X4) %>% str_trim()
tokyo_subevents$X6 = gsub("[[:digit:]]", "", tokyo_subevents$X6) %>% str_trim()


# In 'tokyo subevents' dataframe we are looking into gold winners column
# if there is a country name, the event is a team event, otherwise individual event
genre = ifelse(is.na(match(tokyo_subevents$X2, country)) == T, 'individual', 'team')

#first attempt of dataframe to aggregate events, subevents and their types
data_1 = data.frame(events, tokyo_subevents$X1, genre)
# To give the name of each column name
colnames(data_1) = c('Events', 'Sub Events', 'Type')
View(data_1)

## Types of medals
medal = c('gold', 'silver', 'bronze')
data_2 = data.frame(data_1[rep(seq_len(nrow(data_1)), each = 3),], medal)
View(data_2)

#attempts to extract individual players name and corresponding countries
A <- tokyo_subevents[,c(2,4,6)] 
B <- as.data.frame(t(A))
html_medal <- (stack(B))$values
index1 <- which(data_2$Type == "team") ## Identify which are team events
html_medal[index1] = NA   ## Assign NA to the team events

## Extract the country codes from "tokyo_subevents" from the 3 rd 5 th and 7th column
html_country_code <- (tokyo_subevents[,c(3,5,7)] %>% t() %>% as.data.frame() %>% stack())$values

## Make a new data frame with html_medal and html_country_code
data_3 = data.frame(data_2,html_medal,html_country_code)
row.names(data_3) = 1 : nrow(data_3)
view(data_3)

#### Working for joint winners
# Identify which country_code contains 6 characters
joint = which(nchar(data_3$html_country_code) > 3)

# Copy all the columns of data_3 in data_4
data_4 = data_3

# Keep the first 3 characters for the 1st candidate of each joint winner
data_4$html_country_code[joint] = substring(data_3$html_country_code[joint], first = 1, last = 3)

View(data_4)

# Country_code for the second candidate of each joint winner
# joint_country_code = substring(data_3$html_country_code[joint], first = 4) 

l = nchar(data_3$html_country_code[joint])

joint_country_seperation = c()
for(i in 1 : length(joint)){
  joint_country_seperation = c(joint_country_seperation, 
                               stri_sub(data_3$html_country_code[joint[i]],
                                        from = seq(4, l[i], 3),
                                        to = seq(6, l[i], 3)))
}


#----------------------------------
#playing with the names!!

## Collect the unique letters
un_let = (strsplit(data_3$html_medal, split = '')) %>% unlist() %>% unique()
## Remove special characters from those set of unique letters
not_Spcl = c(" ", '-', "'",".",NA,'—')
## Collect only the special letters
spcl = un_let[! (un_let %in% c(letters,LETTERS,not_Spcl))]

## Split each name of the joint winner
words = strsplit(data_3$html_medal[joint], split = '')

# Create a empty vector
final_names_together = c()

## Add ";" between joint winners to split their names
for(i in 1 : length(words)){
  for(j in 1 : length(words[[i]])){
    if(words[[i]][j] %in% letters & words[[i]][j+1] %in% LETTERS | 
       words[[i]][j] %in% spcl & words[[i]][j+1] %in% LETTERS){
      words[[i]] = append(words[[i]], ';', after = j)
    }
  }
  final_names_together[i] = paste(words[[i]], collapse = '')  
}


final_names_together[final_names_together == 'NA'] = NA

## Add NA where NA appears
NA_index = which(is.na(final_names_together) == T)

if(length(NA_index) > 0){
  r <- nchar(data_3$html_country_code[joint[NA_index]])/3
  ## Split the names based on that ";"
  final_names_NA = c()
  for(i in 1:length(NA_index))
  {
    final_names_NA =  append(final_names_together, rep(NA,r[i]-1), after = NA_index[i])
    NA_index[i+1] = NA_index[i+1] + i
  }
  
  final_names = unlist(strsplit(final_names_NA,split = ";",fixed = TRUE))
}else{
  final_names = unlist(strsplit(final_names_together,split = ";",fixed = TRUE))
}


final_names[which(final_names == "Namara") - 1] = paste(final_names[which(final_names == "Namara") - 1], 'Namara',sep = '')
final_names = final_names[-which(final_names == "Namara")]

#----------------------------------------------------------
## Add the names properly

#creating a sequence to catch the first joint winner name
s = cumsum(c(1,l/3))
s = s[-length(s)] 

s1 = cumsum(c(1,l/3 - 1))
s1 = s1[-length(s1)]

data_4$html_medal[joint] = final_names[s]

## Copy the data of data_4 to data_5
data_5 = data_4

d = cumsum(diff(s)-1)

joint1 = joint

#appending new rows for joint winners
for (i in 1 : length(joint1)){
  for(j in 0 : (l[i] / 3 - 2)){
    data_5 = data_5 %>% add_row(Events = data_5$Events[joint1[i]],
                                Sub.Events = data_5$Sub.Events[joint1[i]],
                                Type = data_5$Type[joint1[i]],
                                medal = data_5$medal[joint1[i]],
                                html_medal = (final_names[s[i]+j+1]),
                                html_country_code = joint_country_seperation[s1[i] + j],
                                .after = joint1[i] + j)
  }
  joint1[i+1] = joint1[i+1] + d[i]
}


View(data_5)

#=================================================================

html_player = read_html(url[34])

## Find the url of each individual player
player_url_index = which((html_player %>% html_elements('td a') %>% html_attr('href') %>% substr(start = 1, stop = 4)) == '/ath')

player_url = paste('http://www.olympedia.org',(html_player %>% html_elements('td a') %>% html_attr('href'))[player_url_index],sep = '')

## Only extract from 1859 - 2020 summer games
game_name = (html1 %>% html_elements('td a') %>% html_text())[299 : 332]

## Create a empty vector years 
years = c()
## By default take each summer games date 01-01-years
for(i in 1 : length(game_name)){
  years[i] = paste(substring(game_name[i], nchar(game_name[i]) - 3), '-01-01', sep = '')
}
## Convert it as a  date format
years = as.Date(years)

gender = c()
height = 0
weight = 0
age = 0
name = c()

for(i in 1 : length(player_url)){
  print(player_url[i])
  html = read_html(player_url[i])
  info = as.data.frame((html %>% html_table())[[2]])
  
  name[i] = gsub('•', ' ',info[which(info$X1 == 'Used name'), 2])  ## Extract the names after removing "•"
  gender[i] = info[which(info$X1 == 'Sex'), 2]  ## Extract gender from table
  ## Assign NA to those for which measurements are not available
  if(length(unique(info$X1 == 'Measurements')) == 1){
    height[i] = NA
    weight[i] = NA
  }
  else{
    msm = info[which(info$X1 == 'Measurements'), 2]  ## First extract measurements from table
    height[i] = as.numeric(substr(msm,1,3)) ## Extract first 3 characters to find the height of that individual
    tmp = substring(msm, first = 10)  ## Remove upto first 10 characters from measurements
    weight[i] = as.numeric(substring(tmp, first = 1, last = nchar(tmp) - 3))  ## Extract only the digits of weight removing "kg"
  }
  
  bday = info[which(info$X1 == 'Born'), 2]  ## Extract the birthdays
  ## Some birthdyas contains "in .... " those birthdays are split based on that "in"
  if(grepl(' in ', bday) == 1){
    DOB = unlist(strsplit(bday,split = " in ",fixed = TRUE))[1] %>% dmy() ## Extract those birthdays and convert in it date_month_year format
  }
  ## some birthdays are given in mmyyyy format only
  else if(! substring(bday ,first = 1, last = 1) %in% as.character(0:9) == T){
    DOB = my(bday)
  }else{
    DOB = dmy(bday) ## Extract birthdays and convert in it date_month_year format
  }
  age[i] = round(age_calc(DOB, as.Date(years[length(years)]),units = 'years'))  ## Calculating the age of those individual
}

## CReate the data frame player_bio
player_bio = data.frame(name, gender, height, weight, age)

View(player_bio)
## Save the data player_bio
save(player_bio, file = 'player_bio.Rdata')

age_main = gender_main = height_main = weight_main = rep(0, nrow(data_5))

## If gender , height, weight not available then put NA or otherwise assign corresponding age,height,weight,gender
for(i in 1 : nrow(data_5)){
  if(is.na((data_5$html_medal[i])) == T){
    age_main[i] = gender_main[i] = height_main[i] = weight_main[i] = NA
  }
  else{
    ind = match(data_5$html_medal[i], player_bio$name)
    age_main[i] = player_bio$age[ind]
    height_main[i] = player_bio$height[ind]
    weight_main[i] = player_bio$weight[ind]
    gender_main[i] = player_bio$gender[ind]
  }
}


# FINALLY!!!

data_6 = data.frame(data_5, gender_main, age_main, height_main, weight_main)
colnames(data_6) = c('Events',
                     'Subevents',
                     'Type',
                     'Medal',
                     'Player',
                     'Country',
                     'Gender',
                     'Age',
                     'Height',
                     'Weight')

data_6[which(data_6 == '—', arr.ind = T)] = NA

View(data_6)



save(data_6, file = 'tokyo.Rdata')
