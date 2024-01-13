library(rvest)
library(tidyverse)
library(eeptools)
library(lubridate)
library(stringi)

#main page 
html1 = read_html('http://www.olympedia.org/editions/medals')

#ids for modern olymic summer games
id = (html1 %>% html_elements('.table.table-striped a') %>% html_attr('href'))[320 : 332]

rm_index <- c(8,10,12)

id <- id[-rm_index]

#creating url for individual games
url = paste('http://www.olympedia.org',id,sep = '')


#all countries which have won atleast one medal in any olympic
html_country = read_html('http://www.olympedia.org/statistics/medal/country')

## List all the country name with their corresponding codes
country_with_codes = (html_country %>% html_elements('td a') %>% html_text())

## Name of the countries
country = country_with_codes[seq(1,length(country_with_codes),2)]



Medal_winner_data <- function(url)
{
  html <- read_html(url)
  
  data_table <- (html %>% html_table())[[1]]
  
  #trying to find the event names equating first and the second column
  
  ### First find the index of the main event name of each game
  index = which(data_table$X1 == data_table$X2) 
  ### Then find the name of that event using those indices
  olympic_events = data_table$X1[index]
  
  
  
  #arranging the event names to add to the dataframe, repeating each name suitable number of times
  index_1 = c(index, nrow(data_table) + 1)
  
  
  ## Create a empty vector to repeat the name of the main events
  events = c()
  for(i in 1 : length(index)){
    events = c(events, rep(olympic_events[i], diff(index_1)[i] - 1))
  }
  
  #rest of the dataframe containing the subevents details
  olympic_subevents = data_table[-c(1,index),]
  #View(olympic_subevents)
  
  
  # In the 2nd,4th and 6th column of olympic_Subevents dataframe, some countries
  # are followed by some numbers which needs to be removed
  olympic_subevents$X2 = gsub("[[:digit:]]", "", olympic_subevents$X2) %>% str_trim()
  olympic_subevents$X4 = gsub("[[:digit:]]", "", olympic_subevents$X4) %>% str_trim()
  olympic_subevents$X6 = gsub("[[:digit:]]", "", olympic_subevents$X6) %>% str_trim()
  
  
  # In 'olympic subevents' dataframe we are looking into gold winners column
  # if there is a country name, the event is a team event, otherwise individual event
  genre = ifelse(is.na(match(olympic_subevents$X2, country)) == T, 'individual', 'team')
  
  #first attempt of dataframe to aggregate events, subevents and their types
  data_1 = data.frame(events, olympic_subevents$X1, genre)
  # To give the name of each column name
  colnames(data_1) = c('Events', 'Sub Events', 'Type')
  #View(data_1)
  
  ## Types of medals
  medal = c('gold', 'silver', 'bronze')
  data_2 = data.frame(data_1[rep(seq_len(nrow(data_1)), each = 3),], medal)
  #View(data_2)
  
  #attempts to extract individual players name and corresponding countries
  A <- olympic_subevents[,c(2,4,6)] 
  B <- as.data.frame(t(A))
  html_medal <- (stack(B))$values
  index1 <- which(data_2$Type == "team") ## Identify which are team events
  html_medal[index1] = NA   ## Assign NA to the team events
  
  ## Extract the country codes from "olympic_subevents" from the 3 rd 5 th and 7th column
  html_country_code <- (olympic_subevents[,c(3,5,7)] %>% t() %>% as.data.frame() %>% stack())$values
  
  ## Make a new data frame with html_medal and html_country_code
  data_3 = data.frame(data_2,html_medal,html_country_code)
  row.names(data_3) = 1 : nrow(data_3)
  
  #view(data_3)
  
  #### Working for joint winners
  # Identify which country_code contains 6 characters
  joint = which(nchar(data_3$html_country_code) > 3)
  
  # Copy all the columns of data_3 in data_4
  data_4 = data_3
  
  # Keep the first 3 characters for the 1st candidate of each joint winner
  data_4$html_country_code[joint] = substring(data_3$html_country_code[joint], first = 1, last = 3)
  
  #View(data_4)
  
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
  not_Spcl = c(" ", '-', "'",NA,'—')
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
    i = 3
    for(i in 1:length(NA_index))
    {
      final_names_together =  append(final_names_together, rep(NA,r[i]-1), after = NA_index[i])
      NA_index[i+1] = NA_index[i+1] + i
    }
    
    final_names = unlist(strsplit(final_names_together,split = ";",fixed = TRUE))
  }else{
    final_names = unlist(strsplit(final_names_together,split = ";",fixed = TRUE))
  }
  
  if(length(which(final_names == "Namara"))>0){
    final_names[which(final_names == "Namara") - 1] = paste(final_names[which(final_names == "Namara") - 1], 'Namara',sep = '')
    final_names = final_names[-which(final_names == "Namara")]
  }
  
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

  html_player = read_html(url)
  
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
  
  ## Create the data frame player_bio
  player_bio = data.frame(name, gender, height, weight, age)
  
  #View(player_bio)
  ## Save the data player_bio
  #save(player_bio, file = 'player_bio.Rdata')
  
  age_main = gender_main = height_main = weight_main = rep(0, nrow(data_5))
  i = 212
  ## If gender , height, weight not available then put NA or otherwise assign corresponding age,height,weight,gender
  for(i in 1 : nrow(data_5)){
    if(is.na((data_5$html_medal[i])) == T){
      age_main[i] = gender_main[i] = height_main[i] = weight_main[i] = NA
    }else{
      ind = match(data_5$html_medal[i], player_bio$name)
      age_main[i] = player_bio$age[ind]
      height_main[i] = player_bio$height[ind]
      weight_main[i] = player_bio$weight[ind]
      gender_main[i] = player_bio$gender[ind]
    }
  }
  
  
  # FINALLY!!!
  
  data_6 = data.frame(data_5, age_main, gender_main, height_main, weight_main)
  colnames(data_6) = c('Events',
                       'Subevents',
                       'Type',
                       'Medal',
                       'Player',
                       'Country',
                       'Age',
                       'Gender',
                       'Height',
                       'Weight')
  data_6[which(data_6 == '—', arr.ind = T)] = NA
  return(data_6)
}


los_Angeles_1984 <- Medal_winner_data(url[1])
Seoul_1988 <- Medal_winner_data(url[2])
Barcelona_1992 <- Medal_winner_data(url[3])
Atlanta_1996 <- Medal_winner_data(url[4])
Sydney_2000 <- Medal_winner_data(url[5])
Athina_2004 <- Medal_winner_data(url[6])
Beijing_2008 <- Medal_winner_data(url[7])
London_2012 <- Medal_winner_data(url[8])
Rio_de_janeiro_2016 <- Medal_winner_data(url[9])
Tokyo_2020 <- Medal_winner_data(url[10])


save(London_2012, file =  '2012.Rdata')
save(Rio_de_janeiro_2016, file = '2016.Rdata')
save(Athina_2004, file = '2004.Rdata')
save(Sydney_2000, file = '2000.Rdata')
save(los_Angeles_1984, file = '1984.Rdata')
save(Atlanta_1996, file = '1996.Rdata')
save(Seoul_1988, file = '1988.Rdata')
save(Barcelona_1992, file = '1992.Rdata')
save(Tokyo_2020, file = '2020.Rdata')
save(Beijing_2008, file = '2008.Rdata')


all_data <- list(Los_Angeles_1984,
                 Seoul_1988,
                 Barcelona_1992,
                 Atlanta_1996,
                 Sydney_2000,
                 Athina_2004,
                 Beijing_2008,
                 London_2012,
                 Rio_de_janeiro_2016,
                 Tokyo_2020)
names(all_data) <- c("Los_Angeles_1984",
                     "Seoul_1988",
                     'Barcelona_1992',
                     'Atlanta_1996',
                     'Sydney_2000',
                     'Athina_2004',
                     'Beijing_2008',
                     'London_2012',
                     'Rio_de_janeiro_2016',
                     'Tokyo_2020')
