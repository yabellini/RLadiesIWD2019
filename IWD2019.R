# -----
# Link I used
# https://ropensci.org/blog/2016/08/23/z-magick-release/
# https://ropensci.org/tutorials/magick_tutorial/
# https://d4tagirl.com/2017/05/how-to-fetch-twitter-users-with-r  
# https://www.gastonsanchez.com/r4strings/stringr-basics.html


  
library(readxl)
library(magick)
library(purrr)
library(dplyr)
library(tidyr)
#library(ggmap)
#library(nominatim)
library(ggplot2)
library(maps)
library(ggthemes)
library(stringr)

#devtools::install_github("hrbrmstr/nominatim")

# armar_capitulo <- function(texto, template, locationRL, colorRL) {
#   
#   image_annotate(template, texto, font = 'helvetica', size = 30, location = location, color = colorRL)
#   
#   return(list(template, location))
# }

# imprimir <- function(ds) {
#   CC <- ds %>% select(4:13)
#   
#   # for (i in colnames(CC)){
#     #Verificar si tiene datos
#   
#     place=place+30
#     
#     
#     template <- image_annotate(template, , font = 'helvetica', size = 20, location = paste ("+90+", as.character(place)), color = "purple")
#     
#   }
#   
# }
  

#CCRL <- read_excel("D:/Rladies/IWD019/IWD2019/CCRL.xlsx")
CCRL <- read_excel("D:/Rladies/IWD019/IWD2019/CCRL2.xlsx")

#I have to geposition location for chapter outside R because google ask for a API key now and I have to give my credit card number
#and I do not want to do that ...jeje
#The code for doind that task in R is in the post for Dani Vazquez

LatLong <- read_excel("D:/Rladies/IWD019/IWD2019/LatLong.xlsx")

CCRL <- CCRL %>% inner_join(LatLong, by = "City")

#Filter Chapters without Data
SinData <- CCRL %>% 
  filter (is.na(CCRL$Meetup)& is.na(CCRL$Twitter) & is.na(CCRL$Email) & is.na(CCRL$Facebook) &
            is.na(CCRL$Instagram) & is.na(CCRL$Periscope) & is.na(CCRL$Youtube) & is.na(CCRL$GitHub) &
            is.na(CCRL$Website) & is.na(CCRL$Slack))


#Filter Chapters with Data
CCRL <- CCRL %>% 
  filter(CCRL$Country !='Remote') %>%
  anti_join(SinData, by='City') 
  

#Se repite para cada fila del Set de datos
#Se debe iterar por cada columna sumando 30-40 puntos desde la anterior, solo en caso que tenga
#datos imprima una linea
#Se genera el mapa resaltando el lugar del capítulo en el mundo

#For loop for every row in Current Chapter
#Add 30-40 pixel for each column only in case that have data, have to choose the rigth logo
#Make a map were highlight the place of the Chapter in the world

#I could not find a way to avoid the for loop

for (i in 1:nrow(CCRL)) { #Line for all the chapters
#for (i in 2:10) {  #Line for testing
  template <- image_read("D:/Rladies/IWD019/IWD2019/RLTemplate.png")

  #Add the chapter map
  
  world <- ggplot() +
    borders("world", colour = "gray85", fill = "gray80") +
    theme_map()
  
  
  if(!is.na(CCRL$Lat[i])){
    map <- world +
      geom_point(aes(x = Longitude, y = Latitude, size = 1),
                 data = CCRL, colour = 'black', alpha = .5) +
      geom_point(aes(x = Longitude[i], y = Latitude[i], size = 10),
                 data= CCRL, colour= 'purple', alpha = .5) +
      geom_point(aes(x = Lon[i], y = Lat[i], size = 10),
                 data= CCRL, colour= 'purple', alpha = .5) +
      theme(legend.position="none")
    
    print('Hice el mapa con dos ciudades')
    
  } else {
    map <- world +
      geom_point(aes(x = Longitude, y = Latitude, size = 1),
                 data = CCRL, colour = 'black', alpha = .5) +
      geom_point(aes(x = Longitude[i], y = Latitude[i], size = 10),
                 data= CCRL, colour= 'purple', alpha = .5) +
      theme(legend.position="none")
    
    print('hice el mapa con una ciudad')
  }
  
  ggsave("map.png", width = 7, height = 4)
  
  map <- image_read("D:/Rladies/IWD019/IWD2019/map.png")
  
  place = 230
  
  template <- image_composite(template, image_scale(map, 'x200'), offset = paste("+620+", as.character(place)))  
  
  print('Grabe el mapa')
  
  #Ad the rest of the info
  place=190  
  
  template <- image_annotate(template, paste("Chapter R-Ladies",CCRL$City[i],'in',CCRL$Country[i]), font = 'helvetica', size = 32, location = "+50+20") %>%
    image_annotate( "Organized by:", font = 'helvetica', size = 22, location = "+50+65", color = "black") %>%
    image_annotate("Contact information:", font = 'helvetica', size = 22, location = "+50+190", color = "black") %>%
    image_annotate("Location:", font = 'helvetica', size = 22, location = "+640+190", color = "black")

  org <-str_wrap(CCRL$Organizers[i], width = 70)
  
  template <- image_annotate(template, org, font = 'helvetica', size = 25, location = "+50+90", color = "black")
  
  print('Grabe los titulos')
  
  if (!is.na(CCRL$Twitter[i])){ 
    place=place+40
    pic <- image_read("D:/Rladies/IWD019/IWD2019/twitter.png")
    template <- image_composite(template, image_scale(pic, "x30"), offset = paste("+50+", as.character(place)))
    template <- image_annotate(template, CCRL$Twitter[i] , font = 'helvetica', size = 20, location = paste ("+95+", as.character(place)), color = "purple")
    print('Grabe Twitter')
  }

  
  
  if (!is.na(CCRL$Meetup[i])){ 
    place=place+40
    pic <- image_read("D:/Rladies/IWD019/IWD2019/meetup.png")
    template <- image_composite(template, image_scale(pic, "x30"), offset = paste("+50+", as.character(place)))
    template <- image_annotate(template, CCRL$Meetup[i] , font = 'helvetica', size = 20, location = paste ("+95+", as.character(place)), color = "purple")
    print('Grabe Meetup')
  }
  
  
    
  if (!is.na(CCRL$Email[i])){ 
    place=place+40
    pic <- image_read("D:/Rladies/IWD019/IWD2019/mail.png")
    template <- image_composite(template, image_scale(pic, "x30"), offset = paste("+50+", as.character(place)))
    template <- image_annotate(template, CCRL$Email[i] , font = 'helvetica', size = 20, location = paste ("+95+", as.character(place)), color = "purple")
    print('Grabe Mail')
  }

  
  if (!is.na(CCRL$Facebook[i])){
    place=place+40
    pic <- image_read("D:/Rladies/IWD019/IWD2019/facebook.png")
    template <- image_composite(template, image_scale(pic, "x30"), offset = paste("+50+", as.character(place)))

    template <- image_annotate(template, CCRL$Facebook[i] , font = 'helvetica', size = 20, location = paste ("+95+", as.character(place)), color = "purple")
  
    print('Grabe Facebook')
  }
  
  if (!is.na(CCRL$Instagram[i])){
    place=place+40
    pic <- image_read("D:/Rladies/IWD019/IWD2019/instagram.png")
    template <- image_composite(template, image_scale(pic, "x30"), offset = paste("+50+", as.character(place)))
    
    template <- image_annotate(template, CCRL$Instagram[i] , font = 'helvetica', size = 20, location = paste ("+95+", as.character(place)), color = "purple")
    print('Grabe Instagram')
  }
  
  if (!is.na(CCRL$Periscope[i])){
    place=place+40
    
    pic <- image_read("D:/Rladies/IWD019/IWD2019/periscope.png")
    template <- image_composite(template, image_scale(pic, "x30"), offset = paste("+50+", as.character(place)))
    
    template <- image_annotate(template, CCRL$Periscope[i] , font = 'helvetica', size = 20, location = paste ("+95+", as.character(place)), color = "purple")
    print('Grabe periscope')
  }
  
  if (!is.na(CCRL$Youtube[i])){
    place=place+40
    pic <- image_read("D:/Rladies/IWD019/IWD2019/youtube.png")
    template <- image_composite(template, image_scale(pic, "x30"), offset = paste("+50+", as.character(place)))
    
    template <- image_annotate(template, CCRL$Youtube[i] , font = 'helvetica', size = 20, location = paste ("+95+", as.character(place)), color = "purple")
    print('Grabe youtube')
  }
  
  if (!is.na(CCRL$GitHub[i])){
    place=place+40
    
    pic <- image_read("D:/Rladies/IWD019/IWD2019/github.png")
    template <- image_composite(template, image_scale(pic, "x30"), offset = paste("+50+", as.character(place)))
    
    template <- image_annotate(template, CCRL$GitHub[i] , font = 'helvetica', size = 20, location = paste ("+95+", as.character(place)), color = "purple")
    print('Grabe github')
  }
  
  if (!is.na(CCRL$Website[i])){
    place=place+40
    
    pic <- image_read("D:/Rladies/IWD019/IWD2019/web.png")
    template <- image_composite(template, image_scale(pic, "x30"), offset = paste("+50+", as.character(place)))
    
    template <- image_annotate(template, CCRL$Website[i] , font = 'helvetica', size = 20, location = paste ("+95+", as.character(place)), color = "purple")
    print('Grabe website')
  }
  
  if (!is.na(CCRL$Slack[i])){
    place=place+40
    
    pic <- image_read("D:/Rladies/IWD019/IWD2019/slack.png")
    template <- image_composite(template, image_scale(pic, "x30"), offset = paste("+50+", as.character(place)))
    
    
    template <- image_annotate(template, CCRL$Slack[i] , font = 'helvetica', size = 20, location = paste ("+95+", as.character(place)), color = "purple")
    print('Grabe slack')
  }


  #Add Founders
   if (!is.na(CCRL$Founders[i])){
     place=place+40
     
     template <- image_annotate(template, "Founded by:", font = 'helvetica', size = 22, location = paste ("+50+", as.character(place)), color = "black") 
     
     place=place+30
     
     template <- image_annotate(template, CCRL$Founders[i], font = 'helvetica', size = 25, location = paste ("+50+", as.character(place)), color = "black")
     
     print('Grabe los fundadores')
   }
  
  print('Estoy por grabar la ficha')
  #Save the final picture
  image_write(template, paste("Chapter",str_replace_all(CCRL$City[i], fixed(" "), ""),".png"), format= "png")
  
  print('Grabe la ficha')
}

# Generar la figura para Remote


#Armado de las frases para los capitulos con datos
#Base code from Maëlle and the post in R-Ladies blog

# templates

templates <- c("Did you know that there is an adjective chapter in X? They’d love for you to visit!",
               
               "Hop over and join this adjective group of #rladies in X!",
               
               "Do you like #rstats? Looking for like-minded people? Come to have fun with us in X!",
               
               "Do you like #rstats? Looking to learn more? Come to share with us in X!",
               
               "Celebrate woman’s day with our adjective chapter in X",
               
               "Looking for something to inspire you? Meet our adjective chapter in X",
               
               "Learn all about what this adjective chapter X is up to this Women’s day!",
               
               "We are so proud of our adjective chapter in X!",
               
               "Nourish your mind today! Join adjective chapter X!",
               
               "We are privileged to have adjective chapter X in our network!",
               
               "We are privileged to feature adjective chapter X this women’s day!")


Chapter_adjectives <- c("awesome", "fantastic", "wonderful", "amazing", "brilliant", "phenomenal", "remarkable", "talented", "incredible", "top-notch", "magnificent", "marvelous", "insightful", "capable", "admirable", "outstanding", "splendid", "exceptional")


# get as many sentences as there are chapters
combinations <- expand.grid(templates, Chapter_adjectives)
combinations <- dplyr::mutate_all(combinations, as.character)

create_sentence <- function(template, adjective){
  stringr::str_replace(template, "adjective", adjective)
}

sentences <- purrr::map2_chr(combinations$Var1, combinations$Var2,
                             create_sentence)
set.seed(42)
# the first one is chosen, this way it's not "another" or "one more"
sentences <- c(sentences[1],
               sample(sentences, nrow(CCRL) - 1, replace = TRUE))

# add actual tweet text
CCRLTw <- dplyr::mutate(CCRL,
                        tweet = stringr::str_replace(sentences,"X",CCRL$City),
                        tweet = ifelse(!is.na(CCRL$Meetup),
                                       paste0(tweet, " Meetups here: ", CCRL$Meetup), tweet),
                        tweet = paste(tweet, "#rladies #iwd2018"),
                        picture=paste("Chapter",str_replace_all(CCRL$City, fixed(" "), ""),".png"))

# save tweets
tweets <- dplyr::select(CCRLTw, City, tweet, picture) %>%
  arrange(City)
Encoding(tweets$City) <- "UTF-8"
Encoding(tweets$tweet) <- "UTF-8"
readr::write_csv(tweets, path = "ready_tweets.csv")

