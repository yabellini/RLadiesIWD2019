-----
# Link I used
# https://ropensci.org/blog/2016/08/23/z-magick-release/
# https://ropensci.org/tutorials/magick_tutorial/
# https://d4tagirl.com/2017/05/how-to-fetch-twitter-users-with-r  

  
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
#     template <- image_annotate(template, , font = 'helvetica', size = 20, location = paste ("+90+", as.character(place)), color = "pink")
#     
#   }
#   
# }
  

CCRL <- read_excel("D:/Rladies/IWD019/IWD2019/CCRL.xlsx")

#I have to geposition location for chapter outside R because google ask for a API key now and I have to give my credit card number
#and I do not want to do that ...jeje
#The code for doind that task in R is in the post for Dani Vazquez


LatLong <- read_excel("D:/Rladies/IWD019/IWD2019/LatLong.xlsx")

CCRL <- CCRL %>% inner_join(LatLong)

#Mapa general de Capítulos
#Map for all the chapters

# world <- ggplot() +
#   borders("world", colour = "gray85", fill = "gray80") +
#   theme_map()
# map <- world +
#   geom_point(aes(x = Longitude, y = Latitude, size = 1),
#              data = CCRL, colour = 'black', alpha = .5) +
#   geom_point(aes(x = Longitude[2], y = Latitude[2], size = 10),
#              data= CCRL, colour= 'purple', alpha = .5)
# 
# ggsave("map.png", width = 7, height = 4)

#template <- image_read("D:/Rladies/IWD019/IWD2019/Template.png")


#Se repite para cada fila del Set de datos
#RGB: 136, 57, 138
#Se debe iterar por cada columna sumando 30-40 puntos desde la anterior, solo en caso que tenga
#datos imprima una linea
#Se genera el mapa resaltando el lugar del capítulo en el mundo

#For loop for every row in Current Chapter
#Add 30-40 pixel for each column only in case that have data, have to choose the rigth logo
#Make a map were highlight the place of the Chapter in the world
#I could not find a way to avoid the for loop

#for (i in 1:nrow(CCRL)) { #Line for all the chapters
for (i in 2:5) {  #Line for testing
  template <- image_read("D:/Rladies/IWD019/IWD2019/RLTemplate.png")
  place=200  
  
  template <- image_annotate(template, paste("Chapter R-Ladies",CCRL$City[i],'in',CCRL$Country[i]), font = 'helvetica', size = 40, location = "+50+50") %>%
    image_annotate( "Organized by:", font = 'helvetica', size = 25, location = "+50+120", color = "black") %>%
    image_annotate(CCRL$Organizers[i], font = 'helvetica', size = 30, location = "+50+150", color = "black") %>%
    image_annotate("Ways to contact", font = 'helvetica', size = 25, location = "+50+200", color = "black")


  if (!is.na(CCRL$Email[i])){ 
    place=place+40
    pic <- image_read("D:/Rladies/IWD019/IWD2019/mail.png")
    template <- image_composite(template, image_scale(pic, "x30"), offset = paste("+50+", as.character(place)))
    template <- image_annotate(template, CCRL$Email[i] , font = 'helvetica', size = 20, location = paste ("+135+", as.character(place)), color = "pink")
  }

  
  if (!is.na(CCRL$Facebook[i])){
    place=place+40
    pic <- image_read("D:/Rladies/IWD019/IWD2019/facebook.png")
    template <- image_composite(template, image_scale(pic, "x30"), offset = paste("+50+", as.character(place)))

    template <- image_annotate(template, CCRL$Facebook[i] , font = 'helvetica', size = 20, location = paste ("+135+", as.character(place)), color = "pink")
  
  }
  
  if (!is.na(CCRL$Instagram[i])){
    place=place+40
    pic <- image_read("D:/Rladies/IWD019/IWD2019/instagram.png")
    template <- image_composite(template, image_scale(pic, "x30"), offset = paste("+50+", as.character(place)))
    
    template <- image_annotate(template, CCRL$Instagram[i] , font = 'helvetica', size = 20, location = paste ("+135+", as.character(place)), color = "pink")
    
  }
  
  if (!is.na(CCRL$Periscope[i])){
    place=place+40
    template <- image_annotate(template, CCRL$Periscope[i] , font = 'helvetica', size = 20, location = paste ("+135+", as.character(place)), color = "pink")
    
  }
  
  if (!is.na(CCRL$Youtube[i])){
    place=place+40
    pic <- image_read("D:/Rladies/IWD019/IWD2019/youtube.png")
    template <- image_composite(template, image_scale(pic, "x30"), offset = paste("+50+", as.character(place)))
    
    template <- image_annotate(template, CCRL$Youtube[i] , font = 'helvetica', size = 20, location = paste ("+135+", as.character(place)), color = "pink")
    
  }
  
  if (!is.na(CCRL$GitHub[i])){
    place=place+40
    
    pic <- image_read("D:/Rladies/IWD019/IWD2019/github.png")
    template <- image_composite(template, image_scale(pic, "x30"), offset = paste("+50+", as.character(place)))
    
    template <- image_annotate(template, CCRL$GitHub[i] , font = 'helvetica', size = 20, location = paste ("+135+", as.character(place)), color = "pink")
    
  }
  
  if (!is.na(CCRL$Website[i])){
    place=place+40
    
    pic <- image_read("D:/Rladies/IWD019/IWD2019/web.png")
    template <- image_composite(template, image_scale(pic, "x30"), offset = paste("+50+", as.character(place)))
    
    template <- image_annotate(template, CCRL$Website[i] , font = 'helvetica', size = 20, location = paste ("+135+", as.character(place)), color = "pink")
    
  }
  
  if (!is.na(CCRL$Slack[i])){
    place=place+40
    
    pic <- image_read("D:/Rladies/IWD019/IWD2019/slack.png")
    template <- image_composite(template, image_scale(pic, "x30"), offset = paste("+50+", as.character(place)))
    
    
    template <- image_annotate(template, CCRL$Slack[i] , font = 'helvetica', size = 20, location = paste ("+135+", as.character(place)), color = "pink")
    
  }

  #Add the chapter map
  
   world <- ggplot() +
     borders("world", colour = "gray85", fill = "gray80") +
     theme_map()
   map <- world +
     geom_point(aes(x = Longitude, y = Latitude, size = 1),
                data = CCRL, colour = 'black', alpha = .5) +
     geom_point(aes(x = Longitude[i], y = Latitude[i], size = 10),
                data= CCRL, colour= 'purple', alpha = .5)
   
   ggsave("map.png", width = 7, height = 4)
  
   map <- image_read("D:/Rladies/IWD019/IWD2019/map.png")

   place = 400
     
   template <- image_composite(template, image_scale(map, 'x150'), offset = paste("+650+", as.character(place)))
   
   image_write(template, paste("Chapter",CCRL$City[i],".png"), format= "png")
  
}

#image_browse(template)

