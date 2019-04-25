#Read the Current Chapter file

CC <- read_excel("D:/Rladies/IWD019/IWD2019/CCRL2.xlsx")

#I have to geposition location for chapter outside R because google ask for a API key now and I have to give my credit card number
#and I do not want to do that ...jeje
#The code for doind that task in R is in the post for Dani Vazquez: https://d4tagirl.com/2017/05/how-to-fetch-twitter-users-with-r

LatLong <- read_excel("D:/Rladies/IWD019/IWD2019/LatLong.xlsx")

CC <- CC %>% inner_join(LatLong, by = c("City", "State/Region", "Country"))


world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map()

map <- world +
  geom_point(aes(x = Longitude, y = Latitude,
                 size = 3),
             data = CC, colour = '#562457', alpha = .5) 
