library(tidyverse)

sample_data <-  read_csv("sample_data.csv")

name <- "agar"
name

year <- "1881"

flower <- "rose"

flower


sys.date()


# use as a comments section, always comment your codes for the future, so you will know what are u doing 

ggplot(data=sample_data) + 
  aes(x=temperature) +
  labs(x = "Temperature(C)")+
  aes(y = cells_per_ml/1000000)+
  labs(y="Cells (Millions/mL")+
  geom_point()+
  labs(title="Does temperature affect microbial abundance?") +
  aes(color=env_group) +
  aes(size = chlorophyll) +
  aes(shape= env_group) +
  labs(size = "Chlorophyll (ug/L)",
       color = "Enviromental Group",
       shape = "Enviromental Group")


ggplot(data = sample_data) +
  aes(x=temperature,
      y=cells_per_ml/1000000,
      color=env_group,
      size=chlorophyll) +
  geom_point() +
  labs(x="Temperature(C)",
       y="Cells (Millions/mL)",
       title= "Does temperature affect microbial abundance?",
       size= "Chlorophyll (ug/L)",
       color="Enviromental Group",
       shape= "Enviromental Group")


## importing dataset

buoy_data <- read_csv("buoy_data.csv")
view(buoy_data)

dim(buoy_data)

head(buoy_data) #see beginnig of data
tail(buoy_data) # see end of data


#plot some more
#introduce facets

ggplot(data=buoy_data) +
  aes(x= day_of_year,
      y= temperature,
      group= sensor,
      color= depth) +
  geom_line()+
  #facet_wrap(~buoy, scale= "free_y")
facet_grid(rows= vars(buoy))

## structire of data object
str(buoy_data)


#discrete plots
#boxplot

ggplot(data= sample_data) +
  aes(x= env_group,
      y= cells_per_ml) +
  #geom_violin()
  geom_boxplot(aes(fill= env_group)) +
  scale_fill_manual(values= c("pink", "tomato", "papayawhip"))
  #geom_jitter(aes(size=chlorophyll)) 

  
##scake fill brewer
ggplot(data= sample_data) +
  aes(x= env_group,
      y= cells_per_ml) +
  #geom_violin()
  geom_boxplot(aes(fill= env_group)) +
  scale_fill_brewer(palette = "Set1")

#geom_jitter(aes(size=chlorophyll)) 


## costume palette time

#install.packages("wesanderson")

library(wesanderson)

ggplot(data= sample_data) +
  aes(x= env_group,
      y= cells_per_ml) +
  #geom_violin()
  geom_boxplot(aes(fill= env_group)) +
  scale_fill_manual(values= wes_palette(('Cavalcanti1')))
#geom_jitter(aes(size=chlorophyll)) 

###

library(NatParksPalettes)

ggplot(data= sample_data) +
  aes(x= env_group,
      y= cells_per_ml) +
  #geom_violin()
  geom_boxplot(aes(fill= env_group)) +
  scale_fill_manual(values= natparks.pals(('Glacier')))

##Chjange transparency

ggplot(data= sample_data) +
  aes(x= env_group,
      y= cells_per_ml) +
  geom_boxplot(fill= "darkblue", alpha= 0.3) 

  ## univariate plots
ggplot(sample_data) +
  aes(x= cells_per_ml) +
  geom_density(aes(fill= env_group), alpha= 0.5) +
  theme_bw()


##rotate x axis labes
box_plot <- 
  ggplot(data= sample_data) +
  aes(x= env_group,
      y= cells_per_ml) +
  geom_boxplot() +
theme(axis.text.x = element_text(angle= 90, vjust= 0.5, hjust= 1))




#savings plots
ggsave("awesome_plor.jpg", width = 6, height = 4)

#add changes to the plor for black and white or other theme

box_plot + theme_bw()
      
 
  