#loading packages
library(tidyverse)
# reading in data

sample_data <- read.csv("data/sample_data.csv")

#summarize 
summarize(sample_data, average_cells= mean(cells_per_ml))

sample_data %>% summarize(average_cells= mean(cells_per_ml))

#filtering rows

sample_data %>% 
  filter(env_group== "Deep") %>% 
  summarise(average_cells= mean(cells_per_ml))

#Calculate the average chlorophyll in the entire dataset
#Calculate the average chlorophyll just in Shallow September

summarise(sample_data, average_Chlorophyll= mean(chlorophyll))

sample_data %>% 
  filter(env_group== "Shallow_September") %>%
  summarise(average_Chlorophyll= mean(chlorophyll))


#group_by  

sample_data %>% 
  group_by(env_group) %>% 
  summarise(average_cells= mean(cells_per_ml),
            min_cells= min(cells_per_ml))

#Calculate the average temp per env group

sample_data %>% 
  group_by(env_group) %>% 
  summarise(average_temp= mean(temperature))

#Mutate
#TN:TP

sample_data %>% 
  mutate(tn_tp_ratio= total_nitrogen/total_phosphorus)

sample_data %>% 
  mutate(temp_is_hot= temperature > 8) %>% 
  group_by(env_group, temp_is_hot) %>% 
  summarise(avg_temp= mean(temperature))

#Selecting columns with select

sample_data %>% 
  select(sample_id, depth)

sample_data %>% 
  select(-env_group)

sample_data %>% 
  select(sample_id:temperature)

sample_data %>% 
  select(starts_with("total"))

#create a data frame with only sample _id, env_group, depth< tenperature and cells_per_ml

sample_data %>% 
  select(sample_id, env_group, depth,temperature, cells_per_ml)

sample_data %>% 
  select(sample_id:temperature)

#Cleaning data

taxon_clean <- read_csv("data/taxon_abundance.csv", skip = 2) %>% 
  select(-...10) %>% 
  rename(sequencer= ...9) %>% 
  select(-Lot_Number, -sequencer)


#Remove the lot numer and sequencer collumns
#assign this all to an object called "taxon_clean"

taxon_long <- taxon_clean %>% 
  pivot_longer(cols = Proteobacteria:Cyanobacteria,
               names_to = "Phylum", 
               values_to = "Abundance")
taxon_long %>% 
  group_by(Phylum) %>% 
  summarise(avg_abun= mean(Abundance))


taxon_long %>% 
  ggplot()+
  aes(x= sample_id,
      y= Abundance,
      fill= Phylum) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))

# making long data wide

taxon_long %>% 
  pivot_wider(names_from = "Phylum",
              values_from = "Abundance")

# Joining dara frames

head(sample_data)

head(taxon_clean)  

#inner join

inner_join(sample_data, taxon_clean, by = "sample_id")

anti_join(sample_data, taxon_clean, by= "sample_id")

sample_data$sample_id

taxon_clean$sample_id

taxon_clean_goodSep <- taxon_clean %>% 
  mutate(sample_id= str_replace(sample_id, pattern = "Sep", replacement = "September"))


sample_and_taxon <- inner_join(sample_data, taxon_clean_goodSep, by="sample_id")

write_csv(sample_and_taxon, file = "data/sample_and_taxon.csv")


#make a plot
#ask: where does chloroflexi like to live?
install.packages("ggpubr")
library(ggpubr)

sample_and_taxon %>% 
  ggplot()+
  aes(x= depth,
      y= Chloroflexi) +
  geom_point() +
  labs(x= "Depth(m)",
       y= "Chloroflexi relative abundance") +
  geom_smooth(method = "lm") +
  stat_cor() +
  annotate(geom = "text",
           x= 25, y= .3,
           label= "This is a text label")

#whats the average abundance and sd of Chloroflexi in our three env_groups

sample_and_taxon %>% 
  group_by(env_group) %>% 
  summarise(avg_chloro= mean(Chloroflexi),
            sd_chloro= sd(Chloroflexi))





