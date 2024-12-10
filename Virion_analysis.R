#load libraries 

#install.packages("dplyr")
library(dplyr)
library(ggplot2)

Cui_etal2014.csv <- read.csv("question-5-data/Cui_etal2014.csv")
head(Cui_etal2014.csv)
summary(Cui_etal2014.csv)
dim(Cui_etal2014.csv)
#data has 33 rows and 13 colomns


#transformation for linear model

# filter data set 
vir_vol <- Cui_etal2014.csv %>%  select(Virion.volume..nm.nm.nm., Genome.length..kb.)
#rename cols
virion_volume_genome <- vir_vol %>% 
  rename(
    `Virion_volume` = Virion.volume..nm.nm.nm., 
    `Genome_length` = Genome.length..kb.
  )



#plot of current trend
scatter_virion <- (ggplot(data = virion_volume_genome, aes(x = Genome_length, y = Virion_volume) ) + 
                     geom_point())
scatter_virion

#log transformation
log_transformed_volume_genome <- virion_volume_genome %>% mutate(log(Virion_volume)) %>% mutate(log(Genome_length))



linear_model_genome_volume <- lm(log(Virion_volume) ~ log(Genome_length), data=log_transformed_volume_genome)
summary(linear_model_genome_volume)

#Q 5 D

Volume_genome_plot.pdf <- ggplot(data = log_transformed_volume_genome) + 
  geom_point(aes(x = log(Genome_length), y = log(Virion_volume)), size = 0.75) +
  geom_smooth(method = "lm", aes(x = log(Genome_length), y = log(Virion_volume))) +
  xlab("log[Genome length (kb)]") +
  ylab("log[Virion volume (nm3)]") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 9), 
    axis.text.y = element_text(size = 9), 
    axis.title = element_text(size = 9, face = "bold") # Set axis titles to bold
  )

Volume_genome_plot.pdf

ggsave(filename = "Volume_genome_plot.pdf")

#Q5 E
# Finding estimated volume of a 300 kb dsDNA virus ----
# Use of the equation provided and calculated values of the exponent and scaling
# factor
1181.807*(300^1.5152)

# volume is 6697006 nm^3 or 6.70 x10^6 nm^3 (3sf) 


