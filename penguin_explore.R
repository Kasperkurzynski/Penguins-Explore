setwd("C:/Users/Kasper/OneDrive/Pulpit/Applied Statistics/R Learning/Penguin explorig")
dane <- read.csv("penguins_size.csv")
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(extrafont)


summary(dane)
str(dane)

dane <- dane %>%
  mutate(species = factor(species), island = factor(island), sex = factor(sex)) %>%
  filter(complete.cases(dane) & dane$sex != ".")

# Example of bad visualization
ggplot(dane, aes(x=species)) + geom_bar()

# Example of better visualization
dane %>%
  drop_na() %>%
  count(island, species) %>%
  ggplot() + geom_col(aes(x = species, y = n, fill = species)) +
  geom_label(aes(x = species, y = n, label = n)) +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  facet_wrap(~island) +
  theme_minimal() +
  ggtitle('Penguins species number by islands') + 
  theme(axis.title.y = element_text(color="Grey23", size=16),
        axis.text.y = element_text(size=10),
        axis.title.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="darkslateblue"),
        plot.title = element_text(color="darkslateblue", size=22, family="serif"))

# Example of better visualization
dane %>%
  drop_na() %>%
  count(sex, species) %>%
  ggplot() + geom_col(aes(x = sex, y = n, fill = sex)) +
  geom_label(aes(x = sex, y = n, label = n)) +
  scale_fill_manual(values = c("dodgerblue2","coral2")) +
  facet_wrap(~species) +
  theme_minimal() +
  ggtitle('Penguins female & male number by species') + 
  theme(axis.title.y = element_text(color="Grey23", size=16),
        axis.text.y = element_text(size=10),
        axis.title.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="darkslateblue"),
        plot.title = element_text(color="darkslateblue", size=22, family="serif"))


#Nuda
ggplot(dane, aes(x = flipper_length_mm)) + geom_histogram(binwidth = 5)

#Dzieje siê
a <- ggplot(dane, aes(x = culmen_length_mm, fill=species)) + geom_histogram(binwidth = 2, alpha = 0.7) + facet_grid(~island) + theme_minimal() +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) + 
  ggtitle('Distribution of variables') + 
  theme(axis.title.y = element_text(color="Grey23", size=11),
        axis.text.y = element_text(size=9),
        axis.title.x = element_text(color="Grey23", size=11),
        axis.text.x = element_text(size=9),
        legend.title = element_text(size=10.5),
        legend.text = element_text(size=8.5),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="darkslateblue"),
        plot.title = element_text(color="darkslateblue", size=22, family="serif"))

b <- ggplot(dane, aes(x = culmen_depth_mm, fill=species)) + geom_histogram(binwidth = 1, alpha = 0.7) + facet_grid(~island) + theme_minimal() +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) + 
  theme(axis.title.y = element_text(color="Grey23", size=11),
        axis.text.y = element_text(size=9),
        axis.title.x = element_text(color="Grey23", size=11),
        axis.text.x = element_text(size=9),
        legend.title = element_text(size=10.5),
        legend.text = element_text(size=8.5),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="darkslateblue"))
c <- ggplot(dane, aes(x = flipper_length_mm, fill=species)) + geom_histogram(binwidth = 5, alpha = 0.7) + facet_grid(~island) + theme_minimal() +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) + 
  theme(axis.title.y = element_text(color="Grey23", size=11),
        axis.text.y = element_text(size=9),
        axis.title.x = element_text(color="Grey23", size=11),
        axis.text.x = element_text(size=9),
        legend.title = element_text(size=10.5),
        legend.text = element_text(size=8.5),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="darkslateblue"))
grid.arrange(a,b,c)


#Dzieje siê
ggplot(dane, aes(x = flipper_length_mm, fill=sex)) + geom_histogram(binwidth = 5, alpha = 0.7) + facet_grid(~species, scales="free") + theme_minimal() +
  scale_fill_manual(values = c("dodgerblue2","coral2")) +
  ggtitle('Distribution of flipper length by sex and species') + 
  theme(axis.title.y = element_text(color="Grey23", size=16),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(color="Grey23", size=16),
        axis.text.x = element_text(size=10),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="darkslateblue"),
        plot.title = element_text(color="darkslateblue", size=22, family="serif"))

#Dzieje siê
ggplot(dane, aes(x = flipper_length_mm, fill=species)) + geom_histogram(binwidth = 5, alpha = 0.7) + facet_grid(~island, scales="free") + theme_minimal() +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  ggtitle('Distribution of flipper length by species and island') + 
  theme(axis.title.y = element_text(color="Grey23", size=16),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(color="Grey23", size=16),
        axis.text.x = element_text(size=10),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="darkslateblue"),
        plot.title = element_text(color="darkslateblue", size=22, family="serif"))

#Nuda
ggplot(dane, aes(x = species, y = body_mass_g)) + geom_boxplot()

#Dzieje siê
ggplot(dane, aes(x = species, y = body_mass_g/1000, fill=species)) + geom_jitter() + geom_boxplot(size=1.2, alpha=0.5) + facet_grid(~sex) +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) + theme_bw() + ylab("Body weight in kilograms") + 
  ggtitle("Penguins bodyweight by species and sex") +
  theme(axis.title.y = element_text(color="Grey23", size=16),
        axis.text.y = element_text(size=10),
        axis.title.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="darkslateblue"),
        plot.title = element_text(color="darkslateblue", size=22, family="serif"))

#Dzieje siê
ggplot(dane, aes(x = sex, y = culmen_length_mm, fill=sex)) + geom_jitter() + geom_boxplot(size=1.2, alpha=0.5) + facet_grid(~species) +
  scale_fill_manual(values = c("dodgerblue2","coral2")) + theme_bw() + ylab("Culmen length in mm") + 
  ggtitle("Culmen length by species and sex") +
  theme(axis.title.y = element_text(color="Grey23", size=16),
        axis.text.y = element_text(size=10),
        axis.title.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="darkslateblue"),
        plot.title = element_text(color="darkslateblue", size=22, family="serif"))



#Nuda
ggplot(dane, aes(x = culmen_length_mm, y = culmen_depth_mm)) + geom_point()

#Dzieje siê
ggplot(dane, aes(x = culmen_length_mm, y = culmen_depth_mm, color=species)) + geom_point(aes(shape=species, size=sex), alpha = 0.8) +
  scale_shape_manual(values=c(18, 16, 17)) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  theme_minimal() +
  ggtitle("Relationship between culmen length and depth") +
  theme(axis.title.y = element_text(color="Grey23", size=15),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(color="Grey23", size=15),
        axis.text.x = element_text(size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="darkslateblue"),
        plot.title = element_text(color="darkslateblue", size=25, family="serif"))

ggplot(dane, aes(x = flipper_length_mm, y = body_mass_g/1000, shape=species, color=species)) + geom_point() + geom_smooth(method=lm) +
  theme_minimal() + facet_grid(~sex) +
  ggtitle("Relationship between body weight and flipper length") +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  theme(axis.title.y = element_text(color="Grey23", size=15),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(color="Grey23", size=15),
        axis.text.x = element_text(size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="darkslateblue"),
        plot.title = element_text(color="darkslateblue", size=25, family="serif"))


gento <- dane[dane$species=="Gentoo",]
tapply(gento$flipper_length_mm, gento$sex, mean)
