library(ggplot2)
library(ggalt)
library(baseballr)
library(tidyverse)

movement_plot <- function(...){
  ggplot(...)+
    geom_segment(x= 0,xend = 0, y= -20, yend = 20, color = "grey")+
    geom_segment(x= -20, xend = 20, y = 0, yend = 0, color = "grey")+
    scale_x_continuous(NULL, limits = c(-22,22)) +
    scale_y_continuous(NULL, limits = c(-22,22))
}


### Pitch Usage Pie Chart 
playerid_lookup("Holmes", "Clay")

Gil <- scrape_statcast_savant(start_date = "2022-07-15", "2022-08-05", playerid =  605280, player_type = "pitcher")
str(Gil)

movement_plot(Gildf, aes(x = -(pfx_x)*12, y= (pfx_z)*12, color = pitch_type))+
  geom_point()+
  geom_encircle(aes(group=pitch_type,fill=pitch_type),alpha=0.2)+
  labs(title = "Clay Holmes Movement Plot", xlab = "Horizontal Movement", ylab= "Vertical Movement", 
       caption = "Colin Campbell\n Source:Baseballr")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

Gildf <- mutate(Gil, pitch_type = case_when(pitch_type %in% c("SI") ~ "Sinker",
                                           pitch_type%in% c("SL") ~"Slider "))
### I changed the data input for the ggplot. It was Gil but I wanted "Sinker", "Slider" instead of SI and SL              

### Gil 0-0 
Gil00 <- filter(Gildf, balls == 0 & strikes == 0 )
nrow(Gil00)
table(Gil00$pitch_type)


Prop <- data.frame(round(prop.table(table(Gil00$pitch_type))*100, 1))

Prop$lab <- paste(round(Prop$Freq,1),"%")

ggplot(Prop, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(label = lab), position = position_stack(vjust =  0.5), size = 5)+
  labs(title = "Luis Gil Pitch Usage in 0-0 Count")+theme(legend.title = element_blank())


### Gil 0-2 

Gil02 <- filter(Gildf, balls == 0 & strikes == 2 )
nrow(Gil02)
table(Gil02$pitch_type)


Prop <- data.frame(round(prop.table(table(Gil02$pitch_type))*100, 1))

Prop$lab <- paste(round(Prop$Freq,1),"%")

ggplot(Prop, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(label = lab), position = position_stack(vjust =  0.5), size = 5)+
  labs(title = "Luis Gil Pitch Usage in 0-2 Count", caption = "Colin Campbell")+theme(legend.title = element_blank())

### Gil 2-0
Gil20 <- filter(Gildf, balls == 2 & strikes == 0 )
nrow(Gil20)
table(Gil20$pitch_type)


Prop <- data.frame(round(prop.table(table(Gil20$pitch_type))*100, 1))

Prop$lab <- paste(round(Prop$Freq,1),"%")

ggplot(Prop, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(label = lab), position = position_stack(vjust =  0.5), size = 5)+
  labs(title = "Luis Gil Pitch Usage in 2-0 Count", caption = "Colin Campbell")+theme(legend.title = element_blank())







### Gil 3-2 

Gil32 <- filter(Gildf, balls == 3 & strikes == 2 )
nrow(Gil32)
table(Gil32$pitch_type)


Prop <- data.frame(round(prop.table(table(Gil32$pitch_type))*100, 1))

Prop$lab <- paste(round(Prop$Freq,1),"%")

ggplot(Prop, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(label = lab), position = position_stack(vjust =  0.5), size = 5)+
  labs(title = "Luis Gil Pitch Usage in 3-2 Count", caption = "Colin Campbell")+theme(legend.title = element_blank())


#### Heat Map 

##Drawing The Strike Zone
x <- c(-.95,.95,.95,-.95,-.95)
z <- c(1.6,1.6,3.5,3.5,1.6)


#store in dataframe
sz <- data.frame(x,z)
sz2 <- data.frame(xx,zz)

ggplot() +
  ##First plotting the strike zone that we created
  geom_path(data = sz2, aes(x=x, y=z)) +
  coord_equal() 

##Changing Pitch Names
pitch_desc <- Gil$pitch_type

##Changing Pitch Names
pitch_desc[which(pitch_desc=='CH')] <- "Changeup"
pitch_desc[which(pitch_desc=='CU')] <- "Curveball"
pitch_desc[which(pitch_desc=='FC')] <- "Cutter"
pitch_desc[which(pitch_desc=='FF')] <- "Four seam"
pitch_desc[which(pitch_desc=='FS')] <- "Split Flinger"
pitch_desc[which(pitch_desc=='FT')] <- "Two-Seam"
pitch_desc[which(pitch_desc=='KC')] <- "Kuckle-Curve"
pitch_desc[which(pitch_desc=='SI')] <- "Sinker"
pitch_desc[which(pitch_desc=='SL')] <- "Slider"

ggplot() +
  ##First plotting the strike zone that we created
  geom_path(data = sz, aes(x=x, y=z)) +
  coord_equal() 
ggplot() +
  geom_segment(data = sz, aes(x=x,y=z))+
  coord_equal() 
+
  ##Now plotting the actual pitches
  geom_point(data = Gil, aes(x = plate_x, y = plate_z,color = pitch_desc)) +
  scale_size(range = c(-1.0,2.5))+
  
  ##Using the color package 'Viridis' here
  labs(size = "Exit Veloicty",
       color = "Pitch Type",
       title = "Clay Holmes - Heat Map",
       subtitle = "Post ASG", caption = "Colin Campbell") +
  ylab("Feet Above Homeplate") +
  xlab("Feet From Homeplate") +
  theme(plot.title=element_text(face="bold",hjust=-.015,vjust=0,colour="#3C3C3C",size=20),
        plot.subtitle=element_text(face="plain", hjust= -.015, vjust= .09, colour="#3C3C3C", size = 12)) +
  theme(axis.text.x=element_text(vjust = .5, size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=0)) +
  theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
  theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) +
  theme(panel.background = element_rect(fill = "white")) 

  
  
  
  
  ggplot() +
    ##First plotting the strike zone that we created
    geom_path(data = sz, aes(x=x, y=z)) +
    coord_equal() +
    ##Now plotting the actual pitches
    geom_point(data = Gil, aes(x = plate_x, y = plate_z, color = pitch_desc)) +
    scale_size(range = c(-1.0,2.5))+
    
    ##Using the color package 'Viridis' here
    labs(size = color = "Exit Velo",
         title = "Clay Holmes - Pitch Chart",
         subtitle = "Post ASG", caption = "Colin Campbell") +
    ylab("Feet Above Homeplate") +
    xlab("Feet From Homeplate")  
  theme(plot.title=element_text(face="bold",hjust=-.015,vjust=0,colour="#3C3C3C",size=20),
        plot.subtitle=element_text(face="plain", hjust= -.015, vjust= .09, colour="#3C3C3C", size = 12)) +
    theme(axis.text.x=element_text(vjust = .5, size=11,colour="#535353",face="bold")) +
    theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
    theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
    theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=0)) +
    theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
    theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) +
    theme(panel.background = element_rect(fill = "white"))

#### Glow in the Dark heat map
# Using raster
ggplot(data = Gil, aes(x= plate_x, y=plate_z)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  ) +
  geom_path(data = sz, aes(x=x,y=z)) + 
  scale_size(range = c(-1.0,2.5)) +
  scale_fill_distiller(palette= "Spectral", direction=1) +
  theme_void()+
  labs(size = "Speed",
       color = "Pitch Type",
       title = "Clay Holmes - Pitch Chart",
       subtitle = "Post ASG", caption = "Colin Campbell") +
  ylab("Feet Above Homeplate") +
  xlab("Feet From Homeplate") +
  theme(plot.title=element_text(face="bold",hjust=-.015,vjust=0,colour="#3C3C3C",size=20),
        plot.subtitle=element_text(face="plain", hjust= -.015, vjust= .09, colour="#3C3C3C", size = 12)) +
  theme(axis.text.x=element_text(vjust = .5, size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=0)) +
  theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
  theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) +
  theme(panel.background = element_rect(fill = "white")) 

