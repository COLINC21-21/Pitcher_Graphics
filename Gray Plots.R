Gray <- scrape_statcast_savant(start_date = "2022-09-01", end_date = "2022-10-02", playerid = 592351, player_type = "pitcher")

x <- c(-.95,.95,.95,-.95,-.95)
z <- c(1.6,1.6,3.5,3.5,1.6)
## First one is pitcher 
plate_dimensons <- data.frame(x1=0,x2=-.95, x3= .95,y1=1,y2=.5,y3=0)

## Second one is hitter 
plate_dimensons <- data.frame(x1=0, x2=-.95, x3 = .95, y1 =0, y2= .5, y3=1 )
sz <- data.frame(x,z) 
ggplot() +
  coord_fixed() +
  geom_density2d_filled(data = Gray, 
                        aes(x = plate_x, y = plate_z, fill = after_stat(level)),
                        contour_var = "ndensity", 
                        show.legend = FALSE,  
                        alpha = .85) +
  scale_fill_viridis_d(option = "B") + 
  
  geom_path(data = sz, aes(x=x, y=z)) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = plate_dimensons) +
  geom_segment(aes(x = x2, y = y2, xend = x2, yend = y3), data = plate_dimensons) + 
  geom_segment(aes(x = x2, y = y3, xend = x3, yend = y3), data = plate_dimensons) +
  geom_segment(aes(x = x3, y = y3, xend = x3, yend = y2), data = plate_dimensons) + 
  geom_segment(aes(x = x3, y = y2, xend = x1, yend = y1), data = plate_dimensons) + 
  theme_minimal() +
  facet_wrap(~pitch_type)+
  labs(title = "Jon Gray September & October",
       caption = "Colin Campbell\n Source:Baseballr") +
  
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5, size = 12), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12), 
        strip.text = element_text(size = 12)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(color = "brown"),
        plot.caption = element_text(color = "brown", face = 'bold'))

movement_plot <- function(...){
  ggplot(...)+
    geom_segment(x= 0,xend = 0, y= -20, yend = 20, color = "grey")+
    geom_segment(x= -20, xend = 20, y = 0, yend = 0, color = "grey")+
    scale_x_continuous(NULL, limits = c(-22,22)) +
    scale_y_continuous(NULL, limits = c(-22,22))
}


)

movement_plot(Gray, aes(x = -(pfx_x)*12, y= (pfx_z)*12, color = pitch_type))+
  geom_point()+
  geom_encircle(aes(group=pitch_type,fill=pitch_type),alpha=0.2)+
  labs(title = "Jon Gray Movement Plot", xlab = "Horizontal Movement", ylab= "Vertical Movement", 
       caption = "Colin Campbell\n Source:Baseballr")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "brown"),
        plot.caption = element_text(color = "brown", face = 'bold'))


releasepoint_plot <- function(...){
  ggplot(...)+
    geom_segment(x= 0,xend = 0, y= -20, yend = 20, color = "grey")+
    geom_segment(x= -5, xend = 20, y = 0 , yend = 0, color = "grey")+
    scale_x_continuous(NULL, limits = c(-2.7,.5)) +
    scale_y_continuous(NULL, limits = c(0,7))
}
)

# With encircel 
releasepoint_plot(Gray, aes(x = release_pos_x, y= release_pos_z, color = pitch_type))+
  geom_point()+
  geom_encircle(aes(group=pitch_type,fill=pitch_type),alpha=0.2)+
  labs(title = "Jon Gray Relase Point ", ylab= "Relase Height", 
       caption = "Colin Campbell\n Source:Baseballr")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  theme(plot.title = element_text(color = "brown"),
        plot.caption = element_text(color = "brown", face = 'bold'))

table(Gray$PitchType)


Prop <- data.frame(round(prop.table(table(Gray$pitch_type))*100, 1))

Prop$lab <- paste(round(Prop$Freq,1),"%")

ggplot(Prop, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(label = lab), position = position_stack(vjust =  0.5), size = 5)+
  labs(title = "Colin Ruth Pitch Usage in 0-1 Count")+theme(legend.title = element_blank())

Gray_LHB <- filter(Gray, stand == "L")


ggplot() +
  coord_fixed() +
  geom_density2d_filled(data = Gray, 
                        aes(x = plate_x, y = plate_z, fill = after_stat(level)),
                        contour_var = "ndensity", 
                        show.legend = FALSE,  
                        alpha = .85) +
  scale_fill_viridis_d(option = "B") + 
  
  geom_path(data = sz, aes(x=x, y=z)) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = plate_dimensons) +
  geom_segment(aes(x = x2, y = y2, xend = x2, yend = y3), data = plate_dimensons) + 
  geom_segment(aes(x = x2, y = y3, xend = x3, yend = y3), data = plate_dimensons) +
  geom_segment(aes(x = x3, y = y3, xend = x3, yend = y2), data = plate_dimensons) + 
  geom_segment(aes(x = x3, y = y2, xend = x1, yend = y1), data = plate_dimensons) + 
  theme_minimal() +
  facet_wrap(~pitch_type)+
  labs(title = "Jon Gray September & October",
       subtitle = "VS LHB",
       caption = "Colin Campbell\n Source:Baseballr") +
  
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5, size = 12), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12), 
        strip.text = element_text(size = 12)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(color = "brown"),
        plot.caption = element_text(color = "brown", face = 'bold'))

Gray_RHB <- filter(Gray, stand == "R")
ggplot() +
  coord_fixed() +
  geom_density2d_filled(data = Gray_RHB, 
                        aes(x = plate_x, y = plate_z, fill = after_stat(level)),
                        contour_var = "ndensity", 
                        show.legend = FALSE,  
                        alpha = .85) +
  scale_fill_viridis_d(option = "B") + 
  
  geom_path(data = sz, aes(x=x, y=z)) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = plate_dimensons) +
  geom_segment(aes(x = x2, y = y2, xend = x2, yend = y3), data = plate_dimensons) + 
  geom_segment(aes(x = x2, y = y3, xend = x3, yend = y3), data = plate_dimensons) +
  geom_segment(aes(x = x3, y = y3, xend = x3, yend = y2), data = plate_dimensons) + 
  geom_segment(aes(x = x3, y = y2, xend = x1, yend = y1), data = plate_dimensons) + 
  theme_minimal() +
  facet_wrap(~pitch_type)+
  labs(title = "Jon Gray September & October",
       subtitle = "VS RHB",
       caption = "Colin Campbell\n Source:Baseballr") +
  
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5, size = 12), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12), 
        strip.text = element_text(size = 12)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(color = "brown"),
        plot.caption = element_text(color = "brown", face = 'bold'))


Gray01 <- filter(Gray, balls == 0 & strikes == 1 )
nrow(Gray01)
table(Gray01$PitchType)


Prop <- data.frame(round(prop.table(table(Gray01$pitch_type))*100, 1))

Prop$lab <- paste(round(Prop$Freq,1),"%")

ggplot(Prop, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(label = lab), position = position_stack(vjust =  0.5), size = 5)+
  labs(title = "Colin Ruth Pitch Usage in 0-1 Count")+theme(legend.title = element_blank())

Gray02 <- filter(Gray, balls == 0 & strikes == 2 )
nrow(Gray02)
table(Gray02$PitchType)


Prop <- data.frame(round(prop.table(table(Gray02$pitch_type))*100, 1))

Prop$lab <- paste(round(Prop$Freq,1),"%")

ggplot(Prop, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(label = lab), position = position_stack(vjust =  0.5), size = 5)+
  labs(title = "Colin Ruth Pitch Usage in 0-1 Count")+theme(legend.title = element_blank())

Grayfb <- filter(Gray, pitch_type == "FF")
summary(Grayfb)
