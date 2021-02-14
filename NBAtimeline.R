#Notable Black Achievements
#The 'first' achievements in various fields have historically marked footholds, often leading to more widespread cultural change. The shorthand phrase for this is "breaking the color barrier"
#"[Black] history is about much more than chronicling a series of “firsts.”  The time and place of a breakthrough reflects not only remarkable individual achievement but is itself an indication of the progress or lack of progress of black people in realizing the centuries-old intertwined goals of freedom, equality, and justice." ~ blackpast.org
#Datasource: Wikipedia & Kaggle

library (tidyverse)
library (plotly)
library (showtext)

showtext_auto ()

df <- read.csv("https://query.data.world/s/conu5g63p5djt76z4v3x2liephcggn", header=TRUE, stringsAsFactors=FALSE)

plot <- df %>% 
  group_by (year, category) %>%
  mutate(n = n())%>% 
  ggplot(
  aes (x=year, y=n, text=accomplishment)) +
  geom_col (aes (fill = category)) + 
  ylim (0, 45) +
  theme (legend.position = "right",
         legend.title = element_blank()) +
  scale_fill_brewer(palette = "Dark2") +
  labs (title = "Notable Black 'First' Achievements in the US")

plot1 <- plot + scale_x_continuous(limits = c(1738, 1808))
fig1 <- ggplotly(plot1)

plot2 <- plot + scale_x_continuous(limits = c(1809, 1879))  
fig2 <- ggplotly (plot2)

plot3 <- plot + scale_x_continuous(limits = c(1880, 1950))  
fig3 <- ggplotly (plot3)

plot4 <- plot + scale_x_continuous(limits= c(1951, 2021))  
fig4 <- ggplotly (plot4)

subplot(style (fig1,showlegend = F), 
        style (fig2, showlegend = F), 
        style (fig3, showlegend = F),
        style (fig4, showlegend = T),
        nrows=4, 
        shareX = FALSE)



######################
#third = #1880-1950
#max = 35

third <- df %>%
  filter (1880 <= year & year <= 1950) 

third_grouped <- third %>%
  group_by (year, category) %>%
  mutate(n = n()) 

thirdplot <- ggplot (third_grouped, aes (x=year, y=n)) +
  geom_col (aes (fill = category))+ ylim (0, 45)

######################
#fourth = #1951-2020
#max = 45

fourth <- df %>%
  filter (1951 <= year & year <= 2020) 

fourth_grouped <- fourth %>%
  group_by (year, category) %>%
  mutate(n = n()) 

ggplot (fourth_grouped, aes (x=year, y=n)) +
  geom_col (aes (fill = category))+ ylim (0, 45)

########################
#put plots together

 firstplot /
  secondplot /
  thirdplot /
  fourthplot 
