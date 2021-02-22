library (tidyverse)
library (showtext)
library (patchwork)

df <- read.csv("https://query.data.world/s/conu5g63p5djt76z4v3x2liephcggn", 
               header=TRUE, stringsAsFactors=FALSE) 

## Loading Google fonts (https://fonts.google.com/)
font_add_google("Bebas Neue")
font_add_google ("Lato")

showtext_auto ()

#color palette
bfly.col <- c("5F706A", "#D5B59B", "#C84F36", "#AE8B6D", "#2C2B2C", "#61A89D", "#E7965F", "#793324")

df1 <- df %>%
  group_by (year, category) %>%
  summarize (n = n()) %>%
  ungroup %>%
  group_by (category) %>%
  mutate (csum = cumsum (n))

allyears <- df1 %>% 
  group_by(category) %>% 
  complete(year = full_seq(1738:2021, 1)) %>% 
  group_by (category) %>%
  fill(csum)

allyears[is.na(allyears)] <- 0
allyears$category <- as.factor (allyears$category)

plot <- ggplot (allyears, aes (x=year, y=csum, fill = category)) + 
  geom_area () + 
  scale_y_continuous (position = "right")

plot


plot_total <- plot + theme (text = element_text (family = "Lato", size = 12),
                   plot.title = element_text (family = "Bebas Neue", face = "bold", size = 17),
                   plot.subtitle = element_text (family = "Lato", face = "italic", size = 13),
                   legend.position = "none",
                   panel.grid = element_blank (),
                   axis.line.x = element_line (),
                   axis.text.y = element_blank (),
                   axis.ticks.x = element_blank (),
                   axis.ticks.y = element_blank (),
                   panel.background = element_rect(color = "#E3E4E1", fill = "#E3E4E1"), #ice cube
                   plot.background = element_rect(color = "#e3e4e1", fill = "#e3e4e1")) +
            scale_fill_manual (values = bfly.col) +
            labs (title = "\nBlack history is about much more \nthan chronicling \na series of 'firsts'",
                  subtitle = "\nThe time and place of a breakthrough reflects \nnot only remarkable individual achievement\nbut is itself an indication of the progress \nor lack of progress of black people\nin realizing the centuries-old intertwined goals\nof freedom, equality, and justice. \n\n~ blackpast.org ",
                  x = "Year", 
                  y = "Cumulative Achievements") 
plot_total
        
#Facetted
  
facets <-  ggplot (allyears, aes (x=year, y=csum, fill = category)) + 
    geom_area () +
    facet_wrap(~category, nrow = 2, strip.position = "bottom") +
  theme (text = element_text (family = "Lato", size = 10),
         legend.position = "none",
         strip.background = element_rect(
           color="#D5B59B", fill="#d5b59b", size=1.5, linetype="solid"
         ),
         panel.grid = element_blank (),
         panel.background = element_rect(color = "#e3e4e1", fill = "#e3e4e1"),
         plot.background = element_rect(color = "#e3e4e1", fill = "#e3e4e1"),
         axis.text.y = element_blank (),
         axis.ticks.y = element_blank (),
         axis.ticks.x = element_blank ()) +
  
  labs (x = " ", y = " ", caption = "Data from blackpast.org | for @DiversityinData | graphic @joieprout") +
  scale_fill_manual (values = c("5F706A", "#D5B59B", "#C84F36", "#AE8B6D", "#2C2B2C", "#61A89D", "#E7965F", "#793324"))

facets

plot_total / facets



