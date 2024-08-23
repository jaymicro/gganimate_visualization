
# Import package ----------------------------------------------------------------------------------------


library(tidyverse)
library(gganimate)



# CO2 fig -------------------------------------------------------------------------------------------

df <- readxl::read_xlsx("../data/CO2.xlsx") %>% 
  group_by(as.factor(year)) %>% 
  mutate(mean_co2 = mean(co2))
 

p <- ggplot(df, aes(x = year, y = co2))  +
  geom_point(pch=21, color = "black", aes(fill = year, size = co2,group = seq_along(year)), alpha = 0.1, show.legend = F) +
   geom_line(aes(group = 1))+
  theme_bw() +
  scale_x_continuous(limits = c(1955,2024),
    breaks = seq(1955, 2024, by = 5)) +
scale_fill_gradient(low = "blue",  high = "red") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 48, color = "black"),
        axis.text = element_text(size = 24, color = "black"),
        axis.text.x = element_text(angle = -25),
        plot.caption = element_text(size = 14, color = "grey30")) +
  labs(y = bquote(CO[2]~conc.~"(ppm)"),
       x = NULL,
       caption = "Data source: National Oceanic & Atmospheric Administration, US")
p
  
q <- p + 
  #transition_time(year) +
  transition_reveal(year) +
  ease_aes('linear') +
  shadow_mark()

gif_co2 <- animate(q, renderer = gifski_renderer(loop = FALSE),fps = 5, height = 800, width = 1200)
anim_save("../plot/gif_CO2.gif")



# Temp fig ----------------------------------------------------------------------------------------------

df_temp <- read.csv("../data/temp_data.csv") %>% 
  mutate(date = ym(Date),
         year = as.numeric(format(date,'%Y')))


p_temp <- ggplot(df_temp, aes(x = year, y = Anomaly))  +
  geom_point(pch=21,  aes(fill = Anomaly, size = Anomaly,group = seq_along(year)),
              show.legend = F) +
   geom_line(aes(group = 1))+
  geom_hline(aes(yintercept = 0))+
  theme_bw() +
  scale_x_continuous( breaks = c(seq(1950, 2024, by = 5))) +
  scale_fill_gradient2(low = "#0000FF",
                       high = "#FF0000") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 48, color = "black"),
        axis.text = element_text(size = 30, color = "black"),
        axis.text.x = element_text(angle = -35),
        plot.caption = element_text(size = 14, color = "grey30")) +
  labs(y = bquote(Delta~Global~temperatue~"("*degree*C~")"),
       x = NULL,
       caption = "Data source: National Oceanic & Atmospheric Administration, US")+
ylim(c(-0.5, NA))

p_temp

q_temp <- p_temp +
 # transition_time(year) +
  transition_reveal(year) +
  ease_aes('linear') +
  shadow_mark()

gif_temp <- animate(q_temp, renderer = gifski_renderer(loop = FALSE),fps = 5, height = 800, width = 1300)
anim_save("../plot/gif_temp.gif")
