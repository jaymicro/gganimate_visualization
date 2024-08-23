library(tidyverse)
library(rentrez)
library(gganimate)
papers_by_year <- function(years, search_term){
  return(sapply(years, function(y) entrez_search(db="pubmed",term=search_term, mindate=y, maxdate=y, retmax=0)$count))
}


years <- 1970:2023
total_papers <- papers_by_year(years, "")
omics <- c("ecosystem resilience")
trend_data <- sapply(omics, function(t) papers_by_year(years, t))
df <- data.frame(trend_data, years)
plt_df <- df %>% 
  pivot_longer(-years) %>% 
ggplot(., aes(x = years, y = value, group = name, fill = name)) +
  geom_point(pch = 21, size = 3, alpha = 0.3, show.legend = F) +
  geom_path(aes(color = name), linewidth = 0.75) +
  theme_bw() +
  ggsci::scale_fill_aaas(name = "Search terms") +
  ggsci::scale_color_aaas(name = "Search terms") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.key.size = unit(01.75, "cm"),
        plot.caption = element_text(color = "gray40", face = "italic"),
        panel.grid = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.box.margin=margin(5,5,5,5),
        plot.background = element_rect(color = "black", linewidth = 1.5))+
  guides(fill=guide_legend(nrow=4,byrow=TRUE)) +
  labs(title = "Research on ecosystem resilience since 1970",
       y = "Publications per year",
       x = "Year",
       caption = "Data extracted from Pubmed")+
  ggeasy::easy_all_text_color("black") +
  ggeasy::easy_all_text_size(size = 28)

plt_df 

plt <- plt_df + transition_reveal(years) +
  ease_aes('linear') +
  shadow_mark()

pubs <- animate(plt, renderer = gifski_renderer(loop = FALSE),fps = 10, height = 800, width = 1200)
anim_save("../plot/ecosystem_resilience.gif")

