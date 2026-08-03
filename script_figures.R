library(tidyverse)

df <- data.frame(nb_cluster = c("8 → 7", "7 → 6", "6 → 5", "5 → 4",
                       "4 → 3", "3 → 2", "2 → 1"),
                 delta = c(0.5, 0.6, 0.7, 0.8, 1.4, 1.5, 3))

df$nb_cluster <- factor(
  df$nb_cluster,
  levels = df$nb_cluster
)

g <- df %>% 
  ggplot(aes(x = nb_cluster, y = delta))+
  geom_bar(stat = "identity", col = "black", fill = "blue")+
  theme_minimal()+
  labs(x = "number of cluster",
       y = "delta T", 
       title = "Variation of criterion T")+
  theme(axis.text = element_text(size = 15, color = "black"), 
        plot.title = element_text(size = 15, hjust = 0.5),
        axis.title = element_text(size = 15))


ggsave("C:/Users/g.martet/Documents/Agrostat Dijon/figures/var_crit_T.png", g, dpi = 400,
       width = 7, height = 5)
