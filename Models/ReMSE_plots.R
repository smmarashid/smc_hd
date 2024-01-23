library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)
library(ggtext)
library(extrafont)
require(gridExtra)
library(cowplot)
font_import()
fonts()

FinalDF <- read.csv("C:/Users/sanji/Downloads/Thesis/PLOTS/FinalDF_10")
FinalDF$d <- as.factor(FinalDF$d)
FinalDF$N <- as.factor(FinalDF$N)
FinalDF$run <- as.factor(FinalDF$run)

### ReMSE###
filtered_df <- FinalDF[FinalDF$algo != "boot" & FinalDF$d == 16, ]
df_remse <- filtered_df %>%
  group_by(algo, N, run) %>%
  summarize(remse = mean(remse), .groups = 'drop')

remse1 <- ggplot(df_remse, aes(x = algo, y = remse, fill = N)) +
  geom_boxplot() + 
  scale_x_discrete(labels = c("DaC-SMC", "NSMC", "STPF")) +
  labs(x = "", y = "ReMSE", fill = expression(italic(n))) +
  scale_y_continuous(breaks = seq(0,.3,.1),limits = c(0,.3)) + 
  ggthemes::theme_base(base_size = 8) + theme(legend.position = "none") +
  theme(text = element_text(family = "CMU Serif", size = 12),
        axis.text.x = element_text(margin = margin(t = 5)),  # Increase space below x-axis label
        axis.text.y = element_text(margin = margin(r = 5)),
        plot.background = element_blank()
  ) +
  theme(axis.title.x = element_text(margin = margin(t = 5))) +
  ggtitle(expression(italic(d)[italic(x)] == 16)) +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "italic")
  )

filtered_df <- FinalDF[FinalDF$algo != "boot" & FinalDF$d == 32, ]
df_remse <- filtered_df %>%
  group_by(algo, N, run) %>%
  summarize(remse = mean(remse), .groups = 'drop')

remse2 <- ggplot(df_remse, aes(x = algo, y = remse, fill = N)) +
  geom_boxplot() + 
  scale_x_discrete(labels = c("DaC-SMC", "NSMC", "STPF")) +
  labs(x = "", y = "", fill = expression(italic(n))) +
  scale_y_continuous(breaks = seq(0,.3,.1),limits = c(0,.3)) + 
  ggthemes::theme_base(base_size = 8) +  theme(legend.position = "none") +
  theme(text = element_text(family = "CMU Serif", size = 12),
        axis.text.x = element_text(margin = margin(t = 5)),  # Increase space below x-axis label
        axis.text.y = element_text(margin = margin(r = 5)),
        plot.background = element_blank()
  ) + 
  theme(axis.title.x = element_text(margin = margin(t = 5))) +
  ggtitle(expression(italic(d)[italic(x)] == 32)) +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "italic")
  )

filtered_df <- FinalDF[FinalDF$algo != "boot" & FinalDF$d == 64, ]
df_remse <- filtered_df %>%
  group_by(algo, N, run) %>%
  summarize(remse = mean(remse), .groups = 'drop')

remse3 <- ggplot(df_remse, aes(x = algo, y = remse, fill = N)) +
  geom_boxplot() + 
  scale_x_discrete(labels = c("DaC-SMC", "NSMC", "STPF")) +
  labs(x = "Algorithms", y = "ReMSE", fill = expression(italic(n))) +
  scale_y_continuous(breaks = seq(0,.3,.1),limits = c(0,.3)) + 
  ggthemes::theme_base(base_size = 8) +  theme(legend.position = "bottom") +
  theme(text = element_text(family = "CMU Serif", size = 12),
        axis.text.x = element_text(margin = margin(t = 5)),  # Increase space below x-axis label
        axis.text.y = element_text(margin = margin(r = 5)),
        plot.background = element_blank()
  ) + theme(axis.title.x = element_text(margin = margin(t = 5))) +
  ggtitle(expression(italic(d)[italic(x)] == 64)) +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "italic")
  )

filtered_df <- FinalDF[FinalDF$algo != "boot" & FinalDF$d == 256, ]
df_remse <- filtered_df %>%
  group_by(algo, N, run) %>%
  summarize(remse = mean(remse), .groups = 'drop')

remse4 <- ggplot(df_remse, aes(x = algo, y = remse, fill = N)) +
  geom_boxplot() + 
  scale_x_discrete(labels = c("DaC-SMC", "NSMC", "STPF")) +
  labs(x = "Algorithms", y = "", fill = expression(italic(n))) +
  scale_y_continuous(breaks = seq(0,.3,.1),limits = c(0,.3)) + 
  ggthemes::theme_base(base_size = 8) +  theme(legend.position = "bottom") +
  theme(text = element_text(family = "CMU Serif", size = 12),
        axis.text.x = element_text(margin = margin(t = 5)),  # Increase space below x-axis label
        axis.text.y = element_text(margin = margin(r = 5)),
        plot.background = element_blank()
  ) +
  theme(axis.title.x = element_text(margin = margin(t = 5))) +
  ggtitle(expression(italic(d)[italic(x)] == 256)) +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "italic")
  )

remse <- grid.arrange(remse1, remse2, remse3, remse4, ncol=2, heights = c(1, 1.3))


ggsave("C:/Users/sanji/Downloads/Thesis/PLOTS/remse_10.pdf", plot = remse, width = 4, height = 4, dpi = 300)
