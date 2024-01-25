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

### Load the final, merged dataset (here, for tau=1, lambda=0)
FinalDF <- read.csv("C:/Users/sanji/Downloads/Thesis/PLOTS/FinalDF_10")
FinalDF$d <- as.factor(FinalDF$d)
FinalDF$N <- as.factor(FinalDF$N)
FinalDF$run <- as.factor(FinalDF$run)

### KS distance###
filtered_df <- FinalDF[FinalDF$algo != "boot" & FinalDF$d == 16, ]
df_ks <- filtered_df %>%
  group_by(algo, N, run) %>%
  summarize(ks = mean(ks), .groups = 'drop')

ks1 <- ggplot(df_ks, aes(x = algo, y = ks, fill = N)) +
  geom_boxplot() + 
  scale_x_discrete(labels = c("DaC-SMC", "NSMC", "STPF")) +
  labs(x = "", y = "KS distance", fill = expression(italic(n))) +
  scale_y_continuous(breaks = seq(0,.1,.02),limits = c(.05,.1)) + 
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
df_ks <- filtered_df %>%
  group_by(algo, N, run) %>%
  summarize(ks = mean(ks), .groups = 'drop')

ks2 <- ggplot(df_ks, aes(x = algo, y = ks, fill = N)) +
  geom_boxplot() + 
  scale_x_discrete(labels = c("DaC-SMC", "NSMC", "STPF")) +
  labs(x = "", y = "", fill = expression(italic(n))) +
  scale_y_continuous(breaks = seq(0,.1,.02),limits = c(.05,.1)) + 
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
df_ks <- filtered_df %>%
  group_by(algo, N, run) %>%
  summarize(ks = mean(ks), .groups = 'drop')

ks3 <- ggplot(df_ks, aes(x = algo, y = ks, fill = N)) +
  geom_boxplot() + 
  scale_x_discrete(labels = c("DaC-SMC", "NSMC", "STPF")) +
  labs(x = "Algorithms", y = "KS distance", fill = expression(italic(n))) +
  scale_y_continuous(breaks = seq(0,.1,.02),limits = c(.05,.1)) + 
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
df_ks <- filtered_df %>%
  group_by(algo, N, run) %>%
  summarize(ks = mean(ks), .groups = 'drop')

ks4 <- ggplot(df_ks, aes(x = algo, y = ks, fill = N)) +
  geom_boxplot() + 
  scale_x_discrete(labels = c("DaC-SMC", "NSMC", "STPF")) +
  labs(x = "Algorithms", y = "", fill = expression(italic(n))) +
  scale_y_continuous(breaks = seq(0,.1,.02),limits = c(.05,.1)) + 
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

ks <- grid.arrange(ks1, ks2, ks3, ks4, ncol=2, heights = c(1, 1.3))

ggsave("C:/Users/sanji/Downloads/Thesis/PLOTS/ks.pdf", plot = ks, width = 4, height = 4, dpi = 300)
