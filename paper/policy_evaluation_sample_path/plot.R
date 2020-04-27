library(dplyr)
library(ggplot2)
library(grid)
library(tikzDevice)

g_values_Model1 <- read.csv("g_values_Model1.csv") %>% dplyr::pull(g_model1)
g_values_Model2 <- read.csv("g_values_Model2.csv") %>% dplyr::pull(g_model2)

g_values <- data.frame(model_1 = g_values_Model1, model_2 = g_values_Model2)
plot_dat <- reshape2::melt(g_values) %>%
  dplyr::mutate(variable = ifelse(variable == "model_1", "Model I", "Model II"))

mu <- plot_dat %>% 
  dplyr::group_by(variable) %>%
  dplyr::summarise(grp.mean = mean(value)) 

tikzDevice::tikz("plot_g_distribution1.tex", width = 10, height = 7, standAlone=F)
ggplot2::ggplot(plot_dat, aes(x = value)) + 
  geom_density(aes(fill = variable), alpha = 0.4) +
  geom_vline(aes(xintercept = grp.mean, color = variable),
             data = mu, linetype = "dashed") +
  scale_color_manual(values = c("gray27", "gray65"))+
  scale_fill_manual(values = c("gray27", "gray65")) + 
  xlab("Average reward per time unit(g)") + ylab("Density") + 
  ggplot2::geom_vline(xintercept = g_opt_ssm) + 
  scale_x_continuous(breaks = seq(-100, 500, by = 50)) + 
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        panel.background = element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        #legend.key = element_rect(fill = NA, colour = NA),
        #legend.key.width = unit(2, "cm"), legend.text.align=0.5, axis.title.x= element_text(vjust = -0.7),
        strip.background=element_rect(fill = NA))
dev.off()
