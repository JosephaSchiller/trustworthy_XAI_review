# Project: trustworthy_XAI_review
# Date 17.07.2024
# Last update: 24.07.2024
# Author: Josepha Schiller

# Task: Create figures for publication

#--------------------------------------------------------------------------------

# load libraries
library(tidyverse)
library(treemapify)

#--------------------------------------------------------------------------------

# load data
setwd()
df <- read.table(file = "XAI_method_comparison.csv", header = T, sep = ",")

# inspect variable type
str(df)

#--------------------------------------------------------------------------------

# plot
df$method <- reorder(df$method, -df$count)

fig3a <- ggplot(data=df, aes(x=method, y=count, fill = type)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("darkgreen", 
                             "orange",
                             "grey",
                             "darkblue")) +
  theme(axis.text.x = element_text(hjust = 1, size = 12),
        axis.text.y = element_text(hjust = 1, size = 12),
        panel.background = element_rect(fill = "white"),
        legend.position = c(0.7, 0.8),
        legend.text = element_text(size = 12)) +
  geom_text(aes(label=count), vjust=1, hjust= 0, color="black", size=3)+
  labs(x = "XAI method", y = "n. articles") +
  coord_flip()

fig3a

#-------------------------------------------------------------------------------

# save fig 3a

png("XAI_method_comparison.png", width = 8 * 100, height = 5 *100, units = "px", res = 100)
fig3a
dev.off()

svg("XAI_method_comparison.svg", width = 8, height = 5)
fig3a
dev.off()

#--------------------------------------------------------------------------------

# trustworthy distribution figure

#--------------------------------------------------------------------------------

# load data

setwd()
df2 <- read.table(file = "tab_trust_articles.csv", header = T, sep = ",")

#--------------------------------------------------------------------------------

# compute proportions
df2$prop <- df2$nr_articles/577*100



#--------------------------------------------------------------------------------

# create plot

fig3b <-  ggplot(df2, aes(area = prop, fill = trust)) +
  geom_treemap() +
  geom_treemap_text(aes(label = paste0(round(prop, 2), " %")), 
                    size = 12,  color = "white", 
                    place = "middle", reflow =  T, fontface = "bold") +
  theme(legend.position = c(0.2, 0.2), 
        legend.text = element_text(size = 12)) +
  scale_fill_manual(values = c("#E5323B", "#235FA4", "#0A284B")) # Dunkelgrau



fig3b
#--------------------------------------------------------------------------------

# save fig3b

png("trust_mentiones.png", width = 6 * 100, height = 5 *100, units = "px", res = 100)
fig3b
dev.off()

svg("trust_mentiones.svg", width = 6, height = 5)
fig3b
dev.off()

