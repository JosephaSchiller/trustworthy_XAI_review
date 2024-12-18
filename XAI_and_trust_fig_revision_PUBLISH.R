# Project: XAI Review Article
# Date 17.07.2024
# Last update: 11.12.2024
# author: Josepha Schiller

# Task: create barplot to compare mentions of XAI methods

#--------------------------------------------------------------------------------

# load libraries
library(tidyverse)
library(readxl)
library(waffle) # not used for published figure
library(treemapify) # treemap
library(patchwork) # combining figures

#--------------------------------------------------------------------------------

# load data
setwd("")
df <- read.table(file = "XAI_method_comparison_revised.csv", header = T, sep = ",")

# inspect variable type
str(df)

#--------------------------------------------------------------------------------

# plot

df$method <- reorder(df$method, -df$count)


fig3 <- ggplot(data=df, aes(x=method, y=count, fill = type)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("darkgreen", 
                             "orange",
                             "grey",
                             "darkblue")) +
  theme(axis.text.x = element_text(hjust = 1, size = 16),
        axis.text.y = element_text(hjust = 1, size = 16),
        panel.background = element_rect(fill = "white"),
        legend.position = c(0.7, 0.8),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        axis.title = element_text(size = 18)) +
  geom_text(aes(label=count), vjust=1, hjust= 0, color="black", size=3)+
  labs(x = "XAI method", y = "n. articles") +
  coord_flip()


fig3

#-------------------------------------------------------------------------------

# save fig 3

png("XAI_method_comparison.png", width = 8 * 100, height = 5 *100, units = "px", res = 100)
fig3
dev.off()

ggsave(
  filename = "XAI_method_comparison.png",
  plot = fig3, 
  width = 8,
  height = 5,
  dpi = 1200
)

svg("XAI_method_comparison.svg", width = 8, height = 5)
fig3
dev.off()

#--------------------------------------------------------------------------------

# trustworthy distribution figure

#--------------------------------------------------------------------------------

# load data

setwd("")
df2 <- read.table(file = "tab_trust_articles.csv", header = T, sep = ",")

#--------------------------------------------------------------------------------

# compute proportions
df2$prop <- df2$nr_articles/577*100


#--------------------------------------------------------------------------------

# create plot

# first version 
fig4 <- ggplot(df2, aes(fill=trust, values=prop)) +
  geom_waffle(color = "white", make_proportional = T) +
  scale_fill_manual(
    values = c("darkblue", "orange", "grey")) +
  theme_void()

fig4

# second version 
fig4 <-  ggplot(df2, aes(area = prop, fill = trust)) +
  geom_treemap() +
  geom_treemap_text(aes(label = paste0(round(prop, 2), " %")), 
                    size = 20,  color = "white", 
                    place = "middle", reflow =  T, fontface = "bold") +
  theme(legend.position = c(0.2, 0.2), 
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 22),
        legend.key.size = unit(1.5, "cm")) +
  #scale_fill_manual(values = c("#696969", "#A9A9A9","#D3D3D3" ))  # Set the color palette
  #scale_fill_manual(values = c("#0072B2","#E69F00", "#999999")) # Dunkelgrau
  scale_fill_manual(values = c("#E5323B", "#235FA4", "#0A284B")) # Dunkelgrau

fig4
#--------------------------------------------------------------------------------

# save fig 4
setwd("")

png("trust_mentions.png", width = 6 * 100, height = 5 *100, units = "px", res = 100)
fig4
dev.off()

# Save the plot
ggsave(
  filename = "trust_mentions.png",
  plot = fig4, 
  width = 6,
  height = 5,
  dpi = 1200
)

svg("trust_mentions.svg", width = 6, height = 5)
fig4
dev.off()


# --------------------------------------------------------------------------------

# read in the dataframe to inspect method combinations

setwd("")
results <- read.csv("20241209_effectiveness_check.csv")
colnames(results)

# --------------------------------------------------------------------------------

# filter
results1 <- results %>% select(SUM, Authors, LIME, VI, PDP, ALE, Saliency.Maps, SHAP, WoS.Categories)
results1 <- results1 %>% filter(SUM > 0) #check for methods was done manually, hence some articles should be removed


# Create histogram of number of XAI methods used per paper
fig5 <- ggplot(results1, aes(x = factor(SUM))) +
  geom_bar(fill = "#235FA4", color = NA, width = 0.7) +
  labs(x = "n. XAI methods", 
       y = "n. articles") +
  theme(axis.text.x = element_text(hjust = 1, size = 16),
        axis.text.y = element_text(hjust = 1, size = 16),
        panel.background = element_rect(fill = "white"),
        # Remove legend settings since there is no legend in a simple bar plot
        axis.title = element_text(size = 18)) +
  # Get the count data first
  geom_text(stat = 'count', aes(label = ..count..), 
             vjust=1, hjust= 0, color = "black", size = 5) +
  coord_flip()

fig5


# Save the plot
ggsave(
  filename = "number_of_xai_methods.png",
  plot = fig5, 
  width = 10,
  height = 8,
  dpi = 1200
)


# --------------------------------------------------------------------------------

# combine figures

combined_fig <- (fig3 + fig5) + 
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = 'A')

combined_fig


# Save combined figure
ggsave(
  filename = "combined_xai_figures2.png",
  plot = combined_fig,
  width = 11,
  height = 10,
  dpi = 1200
)

# Save as SVG
ggsave(
  filename = "combined_xai_figures.svg", 
  plot = combined_fig,
  width = 18,
  height = 10
)


