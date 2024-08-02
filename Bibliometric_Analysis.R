# Project: trustworthy_XAI_review
# Date:2024-05-10 
# Last update:2024-08-02
# Author: Josepha Schiller

# Task: Conduct bibliometric analysis

# Note: 
# The script was provided by Ryo 2023  (DOI: 10.1111/1440-1703.12425) 
# I adjusted it according to the research task. 

# There is a tutorial for the bibliometric analysis by the provider of the bibliometrix R packages.
# https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html 

#--------------------------------------------------------------------------------

# load libraries
library(tidyverse)
library(bibliometrix)
library(Matrix)
library(igraph)
library(ggpubr)
library(cowplot)
library(gridExtra)

#--------------------------------------------------------------------------------

# load relevant files

setwd()
file1 <- "20230730_schiller.bib"

#--------------------------------------------------------------------------------

# process and transform data

# convert file 
M_tmp1 <- convert2df(file = file1, dbsource = "wos", format = "bibtex") # give specification to convert bib text into df
M <- M_tmp1

# filter the time period
M_all <- M %>% filter(PY >= 2015) # removal of 20 articles

#--------------------------------------------------------------------------------

# get summary information

summary_list <- M_all %>% biblioAnalysis() %>% summary(k=10)

summary_list$MainInformationDF
summary_list$AnnualProduction
summary_list$MostProdAuthors
summary_list$MostProdCountries
summary_list$TCperCountries
summary_list$AnnualGrowthRate

#--------------------------------------------------------------------------------

# load relevant sources

synonym_list <- c( 
  
  "biodiversity;diversity;biobiodiversity", 
  "communities;community",
  "data; dataset",
  "convolutional neural networks;convolutional neural network;network (cnn);convolutional neural;convolutional neural network (cnn);convolution; cnn; convolutional neural-networks",
  
  "ecosystems;ecosystem",
  "forests;forest",
  "impacts;impact",
  "prediction;predictions",
  "models;model; data models", 
  "modelling;modeling",
  "method; technique",
  "neural network; networks; network; neural-networks;neural-network;neural networks;artificial neural network;neural networks;artificial neural networks;artificial neural-network;artificial neural-networks;artificial neural; ann",
  
  "plant;plants",
  "population;populations",
  "pattern;patterns",
  "random forest;random forests",
  "response;responses",
  "system;systems", 
  "scale;scales", 
  "species distribution;distribution;distributions;potential distribution;species distributions",
  "species distribution model;species distribution models;distribution models; distribution model",
  "tree;trees",
  "vegetation;vegetations",
  "sic; (sic)(sic)(sic)(sic)(sic)(sic)",
  "algorithm; algorithms",
  "explainable artificial intelligence; explainable AI;explainable AI (xai); xai;explainable artificial intelligence (xai); explainable artificial;explainable machine learning",
  
  "interpretability; explainability; explainable; interpretable; model interpretation; interpretable model",
  
  "artificial intelligence;AI;artificial intelligence (ai);artifical intelligence",
  "extreme gradient boosting; xgboost",
  "interpretable machine learning;interpretable machine; interpretable deep learning",
  "hyperspectral image; hyperspectral imaging",
  "machine learning;machine learning (ml)",
  "deep learning; deep",
  "pm2.5 concentration;pm2.5;pm2.5 concentrations",
  "shap;shapley additive explanations;shapley additive explanations (shap)",
  "synthetic aperture radar;synthetic aperture radar (sar);sar",
  "nested residual u-net; (nru)",
  "lstm;long short-term",
  "aerosal optical depth; aod")




# define removal list
remove_list_trends <- c("system",
                        "area",
                        "size",
                        "modelling",
                        "model",
                        "data",
                        "models",
                        "machine",
                        "explainable artificial intelligence",
                        "interpretable machine learning",
                        "data science",
                        "interpretable",
                        "interpretability",
                        "analysis",
                        "intelligence",
                        "learning",
                        "machine learning",
                        "artificial intelligence",
                        "artificial-intelligence",
                        "part i",
                        "machine learning",
                        "algorithm",
                        "method")


# define synonym list


# -------------------------------------------------------------------------------

# Keyword trend

# customize function
keyword_trend <- function(M,n.items=5, field="AU", min.freq = 1, timespan=c(2021,2030)){
  fieldByYear(M,
              field=field,
              n.items=n.items,
              synonyms=synonym_list,              #adjust manually
              remove.terms = remove_list_trends,  #adjust manually
              timespan=timespan)
}

# apply
M_all %>% keyword_trend(field="ID", n.items=5,timespan=c(2000,2023)) #WoS keyword plus

#-------------------------------------------------------------------------------

# Keyword co-occurrence


# customize function 
Keyword_cooccurrence = function(M, keywords, remove_list, com.rep=0.1){
  windowsFonts("Arial" = windowsFont("Arial"))
  set.seed(1)
 # par(bg = "white")
  M %>% 
    biblioNetwork(., 
                  analysis = "co-occurrences", 
                  network = keywords, #"author_keywords",  # it becomes much more fragmented.
                  #                  network = "keywords", #WOS defined keywords
                  sep = ";",
                  synonyms = synonym_list,
                  remove.terms = remove_list) %>% 
    networkPlot(.,  
                normalize = "jaccard", #try "association"
                weighted=T, 
                n = 80,
                cluster="louvain",
              #  Title = paste("Keyword Co-occurrences:", Subset, ";", keywords), 
                community.repulsion = com.rep,
                remove.isolates = T,  #
                remove.multiple = T,
                type = "fruchterman", 
                size=T, 
                labelsize=1,
                label.cex = T,
                curved = 0.2,
                alpha=0.4,
                verbose = F) %>% 
    .$graph %>% 
    plot(.,  # with igraph for more flexibility
         vertex.label.color = rgb(0, 0, 0, alpha = 0.7),
         vertex.label.family = "Arial",
         vertex.frame.color="NA") 
}

#-------------------------------------------------------------------------------

# apply
M_all %>%  Keyword_cooccurrence("keywords", remove_list_trends, com.rep=0.1)

#-------------------------------------------------------------------------------

# analyse the number of publications per discipline

#-------------------------------------------------------------------------------

# Subject Category
M_all$SC %>% unique()
M_all$web.of.science.categories. %>% unique()

# prepare for plottinh
A <- cocMatrix(M_all, Field = "SC", binary = FALSE, remove.terms = remove.terms, synonyms = synonyms)
n <- colSums(as.array(A))
df <- data.frame(names = names(n), values = n)

df$names <- reorder(df$names, -df$values)


# plot 
fig <- ggplot(data=df, aes(x=names, y=values)) +
  geom_bar(stat="identity",fill = "#EEEDED") +
  theme(axis.text.x = element_text( hjust = 1, size = 10),
        axis.text.y = element_text( hjust = 1, size = 11),
        panel.background = element_rect(fill = "white")) +
  geom_text(aes(label=values), vjust=0, color="#0D1282", size=2.8)+
  labs(x = "WoS category", y = "n. articles") +
  coord_flip()

fig
