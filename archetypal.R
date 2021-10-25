# Import data and load libraries -----------

# Required libraries
library(archetypes)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(dplyr)
library(Hmisc)
library(GGally)
# Import data
df <- readxl::read_excel(
  "https://zenodo.org/record/4767452/files/dataset_archetypes.csv?download=1",
  sheet = "Data",
  col_types = c(
    "numeric",
    "skip",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "text",
    "text"
  )
)

df.pubs <- df[complete.cases(df),] # Remove NA cases

df.pubs$pp_news <- df.pubs$p_news/df.pubs$PUBS
df.pubs$pp_policy <- df.pubs$p_policy/df.pubs$PUBS
df.pubs$pp_oa <- df.pubs$OA/df.pubs$PUBS
df.pubs$pp_twitter <- df.pubs$p_twitter/df.pubs$PUBS

df.pubs <- df.pubs %>% # Select and rename variables
  select(ID_FINAL, translation, dissemination, engagement,
         production, pp_news, pp_oa, pp_policy, pp_twitter,
         transmission, field) %>%
  filter(field!="NA")
names(df.pubs) <- c("ID_FINAL",
                    "Commercialisation",
                    "Dissemination",
                    "Engagement",
                    "Joint research",
                    "Media promotion",
                    "Openness",
                    "Public Policy",
                    "Social visibility",
                    "Transmission",
                    "field")

# Descriptives - Global ------------
# Descriptive table by broad fields
 df.vars <- df.pubs %>%
  tidyr::pivot_longer(!field & !ID_FINAL, 
                      names_to = "variable",
                      values_to = "value")

df.vars %>%
  group_by(field, variable) %>%
summarise(n = n(), mean = mean(value), sd = sd(value)) %>%
  clipr::write_clip()
 
# Distributions and correlations
  
  ggpairs(df.pubs, columns=2:10, aes(colour = field)) + 
    theme_bw() + scale_color_brewer(palette = "Dark2")
  
# Archetypal Analyses ---- 
# Check which is the best k
library(archetypes)

all <- df.pubs %>%
  select(!field & !ID_FINAL)

set.seed(1986)
arc.k <- stepArchetypes(data = all, 
                        k = 1:5, 
                        verbose =F, 
                        nrep = 5)

# RSS (Residual Sum of Squares)

screeplot(arc.k) # 3 archetypes

arc.model <- bestModel(arc.k[[3]])

# Traspose for better readibility
t(parameters(arc.model))



param <- parameters(arc.model)
param <- ifelse(param < 0, 0, param)
param <- as.data.frame(param)

# Convert parameters into percentiles - MAAAAAL
for (i in 1:ncol[,9]) {
  max <- max(all[,i])
  min <- min(all[,i])
  parameter <- param[,i]
  vector <- do.call(paste, c(min, parameter, max))
  percs <- fsmb::percentile(min_param_max)
}

#normalize
source("~/R/functions/min_max_norm.R")

for (i in 1:ncol(param[,1:9])) {
  param_norm[,i] <- fsmb::percentile(param[,i])
}
param$archetype <- c(1,2,3)
param_norm$archetype <- c(1,2,3)

param_norm <-
  tidyr::pivot_longer(param_norm,
                      !c("archetype"),
                      names_to = "variable",
                      values_to = "value")

a1 <- ggplot(subset(param_norm, archetype == 1)) +
  geom_col(aes(x = variable, y = value), fill = "#F25200") +
  labs(title = "Archetype 1", 
       x = element_blank(), 
       y = element_blank()) +
  coord_polar() +
  theme_minimal()

a2 <- ggplot(subset(param_norm, archetype == 2)) +
  geom_col(aes(x = variable, y = value), fill = "#F25200") +
  labs(title = "Archetype 2", 
       x = element_blank(), 
       y = element_blank()) +
  coord_polar() +
  theme_minimal()
  
a3 <- ggplot(subset(param_norm, archetype == 3)) +
  geom_col(aes(x = variable, y = value), fill = "#F25200") +
  labs(title = "Archetype 3", 
       x = element_blank(), 
       y = element_blank()) +
  coord_polar() +
  theme_minimal()


# Add alpha scores to id_extra
alphas <- arc.model[["alphas"]]

alphas <- cbind.data.frame(df.pubs$ID_FINAL, alphas)
write.csv(alphas, file = "alpha-all-respondents.csv", quote = F, 
          row.names = F,)

# Archetypal by discipline------------
# STEM
set.seed(1986)
df.stem <- df.pubs %>%
  filter(field=="STEM") %>%
  select(!field & !ID_FINAL)
arc.k <- stepArchetypes(data = df.stem, k = 1:5, nrep = 5)

# RSS (Residual Sum of Squares)

screeplot(arc.k) # 3 archetypes

arc.model <- archetypes::bestModel(arc.k[[3]])

# Traspose for better readibility
t(archetypes::parameters(arc.model))

# Create polar chart

param <- parameters(arc.model)
param <- ifelse(param < 0, 0, param)
param <- as.data.frame(param)
param$archetype <- c(1,2,3)

#normalize
param_norm <- min_max_norm(param)
param$archetype <- c(1,2,3)
param_norm$archetype <- c(1,2,3)

param_norm <-
  tidyr::pivot_longer(param_norm,
                      !c("archetype"),
                      names_to = "variable",
                      values_to = "value")

a1 <- ggplot(subset(param_norm, archetype == 1)) +
  geom_col(aes(x = variable, y = value), fill = "#00BAB4") +
  labs(title = "Archetype 1", 
       x = element_blank(), 
       y = element_blank()) +
  coord_polar() +
  theme_minimal()

a2 <- ggplot(subset(param_norm, archetype == 2)) +
  geom_col(aes(x = variable, y = value), fill = "#00BAB4") +
  labs(title = "Archetype 2", 
       x = element_blank(), 
       y = element_blank()) +
  coord_polar() +
  theme_minimal()

a3 <- ggplot(subset(param_norm, archetype == 3)) +
  geom_col(aes(x = variable, y = value), fill = "#00BAB4") +
  labs(title = "Archetype 3", 
       x = element_blank(), 
       y = element_blank()) +
  coord_polar() +
  theme_minimal()


# Get alpha scores and merge
alphas <- arc.model[["alphas"]]

df.stem <- subset (df.pubs,
                   field=="STEM",
                   select =
                     c("ID_FINAL"))
alphas <- cbind.data.frame(df.stem, alphas)

write.csv(alphas, file = "alpha-stem-respondents.csv", quote = F, 
          row.names = F)

# SSH
df.ssh <- df.pubs %>%
  filter(field=="SSH") %>%
  select(!field & !ID_FINAL)
set.seed(1986)
arc.k <- stepArchetypes(data = df.ssh, k = 1:5, nrep = 5)

# RSS (Residual Sum of Squares)

screeplot(arc.k) # 3 archetypes

arc.model <- bestModel(arc.k[[3]])

# Traspose for better readibility
t(parameters(arc.model))

# Create polar chart

param <- parameters(arc.model)
param <- ifelse(param < 0, 0, param)
param <- as.data.frame(param)

#normalize
param_norm <- min_max_norm(param)
param$archetype <- c(1,2,3)
param_norm$archetype <- c(1,2,3)

param_norm <-
  tidyr::pivot_longer(param_norm,
                      !c("archetype"),
                      names_to = "variable",
                      values_to = "value")

a1 <- ggplot(subset(param_norm, archetype == 1)) +
  geom_col(aes(x = variable, y = value), fill = "#304160") +
  labs(title = "Archetype 1", 
       x = element_blank(), 
       y = element_blank()) +
  coord_polar() +
  theme_minimal()

a2 <- ggplot(subset(param_norm, archetype == 2)) +
  geom_col(aes(x = variable, y = value), fill = "#304160") +
  labs(title = "Archetype 2", 
       x = element_blank(), 
       y = element_blank()) +
  coord_polar() +
  theme_minimal()

a3 <- ggplot(subset(param_norm, archetype == 3)) +
  geom_col(aes(x = variable, y = value), fill = "#304160") +
  labs(title = "Archetype 3", 
       x = element_blank(), 
       y = element_blank()) +
  coord_polar() +
  theme_minimal()

# Get alpha scores and merge
alphas <- arc.model[["alphas"]]

df.ssh <- subset (df.pubs,
                   field=="SSH",
                   select =
                     c("ID_FINAL"))
alphas <- cbind.data.frame(df.ssh, alphas)

write.csv(alphas, file = "alpha-ssh-respondents.csv", quote = F, 
          row.names = F)

# BIOMED
df.biomed <- df.pubs %>%
  filter(field=="BIOMED") %>%
  select(!field & !ID_FINAL)

set.seed(1986)
arc.k <- stepArchetypes(data = df.biomed, k = 1:5, nrep = 5)

# RSS (Residual Sum of Squares)

screeplot(arc.k) # 3 archetypes

arc.model <- bestModel(arc.k[[3]])

# Traspose for better readibility
t(parameters(arc.model))

# Create polar chart

param <- parameters(arc.model)
param <- ifelse(param < 0, 0, param)
param <- as.data.frame(param)

#normalize
param_norm <- min_max_norm(param)
param$archetype <- c(1,2,3)
param_norm$archetype <- c(1,2,3)

param_norm <-
  tidyr::pivot_longer(param_norm,
                      !c("archetype"),
                      names_to = "variable",
                      values_to = "value")

a1 <- ggplot(subset(param_norm, archetype == 1)) +
  geom_col(aes(x = variable, y = value), fill = "#ADA16B") +
  labs(title = "Archetype 1", 
       x = element_blank(), 
       y = element_blank()) +
  coord_polar() +
  theme_minimal()

a2 <- ggplot(subset(param_norm, archetype == 2)) +
  geom_col(aes(x = variable, y = value), fill = "#ADA16B") +
  labs(title = "Archetype 2", 
       x = element_blank(), 
       y = element_blank()) +
  coord_polar() +
  theme_minimal()

a3 <- ggplot(subset(param_norm, archetype == 3)) +
  geom_col(aes(x = variable, y = value), fill = "#ADA16B") +
  labs(title = "Archetype 3", 
       x = element_blank(), 
       y = element_blank()) +
  coord_polar() +
  theme_minimal()

# Get alpha scores and merge
alphas <- arc.model[["alphas"]]

df.biomed <- subset (df.pubs,
                  field=="BIOMED",
                  select =
                    c("ID_FINAL"))
alphas <- cbind.data.frame(df.biomed, alphas)

write.csv(alphas, file = "alpha-biomed-respondents.csv", quote = F, 
          row.names = F)

