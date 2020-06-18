# install.packages("tidyverse")
# install.packages("viridis")
# install.packages("sf")
# install.packages("ggthemes")
# install.packages("RColorBrewer")
# install.packages("ggplot2")
library(tidyverse)
library(viridis)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(ggplot2)
library(tigris)
library(data.table)


######## USDA Food Desert Atlas Data Geographies - 2017 #################


#
# Tigris Work ------------------------------------------------------------------------
#
usda_data <- fread("./data/working/atlas_patrick_county.csv")

usda_data <- usda_data %>%
  rename(GEOID = CensusTract)

usda_data$GEOID <- as.character(usda_data$GEOID)
usda_data <- as.data.frame(usda_data)

# load in tigris county tract shape files
tract_data <- tracts(51, 141, cb = FALSE, year = 2017)

# change from sp to sf data type
tract_data <- st_as_sf(tract_data)

# join and make sf
usda_geo_data <- usda_data %>%
  left_join(tract_data)
usda_geo_data <- st_as_sf(usda_geo_data)


# USDA plots -----------------------------------------------------------------------
# Vars: LAhalfand10 lahunv10share	lakids10share	lalowi10share	lapop10share	laseniors10share	LILATracts_1And10
# plot(st_geometry(usda_geo_data))

# Plots at 10 miles ------------------------------------------------------------
# LAhalfand10
min_LAhalfand10 <- floor(min(usda_geo_data$LAhalfand10))
max_LAhalfand10 <- ceiling(max(usda_geo_data$LAhalfand10))
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = LAhalfand10)) +
  labs(title = "Low Access Tract at 1/2 mile \nfor 10 miles",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_LAhalfand10, max_LAhalfand10), 
                        breaks = seq(min_LAhalfand10, max_LAhalfand10, length.out = 5))
ggsave(path = "./output/usda/", device = "png", filename = "plot_LAhalfand10.png", plot = last_plot())


# lahunv10share
min_lahunv10share <- floor(min(usda_geo_data$lahunv10share))
max_lahunv10share <- ceiling(max(usda_geo_data$lahunv10share))
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lahunv10share)) +
  labs(title = "Percent of housing units without or \n with low vehicle access at 10 miles",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_lahunv10share, max_lahunv10share), 
                        breaks = seq(min_lahunv10share, max_lahunv10share, length.out = 5))
ggsave(path = "./output/usda/", device = "png", filename = "plot_lahunv10share.png", plot = last_plot())

# lakids10share
min_lakids10share <- floor(min(usda_geo_data$lakids10share))
max_lakids10share <- ceiling(max(usda_geo_data$lakids10share))
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lakids10share)) +
  labs(title = "Percentage of children with low food access at 10 miles",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_lakids10share, max_lakids10share), 
                        breaks = seq(min_lakids10share, max_lakids10share, length.out = 5))
ggsave(path = "./output/usda/", device = "png", filename = "plot_lakids10share.png", plot = last_plot())

# lalowi10share
min_lalowi10share <- floor(min(usda_geo_data$lalowi10share))
max_lalowi10share <- ceiling(max(usda_geo_data$lalowi10share))
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lalowi10share)) +
  labs(title = "Percentage of low income persons \nwith low food access at 10 miles",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_lalowi10share, max_lalowi10share), 
                        breaks = seq(min_lalowi10share, max_lalowi10share, length.out = 5))
ggsave(path = "./output/usda/", device = "png", filename = "plot_lalowi10share.png", plot = last_plot())

# lapop10share
min_lapop10share <- floor(min(usda_geo_data$lapop10share))
max_lapop10share <- ceiling(max(usda_geo_data$lapop10share))
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lapop10share)) +
  labs(title = "Percentage of population with \nlow food access at 10 miles",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_lapop10share, max_lapop10share), 
                        breaks = seq(min_lapop10share, max_lapop10share, length.out = 5))
ggsave(path = "./output/usda/", device = "png", filename = "plot_lapop10share.png", plot = last_plot())

# laseniors10share
min_laseniors10share <- floor(min(usda_geo_data$laseniors10share))
max_laseniors10share <- ceiling(max(usda_geo_data$laseniors10share))
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = laseniors10share)) +
  labs(title = "Percentage of seniors with \nlow food access at 10 miles",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_laseniors10share, max_laseniors10share), 
                        breaks = seq(min_laseniors10share, max_laseniors10share, length.out = 5))
ggsave(path = "./output/usda/", device = "png", filename = "plot_laseniors10share.png", plot = last_plot())

# LILATracts_1And10
min_LILATracts_1And10 <- floor(min(usda_geo_data$LILATracts_1And10))
max_LILATracts_1And10 <- ceiling(max(usda_geo_data$LILATracts_1And10))
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = LILATracts_1And10)) +
  labs(title = "Low income and low access at 10 miles",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_LILATracts_1And10, max_LILATracts_1And10), 
                        breaks = seq(min_LILATracts_1And10, max_LILATracts_1And10, length.out = 5))
ggsave(path = "./output/usda/", device = "png", filename = "plot_LILATracts_1And10.png", plot = last_plot())

# Plots at 20 miles --------------------------------------------------

# LAhalfand20 does not exist

# lahunv20sharem- ALL ZEROES
# min_lahunv20share <- floor(min(usda_geo_data$lahunv20share))
# max_lahunv20share <- ceiling(max(usda_geo_data$lahunv20share))
# ggplot() +
#   geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lahunv20share)) +
#   labs(title = "Percent of housing units without or \n with low vehicle access at 20 miles",
#        caption = "Source: USDA Food Access Research Atlas, 2017") +
#   theme_map() +
#   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#         legend.title = element_text(size = 11, face = "bold"),
#         legend.text = element_text(size = 11),
#         legend.position = "right") +
#   scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
#                         limits = c(min_lahunv20share, max_lahunv20share), 
#                         breaks = seq(min_lahunv20share, max_lahunv20share, length.out = 5))
# ggsave(path = "./output/usda/", device = "png", filename = "plot_lahunv20share.png", plot = last_plot())
# 
# # lakids20share - ALL ZEROES
# min_lakids20share <- floor(min(usda_geo_data$lakids20share))
# max_lakids20share <- ceiling(max(usda_geo_data$lakids20share))
# ggplot() +
#   geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lakids20share)) +
#   labs(title = "Percentage of children with \nlow food access at 20 miles",
#        caption = "Source: USDA Food Access Research Atlas, 2017") +
#   theme_map() +
#   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#         legend.title = element_text(size = 11, face = "bold"),
#         legend.text = element_text(size = 11),
#         legend.position = "right") +
#   scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
#                         limits = c(min_lakids20share, max_lakids20share), 
#                         breaks = seq(min_lakids20share, max_lakids20share, length.out = 5))
# ggsave(path = "./output/usda/", device = "png", filename = "plot_lakids20share.png", plot = last_plot())
# 
# # lalowi20share - ALL ZEROES
# min_lalowi20share <- floor(min(usda_geo_data$lalowi20share))
# max_lalowi20share <- ceiling(max(usda_geo_data$lalowi20share))
# ggplot() +
#   geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lalowi20share)) +
#   labs(title = "Percentage of children with \nlow food access at 20 miles",
#        caption = "Source: USDA Food Access Research Atlas, 2017") +
#   theme_map() +
#   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#         legend.title = element_text(size = 11, face = "bold"),
#         legend.text = element_text(size = 11),
#         legend.position = "right") +
#   scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
#                         limits = c(min_lalowi20share, max_lalowi20share), 
#                         breaks = seq(min_lalowi20share, max_lalowi20share, length.out = 5))
# ggsave(path = "./output/usda/", device = "png", filename = "plot_lalowi20share.png", plot = last_plot())
# 
# # lapop20share - ALL ZEROES
# min_lapop20share <- floor(min(usda_geo_data$lapop20share))
# max_lapop20share <- ceiling(max(usda_geo_data$lapop20share))
# ggplot() +
#   geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lapop10share)) +
#   labs(title = "Percentage of population with \nlow food access at 20 miles",
#        caption = "Source: USDA Food Access Research Atlas, 2017") +
#   theme_map() +
#   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#         legend.title = element_text(size = 11, face = "bold"),
#         legend.text = element_text(size = 11),
#         legend.position = "right") +
#   scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
#                         limits = c(min_lapop20share, max_lapop20share), 
#                         breaks = seq(min_lapop20share, max_lapop20share, length.out = 5))
# ggsave(path = "./output/usda/", device = "png", filename = "plot_lapop20share.png", plot = last_plot())
# 
# # laseniors20share - ALL ZEROES
# min_laseniors20share <- floor(min(usda_geo_data$laseniors20share))
# max_laseniors20share <- ceiling(max(usda_geo_data$laseniors20share))
# ggplot() +
#   geom_sf(data = usda_geo_data, size = 0.2, aes(fill = laseniors20share)) +
#   labs(title = "Percentage of seniors with \nlow food access at 20 miles",
#        caption = "Source: USDA Food Access Research Atlas, 2017") +
#   theme_map() +
#   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#         legend.title = element_text(size = 11, face = "bold"),
#         legend.text = element_text(size = 11),
#         legend.position = "right") +
#   scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
#                         limits = c(min_laseniors20share, max_laseniors20share), 
#                         breaks = seq(min_laseniors20share, max_laseniors20share, length.out = 5))
# ggsave(path = "./output/usda/", device = "png", filename = "plot_laseniors20share.png", plot = last_plot())
# 
# # LILATracts_1And20 - ALL ZEROES
# min_LILATracts_1And20 <- floor(min(usda_geo_data$LILATracts_1And20))
# max_LILATracts_1And20 <- ceiling(max(usda_geo_data$LILATracts_1And20))
# ggplot() +
#   geom_sf(data = usda_geo_data, size = 0.2, aes(fill = LILATracts_1And20)) +
#   labs(title = "Low income and low access at 20 miles",
#        caption = "Source: USDA Food Access Research Atlas, 2017") +
#   theme_map() +
#   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#         legend.title = element_text(size = 11, face = "bold"),
#         legend.text = element_text(size = 11),
#         legend.position = "right") +
#   scale_fill_continuous(name = "", low = "#fff7ec", high = "#7F0000",
#                         limits = c(min_LILATracts_1And20, max_LILATracts_1And20), 
#                         breaks = seq(min_LILATracts_1And20, max_LILATracts_1And20, length.out = 5))
# ggsave(path = "./output/usda/", device = "png", filename = "plot_LILATracts_1And20.png", plot = last_plot())

# Plots at 1 mile ------------------------------------------------------------

# lahunv1share
min_lahunv1share <- floor(min(usda_geo_data$lahunv1share))
max_lahunv1share <- ceiling(max(usda_geo_data$lahunv1share))
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lahunv10share)) +
  labs(title = "Percent of housing units without or \n with low vehicle access at 1 mile",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_lahunv1share, max_lahunv1share), 
                        breaks = seq(min_lahunv1share, max_lahunv1share, length.out = 5))
ggsave(path = "./output/usda/", device = "png", filename = "plot_lahunv1share.png", plot = last_plot())

# lakids1share - change scale
min_lakids1share <- floor(min(usda_geo_data$lakids1share))
max_lakids1share <- ceiling(max(usda_geo_data$lakids1share))
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lakids1share)) +
  labs(title = "Percentage of children with low food access at 1 mile",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_lakids1share, max_lakids1share), 
                        breaks = seq(min_lakids1share, max_lakids1share, length.out = 5))
ggsave(path = "./output/usda/", device = "png", filename = "plot_lakids1share.png", plot = last_plot())

# lalowi1share
min_lalowi1share <- floor(min(usda_geo_data$lalowi1share))
max_lalowi1share <- ceiling(max(usda_geo_data$lalowi1share))
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lalowi1share)) +
  labs(title = "Percentage of low income persons \nwith low food access at 10 miles",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_lalowi1share, max_lalowi1share), 
                        breaks = seq(min_lalowi1share, max_lalowi1share, length.out = 5))
ggsave(path = "./output/usda/", device = "png", filename = "plot_lalowi1share.png", plot = last_plot())

# lapop1share - change scale
min_lapop1share <- floor(min(usda_geo_data$lapop1share))
max_lapop1share <- ceiling(max(usda_geo_data$lapop1share))
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lapop1share)) +
  labs(title = "Percentage of population with \nlow food access at 1 mile",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_lapop1share, max_lapop1share), 
                        breaks = seq(min_lapop1share, max_lapop1share, length.out = 5))
ggsave(path = "./output/usda/", device = "png", filename = "plot_lapop1share.png", plot = last_plot())

# laseniors1share - change scale
min_laseniors1share <- floor(min(usda_geo_data$laseniors1share))
max_laseniors1share <- ceiling(max(usda_geo_data$laseniors1share))
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = laseniors1share)) +
  labs(title = "Percentage of seniors with \nlow food access at 1 mile",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_laseniors1share, max_laseniors1share), 
                        breaks = seq(min_laseniors1share, max_laseniors1share, length.out = 5))
ggsave(path = "./output/usda/", device = "png", filename = "plot_laseniors1share.png", plot = last_plot())



