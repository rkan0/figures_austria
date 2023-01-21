library(httr)
library(jsonlite)
library(cowplot)
library(tidyverse)
library(scales)
library(ggtext)
library(ggsci)
library(ggpubr)

# Anzahl der Wohnräume in Wohnungen - Linz
# https://www.data.gv.at/katalog/dataset/d0d994e4-b6e9-4f31-b594-4bea907e4c6a#resources
res = GET("https://www.data.gv.at/katalog/api/3/action/package_show?id=d0d994e4-b6e9-4f31-b594-4bea907e4c6a")
metadata = fromJSON(rawToChar(res$content))
names(metadata)

# get list of urls for csv files
urllist <- metadata[["result"]][["resources"]][["url"]] 
urllist
# neue Gliederung in 16 Statistische Bezirke
urllist <- str_subset(urllist, "V2014")
# request csv; add new column with year
df <- do.call(rbind, lapply(urllist, function(x) 
  cbind(read.csv(x, sep = ";", fileEncoding = "latin1"), 
        year = str_sub(x, start = -8, end = -5))
  ))


# clean
df_edit <- df
df_edit <- df_edit %>%
  rename(Wohnungen.mit.1.Wohnraum = Wohnungen.mit.einem.Wohnraum) %>%
  dplyr::mutate(Wohnungungen.bekannt = rowSums(across(c(3:10)), na.rm = TRUE)) 


# plot
# rooms with unknown number of rooms are excluded
df_plot <- df_edit %>%
  select(-c(11, 13)) %>%
  pivot_longer(c(3:10), names_to = "rooms", values_to = "number") %>%
  group_by(year, rooms) %>%
  dplyr::summarise(year_total = sum(number)) %>%
  dplyr::arrange(desc(year))


df_plot <- df_plot %>%
  mutate(year = paste("01-01-", year)) %>%
  mutate(year = lubridate::dmy(year))


leg_values <- c(pal_uchicago(palette = "light")(8))
leg_values[2] <- "#767676FF"
leg_values[8] <- "#616530FF"
leg_labels <- c("1 Wohnraum", "2 Wohnräume", "3 Wohnräume","4 Wohnräume", "5 Wohnräume", "6 Wohnräume","7 Wohnräume", ">7 Wohnräume")


plot_subtitle<-"<span style = 'color: black'>Anzahl der Linzer Wohnungen pro Jahr (2000-2022) </span>"
plot_subtitle2<-"<span style = 'color: black'>Veränderung der Anzahl der Wohnungen der gleichen Kategorie gegenüber dem Vorjahr </span>"
plot_caption<-"<p style='color:black'>Datenquelle: Stadt Linz; Veröffentlicht durch: data.gv.at; Visualisierung: &#64;rkan&#64;fosstodon.org"

p1 <- ggplot(df_plot, aes(x = year, y = year_total, fill = rooms)) +
  geom_area() + 
  theme_classic() +
  scale_x_date(date_labels = "%y", 
               date_breaks ="1 year", expand = c(0, 0), 
               name = "Jahr") +
  scale_y_continuous(limits = c(0, 125000),
                     breaks = seq(0,125000, 25000),
                     expand = c(0, 0),
                     labels = label_number(suffix = " K", scale = 1e-3),
                     name = "Anzahl Wohnungen") +
  scale_fill_manual(values = leg_values, 
                    name = "Kategorie",
                    labels = leg_labels) +
  labs(subtitle = plot_subtitle) +
  theme(text = element_text(family = "Merriweather"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        plot.subtitle = element_textbox_simple(family = "Merriweather",size = 13, margin = margin(b = 10)), 
        legend.text = element_text(size = 10))

# percent change
df_change <- df_plot
df_change <- df_change %>%
  group_by(rooms) %>%
  dplyr::mutate(pct_change = (year_total-(lead(year_total, n = 1)))/(lead(year_total, n = 1)) * 100)

annotate <- data.frame(
  x = as.Date(c('2004-01-01', '2013-06-01')),
  y = c(18, 16.5),
  text = c("142 weitere Wohnungen mit mehr \nals 7 Wohnräumen (651 in 2010)", "3.830 weitere Wohnungen mit \n1 Wohnraum (20.439 in 2019)")
  )

p2 <- ggplot(df_change) +
  geom_path(aes(x = year, y = pct_change, group = rooms, col = rooms), size = 1) +
  theme_classic() +
  geom_text(data = annotate, aes(x, y, label = text), hjust = 0, size = 3, family = "Merriweather") +
  # arrow1
  geom_curve(mapping = aes(x = as.Date('2007-09-01'), 
                           xend = as.Date('2009-09-01'),
                           y = 22, yend = 27),
             linewidth = 0.1,
             colour = "grey23", curvature = -0.2,  
             arrow = arrow(length = unit(2, "mm"))) +
  # arrow2
  geom_curve(mapping = aes(x = as.Date('2016-10-01'), 
                           xend = as.Date('2018-09-01'),
                           y = 20, yend = 23),
             colour = "grey23", curvature = -0.2, 
             linewidth = 0.1,
             arrow = arrow(length = unit(2, "mm"))) +
  scale_x_date(name = "Jahr",
               date_labels="%y", 
               limits = as.Date(c('2000-01-01','2022-01-01')),
               date_breaks  ="1 year", expand = c(0, 0)) +
  scale_y_continuous(name="% Veränderung", limits = c(-15, 30),
                     breaks = seq(-15, 30, 15), expand = c(0, 0)) +
  scale_colour_manual(values = leg_values, 
                    name = "Kategorie",
                    labels = leg_labels) +
  labs(caption = plot_caption,
       subtitle = plot_subtitle2) +
  theme(text = element_text(family = "Merriweather"),
    axis.title.y = element_text(margin = unit(c(0, 4.5, 0, 0), "mm")),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
      plot.subtitle = element_textbox_simple(family = "Merriweather", size = 13,margin = margin(t = 8, b = 10)),
      plot.caption = element_textbox_simple(family = "Merriweather", size = 8, margin = margin(t = 5, b = 5)),
      legend.text = element_text(size = 10))

   
# combine plot
ggarrange(p1, p2, nrow = 2, heights = c(0.8, 1),
          common.legend = TRUE, legend = "right")


ggsave("plot.png", width = 10.2, height = 5.91, bg = "white")


# notes
# arrows from: https://github.com/tashapiro/TidyTuesday/blob/master/2021/W48/doctor_who.R
