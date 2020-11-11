
# tidy tuesday - historical phone usage - 2020/11/10 ---------------------


# load packages -----------------------------------------------------------

library(tidyverse)
library(skimr)
library(viridis)
library(plotly)
library(hrbrthemes)
library(patchwork)

# load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2020, week = 46)

mobile <- tuesdata$mobile
landline <- tuesdata$landline


# review data -------------------------------------------------------------

skim(mobile)
skim(landline)

summary(mobile)
summary(landline)

# join data ---------------------------------------------------------------

ll_mob <- landline %>% 
  left_join(mobile, 
            by = c("entity", "code", "year", "continent"),
            suffix = c("_ll", "_mob")) %>% 
  relocate("continent")

# visualize ---------------------------------------------------------------

# change in landline subscriptions across year
landline_plot <- ll_mob %>% 
  group_by(continent, year) %>%
  mutate(landline_subs = round(landline_subs, 2)) %>% 
  summarize(num_subs = mean(landline_subs, na.rm = TRUE)) %>% 
  ggplot(aes(year, num_subs, 
             fill = continent, 
             text = continent)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  ylim(c(0, 250)) +
  ggtitle("Landline subscriptions across time") +
  labs(x = "Year", 
       y = "Average number of landline subscriptions (per 100 people)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "none") 

# change in mobile subscriptions across year
mobile_plot <- ll_mob %>% 
  group_by(continent, year) %>%
  mutate(mobile_subs = round(mobile_subs, 2)) %>% 
  summarize(num_subs = mean(mobile_subs, na.rm = TRUE)) %>%
  ggplot(aes(year, num_subs, 
             fill = continent, 
             text = continent)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Mobile subscriptions across time") +
  labs(x = "Year", y = "Average number of mobile subscriptions (per 100 people)",
       fill = "Continent") +
  theme(panel.grid.minor = element_blank()) +
  theme_bw()

# combine plots
landline_plot + mobile_plot 

# save to object
patch_plot <- landline_plot + mobile_plot 

# sasve plot
ggsave("patch_plot_phone_time.png", patch_plot)

# create interactive plots individually
ll_int_plot <- ggplotly(landline_plot, tooltip = "text")
mob_int_plot <- ggplotly(mobile_plot, tooltip = "text")