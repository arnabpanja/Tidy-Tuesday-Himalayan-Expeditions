library(tidyverse)
library(ggthemes)
library(patchwork)
library(janitor)
library(grid)




tbl_expeditions <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv') %>%
  janitor::clean_names() %>% janitor::remove_empty(which = "rows")
tbl_members <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv') %>%
  janitor::clean_names() %>% janitor::remove_empty(which = "rows")
tbl_peaks <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv') %>% 
  janitor::clean_names() %>% janitor::remove_empty(which = "rows")

names(tbl_expeditions)
names(tbl_members)
names(tbl_peaks)


# colors -------------------------

clrs <- list(navy = "#001F3F", 
             blue = "#0074D9",
             aqua = "#7FDBFF",
             teal = "#39CCCC", 
             olive = "#3D9970", 
             green = "#2ECC40", 
             lime = "#01FF70", 
             yellow = "#FFDC00", 
             orange = "#FF851B", 
             red = "#FF4136", 
             fuchisia = "#F012BE", 
             purple = "#B10DC9", 
             maroon = "#85144B", 
             white = "#FFFFFF", 
             silver = "#DDDDDD", 
             gray = "#AAAAAA", 
             black = "#111111"
             )




# Members who have successfully climbed the peaks ------------

tbl_members_success <- filter(tbl_members, 
                              success == TRUE, 
                              !is.na(highpoint_metres), 
                              !is.na(sex), 
                              !is.na(age)) %>% 
  select(expedition_id, member_id, peak_id, peak_name, age, sex, 
         citizenship, highpoint_metres, solo)

# Top 10 Himalayan peaks by height in metres ------------

tbl_peaks_top_10 <- filter(tbl_peaks, climbing_status == "Climbed") %>% 
  arrange(desc(height_metres)) %>% 
  mutate(rown = row_number()) %>% 
  filter(rown <= 10) %>% 
  select(peak_id, peak_name, peak_alternative_name, 
         height_metres, rank_no = rown)


#Members who have successfully scaled the Top 10 peaks  ------------

tbl_himalayan_data_1 <- inner_join(tbl_members_success, 
                                   tbl_peaks_top_10, 
                                   by = "peak_id") %>% 
                          transmute( 
                                  expedition_id = expedition_id, 
                                  member_id = member_id, 
                                  peak_id = peak_id, 
                                  peak_name = peak_name.x, 
                                  peak_alternative_name = peak_alternative_name, 
                                  age = age, 
                                  sex = ifelse(sex == "M", "Male", "Female"), 
                                  citizenship = citizenship, 
                                  height_metres = height_metres,
                                  highpoint_metres = highpoint_metres, 
                                  rank_no = rank_no
                                  )

# Highpoint Metres Vs Gender Plot  ------------

p_highpoint_metres <- tbl_members_success %>% 
  mutate(new_sex = ifelse(sex == "M", "Male", "Female")) %>% 
  ggplot(mapping = aes(x = new_sex, y = highpoint_metres)) + 
  geom_violin(mapping = aes(fill = new_sex), 
               show.legend = FALSE) +
  scale_fill_manual(values = c(clrs$fuchisia, clrs$aqua)) +
  scale_color_manual(values = c(clrs$fuchisia, clrs$aqua)) +
  theme_solarized_2(light = FALSE, base_size = 8) + 
  theme(axis.text.x = element_text(color = "white"), 
        axis.text.y = element_text(color = "white"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white"), 
        plot.title = element_text(color = "white"),
        strip.text = element_text(color = "white")) + 
  labs(x = "", 
       y = "Highpoint Meters")



# Age vs Gender Variation Plot  ------------

p_age <- tbl_members_success %>% 
  mutate(new_sex = ifelse(sex == "M", "Male", "Female")) %>% 
  ggplot(mapping = aes(x = new_sex, y = age)) + 
  geom_violin(mapping = aes(fill = new_sex), 
               show.legend = FALSE) + 
  scale_fill_manual(values = c(clrs$fuchisia, clrs$aqua)) +
  theme_solarized_2(light = FALSE, base_size = 8) + 
  theme(axis.text.x = element_text(color = "white"), 
        axis.text.y = element_text(color = "white"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white"), 
        plot.title = element_text(color = "white"),
        strip.text = element_text(color = "white")) +  
  labs(x = "", 
       y = "Age")

# Combining the above two plots with patchwork  

p_summary_plots1 <- (p_age / p_highpoint_metres) + plot_layout(guides = 'collect') + 
  plot_annotation(title = "Himalayan Expeditions", 
                  subtitle = "Highpoint & Age Vs Gender Variations", 
                  theme = theme_solarized_2(light = FALSE, base_size = 8) + theme(plot.title = element_text(color = "white"), 
                                                                                   plot.subtitle = element_text(color = "white")))

p_summary_plots1

ggsave(filename = "D:\\R for Data Science\\R plots\\03 May 2020\\Tidy Tuesday Himalayan Expeditions\\p_summary_plots1.png", plot = p_summary_plots1)


# Climbers Count vs Highest Peaks Plot -----------------------


my_grob_text <- grobTree(textGrob("Mount Everest is the most popular\nof all the Himalayan Peaks",
                             x = 0.45, y = 0.9, hjust = 0,
                             gp = gpar(col = "darkgoldenrod1",
                                       fontsize = 8,
                                       fontface = "italic")))
my_grob_arrows <- grobTree(linesGrob(x = c(0.71, 0.88),
                                    y = c(0.85, 0.85), arrow = arrow(ends = "last", type = "closed", angle = 15),
                                    gp = gpar(col = "darkgoldenrod1", lty = "dashed", lwd = 0.5, fill = "darkgoldenrod1")))




p_climbers_count <- tbl_himalayan_data_1 %>% group_by(peak_name) %>% 
  summarise(count = n_distinct(member_id)) %>% 
  ungroup() %>% 
  ggplot() + geom_bar(mapping = aes(x = reorder(peak_name, count), 
                                    y = count, 
                                    fill = peak_name), 
                      stat = "identity", show.legend = FALSE) +
  geom_text(mapping = aes(x = reorder(peak_name, count), 
                          y = count, 
                          label = count, 
                          fontface = "bold"), 
            show.legend = FALSE, 
            color = "White", 
            size = 3, 
            nudge_y = 500) + 
  theme_solarized_2(light = FALSE, base_size = 10) + 
  theme(axis.text.x = element_text(color = "white", angle = 45, hjust = 1), 
        axis.text.y = element_text(color = "white"),
        axis.title.x = element_text(color = "gold1"),
        axis.title.y = element_text(color = "gold1"), 
        plot.title = element_text(color = "gold1")) + 
  labs(x = "Himalayan Peak Name", 
       y = "No of Successful Climbers", 
       title = "Cimbers Count Vs Highest Scaled Peaks") + 
  annotation_custom(grob = my_grob_text) + 
  annotation_custom(grob = my_grob_arrows)
  

p_climbers_count

ggsave(filename = "D:\\R for Data Science\\R plots\\03 May 2020\\Tidy Tuesday Himalayan Expeditions\\p_climbers_count.png", plot = p_climbers_count)



# Age Distribution of Successful Climbers Plot -------

my_grob_text1 <- grobTree(textGrob("The peak age of successful climbers is \n30 years with around 2000 climbers with this age",
                                   x = 0.50, y = 0.9, hjust = 0,
                                   gp = gpar(col = "darkgoldenrod1",
                                             fontsize = 8,
                                             fontface = "italic")))

p_age_counts <- ggplot(data = tbl_himalayan_data_1) + 
  geom_histogram(mapping = aes(x = age), binwidth = 2.5, fill = "cornsilk1") + 
  geom_freqpoly(mapping = aes(x = age), binwidth = 3, size = 1.2, color = "orange") + 
  geom_vline(xintercept = 30, color = "darkred", 
             linetype = "dashed", 
             size = 1) + 
  scale_x_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10)) +
  scale_y_continuous(expand = c(0,0)) + 
  theme_solarized(light = FALSE, base_size = 10) + 
  theme(axis.text.x = element_text(color = "white"), 
        axis.text.y = element_text(color = "white"),
        axis.title.x = element_text(color = "gold1"),
        axis.title.y = element_text(color = "gold1"), 
        plot.title = element_text(color = "gold1")) +
  labs(x = "Age (In Years)", 
       y = "No of Climbers", 
       title = "Age Distribution of Successful Climbers") + 
  annotation_custom(grob = my_grob_text1)

p_age_counts

ggsave(filename = "D:\\R for Data Science\\R plots\\03 May 2020\\Tidy Tuesday Himalayan Expeditions\\p_age_counts.png", plot = p_age_counts)


# Age Distribution of Climbers with Gender Plot --------------

p_age_freqpoly <- ggplot(data = tbl_himalayan_data_1) + 
  geom_freqpoly(mapping = aes(x = age, group = sex, color = sex), binwidth = 3, size = 1.2) + 
  theme_solarized(light = FALSE) + 
  theme(axis.text.x = element_text(color = "white"), axis.text.y = element_text(color = "white")) +
  xlab("Age") + ylab("No of Successful Climbers") + ggtitle("Age Distribution of Climbers with Gender")

p_age_freqpoly


# Climbers vs Countries Plot ---------------------

p_climbers_countries <- tbl_members_success %>% group_by(citizenship) %>% 
  summarise(cnt = n_distinct(member_id)) %>% 
  mutate(rown = row_number(desc(cnt))) %>% 
  filter(rown <= 10) %>% arrange(rown) %>%
  ggplot(mapping = aes(x = reorder(citizenship, cnt), y = cnt)) + 
  geom_bar(mapping = aes(fill = citizenship), stat = "identity", show.legend = FALSE) + theme_solarized(light = FALSE) + 
  theme(axis.text.x = element_text(color = "white", angle = 45, hjust = 1), axis.text.y = element_text(color = "white")) +
  xlab("Country") + ylab("Climbers") + ggtitle("Climbers vs Countries")


p_climbers_countries


# Non Solo expeditions.. proportion of ---------------
# successful members per expedition ------------------
# top 10 expeditions with success rates---------------


filter(tbl_members, !is.na(highpoint_metres), !is.na(sex), !is.na(age), solo == FALSE) %>% 
  group_by(expedition_id) %>% 
  summarise(prop_success = mean(success == TRUE)) %>% 
  filter(prop_success > 0 & prop_success < 1) %>% 
  mutate(rown = row_number(desc(prop_success))) %>% 
  filter(rown <= 20)

# peaks by number of sucessful solo expeditions top 20 ----- 

p_solo_expeditions <- filter(tbl_members, !is.na(highpoint_metres), !is.na(sex), !is.na(age), solo == TRUE) %>% 
  group_by(peak_id, peak_name) %>% 
  summarise(cnt = n_distinct(expedition_id)) %>% 
  ungroup() %>% 
  mutate(rown = row_number(desc(cnt))) %>% 
  filter(rown <= 20) %>% 
  ggcharts::bar_chart(., x = peak_name, y = cnt) + theme_solarized_2(light = FALSE) + 
  theme(axis.text.x = element_text(color = "white"), axis.text.y = element_text(color = "white")) +
  xlab("Peak Name") + ylab("No of Solo Expeditions") + ggtitle("Peaks by Solo Expeditions - Top 20")

p_solo_expeditions

# Top Reasons of Death ----------------

p_reasons_death <- filter(tbl_members, died == TRUE) %>% 
  group_by(death_cause) %>% 
  summarise(cnt = n()) %>% 
  ggplot(mapping = aes(x = reorder(death_cause,cnt), y = cnt)) + 
  geom_bar(mapping = aes(fill = death_cause), stat = "identity", show.legend = FALSE) + 
  coord_flip() + theme_solarized_2(light = FALSE) +
  theme(axis.text.x = element_text(color = "white"), axis.text.y = element_text(color = "white")) +
  xlab("Cause of Death") + ylab("No of Deaths") + ggtitle("Top Reasons of Death")

p_reasons_death  

# No of deaths by age 

p_deaths_by_age <- filter(tbl_members, died == TRUE, !is.na(age)) %>% 
  ggplot() + geom_histogram(mapping = aes(x = age), binwidth = 2.5, fill = "yellow") + 
  geom_freqpoly(mapping = aes(x = age), binwidth = 3.4, size = 0.95, colour = "orange") +
  theme_solarized_2(light = FALSE) +
  theme(axis.text.x = element_text(color = "white"), axis.text.y = element_text(color = "white")) +
  xlab("Age of Climbers") + ylab("No of Deaths") + ggtitle("Deaths by Age of Climbers")

p_deaths_by_age

# Unclimbed peaks - Top 10 by Heights


p_unclimbed_peaks <- filter(tbl_peaks, climbing_status == "Unclimbed") %>% 
  mutate(rown = row_number(desc(height_metres))) %>% 
  filter(rown <= 20) %>% 
  transmute(peak_name = peak_name, height_metres = height_metres, rown = rown) %>% 
  ggplot() + geom_bar(mapping = aes(x = reorder(peak_name, height_metres), y = height_metres, fill = peak_name), stat = "identity", show.legend = FALSE) + 
  coord_flip() + 
  theme_solarized_2(light = FALSE) +
  theme(axis.text.x = element_text(color = "white"), axis.text.y = element_text(color = "white")) +
  xlab("Peak Name") + ylab("Peak Height in Metres") + ggtitle("Top 10 Unclimbed Peaks by Heights")

p_unclimbed_peaks




