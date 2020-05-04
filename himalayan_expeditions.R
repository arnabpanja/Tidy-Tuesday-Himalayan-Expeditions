library(tidyverse)
library(patchwork)
library(ggthemes)



tbl_expeditions <- read_csv("C:\\Users\\lenovo\\Documents\\r_data\\himalayan_expeditions\\expeditions.csv")
View(tbl_expeditions)
tbl_members <- read_csv("C:\\Users\\lenovo\\Documents\\r_data\\himalayan_expeditions\\members.csv")
View(tbl_members)
tbl_peaks <- read_csv("C:\\Users\\lenovo\\Documents\\r_data\\himalayan_expeditions\\peaks.csv")
View(tbl_peaks)

# Questions 
tbl_members_success <- filter(tbl_members, success == TRUE, !is.na(highpoint_metres), !is.na(sex), !is.na(age)) %>% 
  select(expedition_id, member_id, peak_id, peak_name, age, sex, citizenship, highpoint_metres, solo)

tbl_peaks_top_10 <- filter(tbl_peaks, climbing_status == "Climbed") %>% 
  arrange(desc(height_metres)) %>% 
  mutate(rown = row_number()) %>% 
  filter(rown <= 10) %>% 
  select(peak_id, peak_name, peak_alternative_name, height_metres, rank_no = rown)

tbl_himalayan_data_1 <- inner_join(tbl_members_success, tbl_peaks_top_10, by = "peak_id") %>% 
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

p_highpoint_metres <- tbl_members_success %>% mutate(new_sex = ifelse(sex == "M", "Male", "Female")) %>% ggplot(mapping = aes(x = new_sex, y = highpoint_metres)) + geom_boxplot(mapping = aes(fill = new_sex), varwidth = TRUE, show.legend = FALSE) + xlab("Gender") + ylab("Highpoint Metres") + ggtitle("Highpoint Metres Vs Gender")
p_age <- tbl_members_success %>% mutate(new_sex = ifelse(sex == "M", "Male", "Female")) %>% ggplot(mapping = aes(x = new_sex, y = age)) + geom_boxplot(mapping = aes(fill = new_sex), varwidth = TRUE, show.legend = FALSE) + xlab("Gender") + ylab("Age") + ggtitle("Age vs Gender Variation")
p_climbers_count <- tbl_himalayan_data_1 %>% group_by(peak_name) %>% 
  summarise(count = n_distinct(member_id)) %>% 
  ggplot() + geom_bar(mapping = aes(x = reorder(peak_name, count), y = count, fill = peak_name), stat = "identity", show.legend = FALSE)  + ggtitle("Climbers Count vs Highest Peaks") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Himalayan Peak Name") + ylab("No of Successful Climbers") 

p_age_counts <- ggplot(data = tbl_himalayan_data_1) + geom_histogram(mapping = aes(x = age), binwidth = 2.5) + geom_freqpoly(mapping = aes(x = age), binwidth = 3, size = 1.2, color = "orange") + xlab("Age") + ylab("No of Climbers") + ggtitle("Age Distribution of Successful Climbers")

p_age_freqpoly <- ggplot(data = tbl_himalayan_data_1) + geom_freqpoly(mapping = aes(x = age, group = sex, color = sex), binwidth = 3, size = 1.2) + xlab("Age") + ylab("No of Successful Climbers") + ggtitle("Age Distribution of Climbers with Gender")


p_age
p_highpoint_metres

(p_age + p_highpoint_metres) / p_age_freqpoly

p_age_counts
p_climbers_count


p_climbers_countries <- tbl_members_success %>% group_by(citizenship) %>% 
  summarise(cnt = n_distinct(member_id)) %>% 
  mutate(rown = row_number(desc(cnt))) %>% 
  filter(rown <= 10) %>% arrange(rown) %>%
  ggplot(mapping = aes(x = reorder(citizenship, cnt), y = cnt)) + 
  geom_bar(mapping = aes(fill = citizenship), stat = "identity", show.legend = FALSE) + theme_solarized(light = FALSE) + 
  theme(axis.text.x = element_text(color = "white", angle = 45, hjust = 1), axis.text.y = element_text(color = "white")) +
  xlab("Country") + ylab("Climbers") + ggtitle("Climbers vs Countries")


p_climbers_countries


# Non Solo expeditions.. proportion of 
# successful members per expedition 
# top 10 expeditions with success rates


filter(tbl_members, !is.na(highpoint_metres), !is.na(sex), !is.na(age), solo == FALSE) %>% 
  group_by(expedition_id) %>% 
  summarise(prop_success = mean(success == TRUE)) %>% 
  filter(prop_success > 0 & prop_success < 1) %>% 
  mutate(rown = row_number(desc(prop_success))) %>% 
  filter(rown <= 20)

# peaks by number of sucessful solo expeditions
# top 20 

filter(tbl_members, !is.na(highpoint_metres), !is.na(sex), !is.na(age), solo == TRUE) %>% 
  group_by(peak_id, peak_name) %>% 
  summarise(cnt = n_distinct(expedition_id)) %>% 
  ungroup() %>% 
  mutate(rown = row_number(desc(cnt))) %>% 
  filter(rown <= 20) %>% 
  ggcharts::bar_chart(., x = peak_name, y = cnt) + theme_solarized_2(light = FALSE) + 
  theme(axis.text.x = element_text(color = "white"), axis.text.y = element_text(color = "white")) +
  xlab("Peak Name") + ylab("No of Solo Expeditions") + ggtitle("Peaks by Solo Expeditions - Top 20")

filter(tbl_members, died == TRUE) %>% 
  group_by(death_cause) %>% 
  summarise(cnt = n()) %>% 
  ggplot(mapping = aes(x = reorder(death_cause,cnt), y = cnt)) + 
  geom_bar(mapping = aes(fill = death_cause), stat = "identity", show.legend = FALSE) + 
  coord_flip() + theme_solarized_2(light = FALSE) +
  theme(axis.text.x = element_text(color = "white"), axis.text.y = element_text(color = "white")) +
  xlab("Cause of Death") + ylab("No of Deaths") + ggtitle("Top Reasons of Death")
  




