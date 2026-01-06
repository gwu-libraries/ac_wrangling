library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(glue)
library(forcats)
library(lubridate)
library(ggplot2)
library(ggpubr)

# Load weekly feedback files and and stack them into one

feedback_files <- list.files(path = 'data_fall2025/', pattern = "AC*")

feedback_df <- data.frame()
for (f in feedback_files) {
  feedback_df_i <- read_excel(paste0('data_fall2025/', f)) %>%
    select(email = contains('Email'),
           course = `Course Combined`,
           message = contains('Communication'),
           service = Program,
           message_date = `Date Contacted (Assumed)`)
  feedback_df <- rbind(feedback_df, feedback_df_i)
}
rm(feedback_df_i)

# Clean up course anomalies

feedback_df <- feedback_df %>%
  mutate(course = case_when(course == 'CHEM1110' ~ 'CHEM 1110',
                            course == 'CHEM1111' ~ 'CHEM 1111',
                            TRUE ~ course))

# Check to see if any students were notified multiple times for the same service

multi_notify_check_df <- 
  feedback_df %>%
  count(email, course, service) 

# Split "Tutoring AND Review Sessions" into 2 notifications
feedback_df <- feedback_df %>%
  separate_rows(service, sep = ' AND ') %>% # Separate Tutoring AND Review Sessions into 2 rows
  mutate(service = fct_recode(service,
                              'tutoring' = 'Tutoring',
                              'review_session' = 'Review Sessions'))
# Load penji cumulative data

penji_df <- read_excel('data_fall2025/Penji Output 9-22 to 11-21.xlsx')

penji_df <- penji_df %>%
  filter(`Community Name` %in% c('LA Review Sessions & Office Hours', 'Academic Commons Peer Tutoring')) %>%
  mutate(student_id = tolower(sub("@.*", "", `Student Email`))) %>%
  select(student_id,
         service = `Community Name`,
         Status,
         `Student Attendance`,
         Topic,
         date = `Started At Date`) %>%
  filter(!(Status %in% c('Cancelled', 'No Feedback'))) %>% # eliminate 
  filter(`Student Attendance` != 'Absent') %>%
  mutate(course = paste(substr(gsub(" ", "", Topic), 1, 4),
                        substr(gsub(" ", "", Topic), 5, 8)))  %>%
  mutate(service = case_when(service == 'LA Review Sessions & Office Hours' ~ 'review_session',
                             service == 'Academic Commons Peer Tutoring' ~ 'tutoring')) %>%
  select(email = student_id, course, service, visit_date = date) %>%
  mutate(email = paste0(email, '@gwu.edu')) %>%
  distinct()



table(multi_notify_check_df$n)

#   1   2   3 
# 102 579   1 

# Based on the *first* notification, join on matching Penji visits, if any.
# Compute whether Penji visit was before, after, or NA
# Check for "pre-notification" visits (we shouldn't have any, but it's possible)
# Keep NAs to indicate where visits never occurred
# With "post-notification" visits, compute time lag
# Note that this semester's data is not a good test, due to the multi-notifications -- it's possible
#   that visits were due to having multiple reminder notifications.  Is there a way we can analyze these?

match_df <- left_join(feedback_df, penji_df) 

match_df2 <- match_df %>%
  group_by(email, course, service) %>%
  arrange(email, course, service, message_date, visit_date) %>%
  mutate(message_number = dense_rank(message_date)) %>%
  ungroup()

# Analyze response to first messages, ignoring effect of subsequent messages

match_df3 <- match_df2 %>%
  filter(message_number == 1) %>%
  mutate(visited = (!is.na(visit_date))) %>%
  select(-message_number) %>%
  group_by(email, course, service) %>%
  arrange(email, course, service, visit_date) %>%
  mutate(visit_number = dense_rank(visit_date)) %>%
  ungroup() %>%
  mutate(visit_lag_days = time_length(visit_date - message_date, 'day'))

visit1_analysis <- match_df3 %>%
  count(course, service, visited) %>%
  group_by(course, service) %>%
  complete(visited = c(TRUE, FALSE), # ... <data-masking> Specification of columns to expand or complete. Columns can be atomic vectors or lists.
           fill = list(n = 0)) %>%
  ungroup()

visit1_analysis_wide <- visit1_analysis %>%
  pivot_wider(names_from = visited,
              values_from = n,
              names_prefix = 'visited_') %>%
  rename(visit = visited_TRUE, no_visit = visited_FALSE) %>%
  mutate(pct_visit = round(100*visit/(visit + no_visit), 1)) %>%
  arrange(service, course)

write.csv(visit1_analysis_wide, 'visits_analysis.csv', row.names = FALSE)

overall_analysis <- visit1_analysis %>%
  group_by(service, visited) %>%
  summarize(total_n = sum(n)) %>%
  group_by(service) %>%
  mutate(n_pct = round(100*total_n/sum(total_n), 1))

by_course_analysis <- visit1_analysis %>%
  pivot_wider(names_from = visited,
              values_from = n) %>%
  rename(visited = 'TRUE', no_visit = 'FALSE') %>%
  mutate(total_messages = visited + no_visit,
         percent_response = round(visited/total_messages, 2))

by_course_plot_tutoring <-
  by_course_analysis %>%
  filter(service == 'tutoring') %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = percent_response,
                   y = fct_reorder(course, percent_response),
                   yend = fct_reorder(course, percent_response))) +
  geom_point(aes(x = percent_response,
                 y = fct_reorder(course, percent_response),
                 size = total_messages),
             shape = 21,
             fill = 'royalblue') +
  geom_text(aes(x = percent_response + 0.015,
                y = fct_reorder(course, percent_response),
                label = paste0(as.character(visited), '/',
                               as.character(total_messages))),
            size = 3,
            hjust = 0) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(y = 'Course', title = 'Tutoring',
       size = 'Number of \nStudents Messaged',
       x = 'Percent attending')

by_course_plot_tutoring

by_course_plot_review_sessions <-
  by_course_analysis %>%
  filter(service == 'review_session') %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = percent_response,
                   y = fct_reorder(course, percent_response),
                   yend = fct_reorder(course, percent_response))) +
  geom_point(aes(x = percent_response,
                 y = fct_reorder(course, percent_response),
                 size = total_messages),
             shape = 21,
             fill = 'royalblue') +
  geom_text(aes(x = percent_response + 0.03,
                y = fct_reorder(course, percent_response),
                label = paste0(as.character(visited), '/',
                               as.character(total_messages))),
            size = 3,
            hjust = 0) +
  scale_x_continuous(
    limits = c(0, 1.0),
    breaks = seq(0, 1.01, 0.1),
    labels = scales::percent_format()) +
  labs(y = 'Course', title = 'Review Sessions',
       size = 'Number of\nStudents Messaged',
       x = 'Percent attending')

# Lollipop chart - response by course ----

by_course_response_plot <-
  ggarrange(by_course_plot_tutoring,
            by_course_plot_review_sessions,
            ncol = 1,
            heights = c(1, 0.6),
            common.legend = TRUE,
            legend = 'right')

by_course_response_plot  
ggsave('response_by_course.pdf', by_course_response_plot,
       width = 8, height = 7)

# Pie chart for overall statistics ----

overall_stats <- overall_analysis %>%
  mutate(visited = factor(visited,
                          levels = c(FALSE, TRUE),
                          labels = c('No Visit', 'Visit')),
         service = factor(service,
                          levels = c('tutoring', 'review_session'),
                          labels = c('Tutoring', 'Review Sessions'))) %>%
  ggplot() +
  aes(x="", y=n_pct, fill=visited) +
  geom_bar(stat="identity", width=1, color = 'black', linewidth = 0.25) +
  geom_text(aes(label = total_n),
            position = position_stack(vjust = 0.5), size = 2.5) +
  coord_polar("y", start=0) +
  theme_minimal() + 
  labs(fill = '') +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  facet_grid(~ service, scales = 'free')

overall_stats
ggsave('aggregate_stats.pdf', overall_stats, width = 4, height = 2.5)

               