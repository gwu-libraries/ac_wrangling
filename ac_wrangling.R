library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(glue)

ff_raw <- read_excel('data/Faculty Feedback_10-27 (Copy for Dan).xlsx', sheet = 'Original')

# Create list of students noted for specific courses
ff <- ff_raw %>%
  filter(!(`Feedback Description` %in%
             c('Student is doing well',
               'Exceptional performance'))) %>%
  mutate(student_id = tolower(sub("@.*", "", `Student Email`)),
         subject_course = paste(Subject, `Course Number`)) %>%
  # This is just for looking at duplicates
  select(student_id, course = subject_course) %>%
  distinct() # drops many duplicates

penji_raw <- read_excel('data/Penji Data 8-24 to 10-26 (RawOutput).xlsx')

# Create data for students who DID use review sessions or tutoring
penji <- penji_raw %>%
  filter(`Community Name` %in% c('LA Review Sessions & Office Hours', 'Academic Commons Peer Tutoring')) %>%
  mutate(student_id = tolower(sub("@.*", "", `Student Email`))) %>%
  select(student_id, service = `Community Name`, Status, `Student Attendance`, Topic) %>%
  filter(!(Status %in% c('Cancelled', 'No Feedback'))) %>% # eliminate 
  filter(`Student Attendance` != 'Absent') %>%
  mutate(course = paste(substr(gsub(" ", "", Topic), 1, 4),
                        substr(gsub(" ", "", Topic), 5, 8)))  %>%
  select(student_id, course, service) %>%
  distinct()

penji_wide <- penji %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from = service,
              values_from = value,
              values_fill = NA)

merged_df <- left_join(ff, penji_wide)

merged_df <- merged_df %>%
  mutate(across(c(`LA Review Sessions & Office Hours`, `Academic Commons Peer Tutoring`),
         ~replace_na(.x, FALSE))) %>%
  mutate(any_service = `LA Review Sessions & Office Hours` | `Academic Commons Peer Tutoring`) %>%
  filter(!`LA Review Sessions & Office Hours` |
           !`Academic Commons Peer Tutoring`) # keep only students who have NOT used at least one service

tutoring_message = 'Academic Commons has free one-on-one tutoring for a number of courses in biology, chemistry, economics, math, physics, statistics, and more. Many students report gaining confidence and mastering difficult material from using our services. Get started here: go.gwu.edu/tutoring'
both_message = 'Academic Commons has free academic support for a number of courses in biology, chemistry, economics, math, physics, statistics, and more. Many students report gaining confidence and mastering difficult material from using our services. You can find tutoring at go.gwu.edu/tutoring and review sessions at go.gwu.edu/reviewsessions.'
review_sessions_message = 'Review sessions message'

tutoring_message = 'Academic Commons has free one-on-one tutoring for {course}. Many students report gaining confidence and mastering difficult material from using our services. Get started here: go.gwu.edu/tutoring'
both_message = 'Academic Commons has free academic support for {course}. Many students report gaining confidence and mastering difficult material from using our services. You can find tutoring at go.gwu.edu/tutoring and review sessions at go.gwu.edu/reviewsessions'
review_sessions_message = 'Academic Commons has free review sessions for {course}. Many students report gaining confidence and mastering difficult material from using our services. Get started here: go.gwu.edu/reviewsessions'

notification_messages_df <- data.frame(
  notification_type = c('both', 'Review sessions & Office Hours', 'Peer tutoring'),
  notification_content = c(both_message, review_sessions_message, tutoring_message)
)

merged_df <- merged_df %>%
  mutate(notification_type = case_when(!any_service ~ 'both',  # didn't use either service
                                       # so they must have used at least one, but...
                                       #   didn't use review sessions/office hours
                                       !`LA Review Sessions & Office Hours` ~ 'Review sessions & Office Hours',
                                       #   didn't use peer tutoring
                                       !`Academic Commons Peer Tutoring` ~ 'Peer tutoring')) %>%
  select(student_id, course, notification_type)

table(merged_df$notification_type)

notifications_df <- left_join(merged_df, notification_messages_df)

notifications_df <- notifications_df %>%
  mutate(student_email = paste0(student_id, '@gwu.edu')) %>%
  rowwise() %>%
  mutate(full_message = glue(notification_content)) %>%
  ungroup() %>%
  select(student_email, full_message)

write.csv(notifications_df, file = 'notifications_2025-11-04.csv', row.names = FALSE)
write.xlsx(notifications_df,  file = 'notifications_2025-11-04.xlsx')
