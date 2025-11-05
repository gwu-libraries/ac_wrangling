library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(glue)


# Start date ----

START_DATE = '10/27/2025'

# Read in courses served ----

tutoring_courses <- read.delim('tutoring.txt', header = FALSE,
                               col.names = 'course')
tutoring_courses <- tutoring_courses %>%
  mutate(course = substr(course, 1, 9),
         has_tutoring = TRUE)

review_session_courses <- read.delim('review_sessions.txt', header = FALSE,
                               col.names = 'course')
review_session_courses <- review_session_courses %>%
  mutate(course = substr(course, 1, 9),
         has_review_session = TRUE)

course_info = full_join(tutoring_courses, review_session_courses) %>%
  mutate(across(c(has_tutoring, has_review_session), ~replace_na(.x, FALSE)))

ff_raw <- read_excel('data/Feedback Details 11.3.2025.xlsx',
                     sheet = 'Sheet 1 - Feedback Details (2)')

# Create list of students noted for specific courses
ff <- ff_raw %>%
  filter(!(`Feedback Description` %in%
             c('Student is doing well',
               'Exceptional performance'))) %>%
  filter(as.Date(`Comment Date`, format = "%m/%d/%Y") >= as.Date(START_DATE, format = '%m/%d/%Y')) %>% # only keep comments within the past 7 days 
  mutate(student_id = tolower(sub("@.*", "", `Student Email`)),
         course = paste(`Subject Code`, `Course Number`)) %>%
  # This is just for looking at duplicates
  select(student_id, course) %>%
  distinct() # drops many duplicates

penji_raw <- read.csv('data/visits.csv', check.names = FALSE)

# Create data for students who DID use review sessions or tutoring
penji <- penji_raw %>%
  filter(`Community Name` %in% c('LA Review Sessions & Office Hours', 'Academic Commons Peer Tutoring')) %>%
  mutate(student_id = tolower(sub("@.*", "", `Student Email`))) %>%
  select(student_id, service = `Community Name`, Status, `Student Attendance`, Topic) %>%
  filter(!(Status %in% c('Cancelled', 'No Feedback'))) %>% # eliminate 
  filter(`Student Attendance` != 'Absent') %>%
  mutate(course = paste(substr(gsub(" ", "", Topic), 1, 4),
                        substr(gsub(" ", "", Topic), 5, 8)))  %>%
  mutate(service = case_when(service == 'LA Review Sessions & Office Hours' ~ 'review_session',
                             service == 'Academic Commons Peer Tutoring' ~ 'tutoring')) %>%
  select(student_id, course, service) %>%
  distinct()

penji_wide <- penji %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from = service,
              values_from = value,
              values_fill = NA)

merged_df <- left_join(ff, penji_wide)

merged_df <- merged_df %>%
  mutate(across(c(review_session, tutoring),
         ~replace_na(.x, FALSE))) %>%
  #mutate(any_service = review_session | tutoring) %>%
  filter(!review_session |
           !tutoring) %>% # keep only students who have NOT used at least one service
  filter(course %in% course_info$course) # we only care about courses that we offer any service for

# tutoring_message = 'Academic Commons has free one-on-one tutoring for a number of courses in biology, chemistry, economics, math, physics, statistics, and more. Many students report gaining confidence and mastering difficult material from using our services. Get started here: go.gwu.edu/tutoring'
# both_message = 'Academic Commons has free academic support for a number of courses in biology, chemistry, economics, math, physics, statistics, and more. Many students report gaining confidence and mastering difficult material from using our services. You can find tutoring at go.gwu.edu/tutoring and review sessions at go.gwu.edu/reviewsessions.'
# review_sessions_message = 'Review sessions message'

tutoring_message = 'Academic Commons has free one-on-one tutoring for {course}. Many students report gaining confidence and mastering difficult material from using our services. Get started here: go.gwu.edu/tutoring'
both_message = 'Academic Commons has free academic support for {course}. Many students report gaining confidence and mastering difficult material from using our services. You can find tutoring at go.gwu.edu/tutoring and review sessions at go.gwu.edu/reviewsessions'
review_sessions_message = 'Academic Commons has free review sessions for {course}. Many students report gaining confidence and mastering difficult material from using our services. Get started here: go.gwu.edu/reviewsessions'

notification_messages_df <- data.frame(
  notification_type = c('both', 'review_session', 'tutoring'),
  notification_content = c(both_message, review_sessions_message, tutoring_message)
)

merged_df <- left_join(merged_df, course_info) 


merged_df <- merged_df %>%
  mutate(needs_review_session_notification = !review_session & has_review_session,
         needs_tutoring_notification = !tutoring & has_tutoring) %>%
  mutate(needs_both_notification = needs_review_session_notification & needs_tutoring_notification)
  
merged_df <- merged_df %>%  
  mutate(notification_type = case_when(needs_both_notification ~ 'both',  # didn't use either service
                                       # so they must have used at least one, but...
                                       #   didn't use review sessions/office hours
                                       needs_review_session_notification ~ 'review_session',
                                       #   didn't use peer tutoring
                                       needs_tutoring_notification ~ 'tutoring',
                                       TRUE ~ NA)) %>% # this can happen if what the student needs we don't provide
  drop_na() %>%
  select(student_id, course, notification_type) 

# Only include notifications for supported courses


  

table(merged_df$notification_type)

notifications_df <- left_join(merged_df, notification_messages_df)

notifications_df <- notifications_df %>%
  mutate(student_email = paste0(student_id, '@gwu.edu')) %>%
  rowwise() %>%
  mutate(full_message = glue(notification_content)) %>%
  ungroup() %>%
  select(student_email, full_message)

write.csv(notifications_df, file = 'notifications_2025-11-05.csv', row.names = FALSE)
write.xlsx(notifications_df,  file = 'notifications_2025-11-05.xlsx')
