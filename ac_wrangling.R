library(dplyr)
library(tidyr)
library(readxl)

ff_raw <- read_excel('data/Faculty Feedback_10-27 (Copy for Dan).xlsx', sheet = 'Original')
ff <- ff_raw %>%
  filter(!(`Feedback Description` %in%
             c('Student is doing well',
               'Meet with instructor',
               'Exceptional performance'))) %>%
  mutate(student_id = tolower(sub("@.*", "", `Student Email`)),
         subject_course = paste(Subject, `Course Number`)) %>%
  # This is just for looking at duplicates
  arrange(student_id, subject_course) %>%
  select(student_id) %>% # , subject_course) %>%
  distinct() # drops many duplicates

penji_raw <- read_excel('data/Penji Data 8-24 to 10-26 (RawOutput).xlsx')   
penji <- penji_raw %>%
  filter(`Community Name` %in% c('LA Review Sessions & Office Hours', 'Academic Commons Peer Tutoring')) %>%
  mutate(student_id = tolower(sub("@.*", "", `Student Email`))) %>%
  select(student_id, service = `Community Name`) %>%
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
                                       !`Academic Commons Peer Tutoring` ~ 'Peer tutoring'))

table(merged_df$notification_type)

notifications_df <- left_join(merged_df, notification_messages_df) %>%
  mutate(student_email = paste0(student_id, '@gwu.edu')) %>%
  select(student_email, notification_content)

