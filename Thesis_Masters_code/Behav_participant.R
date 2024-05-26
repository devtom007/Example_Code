# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr)  
library(ggplot2)
library( BayesFactor )
# Data --------------------------------------------------------------------

participant_data <- dir('G:\\Other computers\\Main_frame\\Drive\\Behav_data\\behav', pattern = 'csv', full.names = TRUE) %>% 
  map(read_csv)

par_data <- participant_data %>% 
  map(~.x %>% mutate(mask_back=as.character(mask_back))) %>% 
  map(~.x %>% mutate(forced_break=as.character(forced_break)))%>%
  map(~.x %>% mutate(participant=as.character(participant)))%>%
  bind_rows()
# experiment data ---------------------------------------------------------

par_data <-
  par_data %>% 
  filter(block_type=='experiment', trial_type=='decision') %>% 
  mutate(condition = case_when(masked & !staircase ~ 'fully_masked',
                               masked & staircase ~ 'stair_masked',
                               !masked & !staircase ~ 'unmasked')) %>% 
  mutate(answer = answer == key_present) %>% 
  select(participant, session, target_duration, condition, target, answer, accuracy) %>% 
  mutate(target_duration = round(target_duration * 1000, 2))

# Participant data --------------------------------------------------------------------------------------

participant_001 <-data.frame(par_data %>%   filter(participant =="001")) 
participant_003 <- data.frame(par_data %>%  filter(participant =="003"))   
participant_006 <- data.frame(par_data %>%  filter(participant =="006"))
participant_007 <- data.frame(par_data %>%  filter(participant =="007"))
participant_010 <- data.frame(par_data %>%  filter(participant =="010"))






# ** Correct Responses **

response_analyses <- function(df) {
  # Group by target_duration and condition, then calculate mean accuracy
  result <- df %>%
    group_by(target_duration, condition, session) %>%
    summarise(mean_accuracy = mean(as.numeric(accuracy), na.rm = TRUE), sd_session = sd(as.numeric(accuracy), na.rm=TRUE))  # Convert logical to numeric and remove NA values if any
  
  return(result)
}

participant_001 <- response_analyses(participant_001)
participant_003 <- response_analyses(participant_003)  
participant_006 <- response_analyses(participant_006)
participant_007 <- response_analyses(participant_007)
participant_010 <- response_analyses(participant_010)

# Plots per perticipant---------------------------------------


participant_plot <- function(df) {
  argName <- deparse(substitute(df))
  required_columns <- c("target_duration", "condition", "mean_accuracy", "session")
  if(!all(required_columns %in% names(df))) {
    stop("Dataframe does not have the required columns: target_duration, condition, mean_accuracy, session")
  }
  
  # Create the jitter plot
  p <- ggplot(df, aes(x = factor(target_duration), y = mean_accuracy, color = condition)) +
    geom_jitter(size = 2, width = .2) +
    stat_summary(fun = mean, geom = "point", size = 3, shape = 17)+
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3)+
    #geom_errorbar(data = df, aes(y = mean_accuracy, ymin = mean_accuracy - sd(mean_accuracy), ymax = mean_accuracy + sd(mean_accuracy)), width = 0.2, color = "black") + # Individual SD bars
    scale_y_continuous(limits = c(0.3, 1), breaks = seq(0.3, 1, by = 0.1)) +
    labs(x = "Target Duration", y = "Accuracy", title = argName)+ 
   scale_color_manual(values = c("fully_masked" = "blue", "stair_masked" = "green", "unmasked" = "red")) 
   
  return(p)
}
p_1 <- participant_plot(participant_001)
p_1
#ggsave(filename = "P1_final.jpg", plot = p_1, path = "D:\\RM Cognative Psychology\\Thesis\\analysis\\Behav_data", scale = 1, height = 5, width = 4 )

p_3 <- participant_plot(participant_003)
p_3
#ggsave(filename = "P3_final.jpg", plot = p_3, path = "D:\\RM Cognative Psychology\\Thesis\\analysis\\Behav_data", scale = 1, height = 5, width = 4 )

p_6 <- participant_plot(participant_006) 
p_6
#ggsave(filename = "P6_final.jpg", plot = p_6, path = "D:\\RM Cognative Psychology\\Thesis\\analysis\\Behav_data", scale = 1, height = 5, width = 4 )

p_7<- participant_plot(participant_007) 
p_7
#ggsave(filename = "P7_final.jpg", plot = p_7, path = "D:\\RM Cognative Psychology\\Thesis\\analysis\\Behav_data", scale = 1, height = 5, width = 4 )

p_10 <- participant_plot(participant_010) 
p_10
#ggsave(filename = "P10_final.jpg", plot = p_10, path = "D:\\RM Cognative Psychology\\Thesis\\analysis\\Behav_data", scale = 1, height = 5, width = 4 )



# participant_plot <- function(df) {
#   argName <- deparse(substitute(df))
#   required_columns <- c("target_duration", "condition", "mean_accuracy")
#   if(!all(required_columns %in% names(df))) {
#     stop("Dataframe does not have the required columns: target_duration, condition, mean_accuracy")
#   }
#   
#   # Create the bar plot
#   p <- ggplot(df, aes(x = factor(target_duration), y = mean_accuracy, fill = condition)) +
#     geom_bar(stat = "identity", position = "dodge") +
#     labs(x = "Target Duration", y = "Mean Accuracy", title = paste("Performance of", argName )) +
#     scale_fill_brewer(palette = "Pastel1") +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))   + geom_errorbar(aes(ymin=mean_accuracy-sd_session, ymax=mean_accuracy+sd_session), width=.2, position=position_dodge(.9))
#   
#   
#   return(p)
# }
