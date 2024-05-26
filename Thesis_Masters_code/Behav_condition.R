# Packages
library(dplyr)
library(ggplot2)
library(tidyr)

# Getting all the files

file_path <- "D:/RM Cognative Psychology/Thesis/Behav_data/behav/" 

csv_files <- list.files(path = file_path, pattern = "\\.csv$", full.names = TRUE)

behav_data_1 <- read.csv(csv_files[36]) # Get the specific csv file

# Set variables

participant <- behav_data_1$participant[1]
target_duration <- trunc(behav_data_1$target_duration[354]*1000) /1000
session <- behav_data_1$session[1]

final_data <- behav_data_1 %>%
  mutate(accuracy_status = case_when(
    endsWith(accuracy, "True") ~ 1,
    endsWith(accuracy, "False") ~ 0
  ))
final_behav = final_data %>% relocate(accuracy_status, .after = accuracy) # **Changing the position of the columns**
final_behav = final_data %>% relocate(block_count, .after = block_type)

## **Filtering Data**


filtered_behave_experiment<- filter(final_data, block_type == "experiment") # **filtering for the experiment trials**

filtered_behave_detection <- filter(filtered_behave_experiment, trial_type == "decision") # **Filtering for the detection trials**


# **Creating Conditions**

condition <- function(type_c){
  block_number<- list(0,1,2,3,4,5,6,7)
  if (type_c == "fullymasked"){
    masked_behave <<- filter(filtered_behave_detection, masked == "True" , staircase == "False") # **Filtering for the masked trials**
    
    
  }
  if(type_c == "unmasked"){
    unmasked_behave <<- filter(filtered_behave_detection, masked == "False", staircase == "False") # **Filtering for the unmasked trials**
    
  }
  if (type_c == "staircased"){
    staircased_behave <<- filter(filtered_behave_detection, masked == "True", staircase == "True") # **Filtering for the staircased trials**
    
  }
  
  
}

# **Masked Condition**
condition("fullymasked")

# **Unmasked Condition**
condition("unmasked")

# **Staircased Condition**
condition("staircased")

# Creating correct responses

response_analyses <- function(con_type){
  correct_response <- 0
  for (values in con_type$accuracy_status){
    if (values == 1){
      correct_response <- correct_response + 1
    }
  }
  
  
  response <- round(correct_response/384, digits = 2)
  #return(round(response, digits = 2))
  # Assign the response to a variable named after the `con_type` argument
  argName <- deparse(substitute(con_type))
  if (argName == "masked_behave"){
    masked_accuracy <<- response
  }
  if (argName == "unmasked_behave"){
    unmasked_accuracy <<- response
  }
  if (argName == "staircased_behave"){
    staircased_accuracy <<- response
  }
  
}

# Getting correct responses per condition

response_analyses(masked_behave)
response_analyses(unmasked_behave)
response_analyses(staircased_behave)

session_accuracy <- data_frame(masked_accuracy,staircased_accuracy, unmasked_accuracy)
session_accuracy <- pivot_longer(session_accuracy,cols = c(masked_accuracy,staircased_accuracy, unmasked_accuracy),
                                      names_to = "accuracy_type", values_to = "accuracy_value")



# ** Plotting the Conditions **

file_name <- paste( "D:\\RM Cognative Psychology\\Thesis\\analysis\\Behav_data\\P", participant,"_S",session,"_con.jpeg", sep = "" )

jpeg(filename = file_name, width = 650, height = 800 )

ggplot(data = session_accuracy, aes(x = accuracy_type, y = accuracy_value)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.7) +
  labs(x = "Accuracy Type", y = "Accuracy Value", ) + labs(title = paste( "Performance: Participant:", participant, "Session", session, "\n Target Duration:", target_duration,"ms") )
theme_minimal()

dev.off()

ggplot(data = session_accuracy, aes(x = accuracy_type, y = accuracy_value)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.7) +
  labs(x = "Condition", y = "Accuracy") + labs(title = paste( "Performance: Participant:", participant, "Session", session, "\n Target Duration:", target_duration,"ms") )
theme_minimal()    


 
       