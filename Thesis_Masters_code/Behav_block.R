# Packages
library(dplyr)
library(ggplot2)
library(tidyr)
  

#dat <- dir('D:\\RM Cognative Psychology\\Thesis\\Behav_data\\behav', pattern = 'csv', full.names = TRUE) %>% 
#  map(read_csv)
#dat <- dat %>% 
#  map(~.x %>% mutate(mask_back=as.character(mask_back))) %>% 
#  bind_rows()
file_path <- "D:/RM Cognative Psychology/Thesis/Behav_data/behav/"
csv_files <- list.files(path = file_path, pattern = "\\.csv$", full.names = TRUE)

behav_data_1 <- read.csv(csv_files[36]) # Get the data



participant <- behav_data_1$participant[2]
target_duration <- trunc(behav_data_1$target_duration[354]*1000) /1000
session <- behav_data_1$session[1]


final_data <- behav_data_1 %>%
  mutate(accuracy_status = case_when(
    endsWith(accuracy, "True") ~ 1,
    endsWith(accuracy, "False") ~ 0
  ))
final_behav = final_data %>% relocate(accuracy_status, .after = accuracy) # **Changing the position of the columns**
final_behav = final_data %>% relocate(block_count, .after = block_type)
#View(final_behav)

## **Filtering Data**


filtered_behave_experiment<- filter(final_data, block_type == "experiment") # **filtering for the experiment trials**

filtered_behave_detection <- filter(filtered_behave_experiment, trial_type == "decision") # **Filtering for the detection trials**

# **Creating Conditions**

condition <- function(type_c){
  block_number<- list(0,1,2,3,4,5,6,7)
  if (type_c == "fullymasked"){
    filtered_behave <- filter(filtered_behave_detection, masked == "True" , staircase == "False") # **Filtering for the masked trials**
    
  }
  if(type_c == "unmasked"){
    filtered_behave <- filter(filtered_behave_detection, masked == "False", staircase == "False") # **Filtering for the unmasked trials**
    
  }
  if (type_c == "staircased"){
    filtered_behave <- filter(filtered_behave_detection, masked == "True", staircase == "True") # **Filtering for the staircased trials**
  }
  
  # Create a list to store blocks
  blocks <- list()
  
  for (values in block_number){
    if (any(values == filtered_behave$block_count)){
      blocks[[values + 1]] <- filter(filtered_behave, block_count == values)
    }
  }
  
  names(blocks) <- paste("block", sprintf("%01d", 1:8), sep="_")
  
  # Assign the blocks list to a variable named after the `type_c` argument
  assign(type_c, blocks, envir = .GlobalEnv)  
  
}

# **Masked Condition**
condition("fullymasked")

# **Unmasked Condition**
condition("unmasked")

# **Staircased Condition**
condition("staircased")


# ** Correct Responses **

response_analyses_block <- function(block_no){
  correct_response_block <- 0
  for (values in block_no$accuracy_status){
    if (values == 1){
      correct_response_block <- correct_response_block + 1
    }
  }
  
  
  response = (correct_response_block/48)
  return(round(response, digits = 2))
  
}

# ** Mean Accuracy**

mean_accuracy <- function(con_type){
  block_number<- c(0,1,2,3,4,5,6,7)
  block_accuracy <- list()
  for (i in 1:length(con_type)){
    for (value in block_number){
      if (any(value == con_type[[i]][["block_count"]])){
        block_accuracy[value + 1] <- response_analyses_block(con_type[[i]])
        #print(block_accuracy)
      }
      
    }
  }
  block_accuracy <- unlist(block_accuracy)
  block_accuracy <- as.data.frame(block_accuracy)
  #print(block_accuracy)  # Debugging line to check the contents
  argName <- deparse(substitute(con_type))
  colnames(block_accuracy)[1] <- argName
  
  if (argName  == "fullymasked"){
    assign("masked_accuracy", block_accuracy, envir = .GlobalEnv)
  }
  if (argName == "unmasked"){
    assign("unmasked_accuracy", block_accuracy, envir = .GlobalEnv)
  }
  if (argName == "staircased"){
    assign("staircased_accuracy", block_accuracy, envir = .GlobalEnv)
    
  }
}


# ** Mean Accuracy of Masked Trials **
mean_accuracy(fullymasked)

# ** Mean Accuracy of Unmasked Trials **
mean_accuracy(unmasked)

# ** Mean Accuracy of Staircased Trials **
mean_accuracy(staircased)
block_number<- c(1,2,3,4,5,6,7,8)

session_block_accuracy <- cbind(masked_accuracy,staircased_accuracy,unmasked_accuracy)
session_block_accuracy$block_number <- c(1,2,3,4,5,6,7,8)
# ** Plotting the Conditions **


# Plotting Trials
block_number<- c(1,2,3,4,5,6,7,8)
file_name <- paste("P",participant,"_S",session,".jpeg", sep = "" )
title <- paste( "Performance: Participant:", participant, "Session", session, "\n Target Duration:", target_duration,"ms")

plot <- ggplot(data = session_block_accuracy, aes(x = block_number, group = 1)) + 
  geom_line(aes(y = fullymasked, color = "Fully Masked"), size = 0.70) + 
  geom_line(aes(y = staircased, color = "Staircased"), size = 0.70) + 
  geom_line(aes(y = unmasked, color = "Unmasked"), size = 0.70) +
  ylim(0, 1) + labs(x = "Block Number", y = "Accuracy") +
  scale_color_manual(values = c("Fully Masked" = "darkred", "Staircased" = "steelblue", "Unmasked" = "darkgreen"),
                     name = "Condition") 
plot <- plot + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +theme(
  legend.position =c(.8,.20))

plot

ggsave(filename = file_name, plot = plot, path = "D:\\RM Cognative Psychology\\Thesis\\analysis\\Behav_data", scale = 1, height = 5, width = 4 )





# Alternate Plot
#file_name <- paste( "D:\\RM Cognative Psychology\\Thesis\\analysis\\Behav_data\\P", participant,"_S",session,".jpeg", sep = "" )
#matplot(y = cbind(staicased_accuracy, unmasked_accuracy, masked_accuracy),  x = block_number, main = paste( "Performance: Participant:", participant, "Session", session, "\n Target Duration:", target_duration,"ms"), 
#        ylim = c(0.0,1),  type = c("b"), pch = c(0,4,6), lwd = 3, lty = 1, xlab = "Block Number", ylab = "Accuracy", col = c("red", "black", "green"))

#legend("bottomright", legend = c("Staircase trials", "Unmasked Trials", "Masked Trials"),
#       col = c("red", "black", "green"),lty = 1, lwd = 3,pch = c(0,4,6),cex = 1)
  
  
  