# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr)  
library(ggplot2)
library(BayesFactor)
library(brms)
library(bayestestR)
library(see)
#install.packages('patchwork')
# Data --------------------------------------------------------------------

participant_data <- dir('C:\\Users\\TO3TechSupport\\OneDrive - Vrije Universiteit Amsterdam\\Desktop\\Drive\\Behav_data\\Data\\Behav_data', pattern = 'csv', full.names = TRUE) %>% 
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
  select(participant, session, target_duration, condition, target, answer) %>% 
  mutate(target_duration = round(target_duration * 1000, 2))

par_data <- par_data %>% mutate(target = if_else(target, 1, 0)) 
par_data <- par_data %>% mutate(answer = if_else(answer,  1, 0))
par_data <- par_data %>% mutate(participant = case_when(participant == "12" ~ "012", TRUE ~ participant))

# Participant data --------------------------------------------------------------------------------------

participant_001 <-data.frame(par_data %>%   filter(participant =="001", condition == "fully_masked", session < 14))
participant_003 <- data.frame(par_data %>%  filter(participant =="003", condition == "fully_masked"))   
participant_006 <- data.frame(par_data %>%  filter(participant =="006", condition == "fully_masked", session < 3)) 
participant_007 <- data.frame(par_data %>%  filter(participant =="007", condition == "fully_masked", session < 6))
participant_010 <- data.frame(par_data %>%  filter(participant =="010", condition == "fully_masked", session < 3))
participant_012 <- data.frame(par_data %>%  filter(participant == "012", condition == "fully_masked"))

# Creating Contingency tables-----------------------------------------------------------------------------------------------------------------------------------

target_df <- function(df) {
  result <- df %>% split(df, f = df$target_duration)
  return(result)
}

par01 <- target_df(participant_001)
par03 <- target_df(participant_003)
par06 <- target_df(participant_006)
par07 <- target_df(participant_007)
par10 <- target_df(participant_010)

# Creating Contingency table for each target duration--------------------------------------------------------------------

contingency_table <- function(par_df){
  tables <- lapply(par_df, function(df) {
    table(df$target, df$answer)
  })
}



# Applying the contingency function to all participant data----------------------------------------------------------------------------------------------------------------

  
contingency_table_01 <- contingency_table(par01)
contingency_table_03<- contingency_table(par03)
contingency_table_06<- contingency_table(par06)
contingency_table_07<- contingency_table(par07)
contingency_table_10<- contingency_table(par10)




# performing Bayesian Contingency table analyses fro all the participants---------------------------------------------------------------------------------------------------------------------------------

bayesian_analysis<- function(table){
  target_duration <- c("16.67","12.5","8.33","4.17")
  results <- list()
  for (i in target_duration){
      if( is.null(table[[i]])){
        next
              
      }
    else{
      temp <- contingencyTableBF(table[[i]], sampleType = "indepMulti", fixedMargin = "rows")
      results <-c (results,temp)
    }
  }
  return(results)
}


# Analyzing participants data using Bayesian contingency table analysis----------------------------------------------------------------------------------------------------------------
# If we have a BF > 1, then we go a target duration lower
# bayes_analyses_p1 <- bayesian_analysis(contingency_table_01)
# bayes_analyses_p3 <- bayesian_analysis(contingency_table_03)
# bayes_analyses_p6 <- bayesian_analysis(contingency_table_06)
# bayes_analyses_p7 <- bayesian_analysis(contingency_table_07)
# bayes_analyses_p10 <- bayesian_analysis(contingency_table_10)



# print(bayes_analyses_p1)
# print(bayes_analyses_p3)
# print(bayes_analyses_p6)
# print(bayes_analyses_p7)
# print(bayes_analyses_p10)








# Bayesian Logistic Regression -----------------------------------------------------------------------------------------

# How well the Stimuli predicts the response


bayes_model <- function(df){
  target_duration <- c("16.67","12.5","8.33","4.17")
  for (i in target_duration){
    argName <- deparse(substitute(i))
    if (any(i == df$target_duration)){
      df_target <- filter(df, target_duration == i)
      bayes_model_binary <- brm(
        data = df_target,
        family = bernoulli (link = "logit"),
        formula =  answer ~ target ,
        prior = c(prior(normal(0, 5), class = b),
                   prior(normal(0, 5), class = Intercept)),  
        iter = 21000,
        chains = 4,
        warmup = 11000,
        save_pars = save_pars(all = TRUE),
      )
      variable_name <- paste0("logistic_reg_p", df_target$participant[1],"_", argName)
      variable_name <- gsub("\"", "", variable_name)
      assign(variable_name, bayes_model_binary, envir = .GlobalEnv)
    }
  }
}

# Participant data -> The stimuli predicts the response
# 
bayes_model(participant_001)
# print(logistic_reg_p001_16.67)
# plot(logistic_reg_p001_16.67)
bayes_model(participant_003)
bayes_model(participant_006)
bayes_model(participant_007)
bayes_model(participant_010)
# print(logistic_reg_p010_16.67)
# plot(logistic_reg_p010_16.67)


# print(logistic_reg_01)
# print(logistic_reg_03)
# print(logistic_reg_06)
# print(logistic_reg_07)
# print(logistic_reg_10)


# How well the 1 predicts the response - INTERCEPT


bayes_model_intercept <- function(df){
  target_duration <- c("16.67","12.5","8.33","4.17")
  for (i in target_duration){
    argName <- deparse(substitute(i))
    if (any(i == df$target_duration)){
      df_target <- filter(df, target_duration == i)
      bayes_model_binary <- brm(
        data = df_target,
        family = bernoulli (link = "logit"),
        formula =  answer ~ 1 ,
        prior = c(
          prior(normal(0, 5), class = 'Intercept')),
        iter = 21000,
        chains = 4,
        warmup = 11000,
        save_pars = save_pars(all = TRUE),
      )
      variable_name <- paste0("logistic_int_p", df_target$participant[1],"_", argName)
      variable_name <- gsub("\"", "", variable_name)
      assign(variable_name, bayes_model_binary, envir = .GlobalEnv)
    }
  }
}


bayes_model_intercept(participant_001)
print(logistic_int_p001_16.67)
# plot(logistic_int_p001_16.67)
bayes_model_intercept(participant_003)
bayes_model_intercept(participant_006)
bayes_model_intercept(participant_007)
bayes_model_intercept(participant_010)
# print(logistic_int_p010_16.67)
# plot(logistic_int_p010_16.67)


# # Comparing the models for Behavioral data------------------------------------------------------------------
# 
# 
# # Participant 1, Target Duration 16.67
p1_bayes_factor_16.67 <- bayesfactor_models(logistic_int_p001_16.67,logistic_reg_p001_16.67)
# update(p1_bayes_factor_16.67, reference = "top")
# as.matrix(p1_bayes_factor_16.67)
 print(p1_bayes_factor_16.67)
# # 
# Participant 1, Target Duration 12.5
p1_bayes_factor_12.5 <- bayesfactor_models(logistic_int_p001_12.5, logistic_reg_p001_12.5, verbose = FALSE)

#Participant 1, Target Duration 8.33
p1_bayes_factor_8.33 <- bayesfactor_models(logistic_int_p001_8.33, logistic_reg_p001_8.33)

# # Participant 1, Target Duration 4.17
p1_bayes_factor_4.17 <- bayesfactor_models(logistic_int_p001_4.17, logistic_reg_p001_4.17)
#
# # Participant 3, Target Duration 16.67
p3_bayes_factor_16.67 <- bayesfactor_models(logistic_int_p003_16.67, logistic_reg_p003_16.67)
#
# # Participant 3, Target Duration 12.5
p3_bayes_factor_12.5 <- bayesfactor_models(logistic_int_p003_12.5, logistic_reg_p003_12.5)

# Participant 3, Target Duration 8.33
p3_bayes_factor_8.33 <- bayesfactor_models(logistic_int_p003_8.33, logistic_reg_p003_8.33)

# Participant 3, Target Duration 4.17
p3_bayes_factor_4.17 <- bayesfactor_models(logistic_int_p003_4.17, logistic_reg_p003_4.17)

# Participant 6, Target Duration 16.67
p6_bayes_factor_16.67 <- bayesfactor_models(logistic_int_p006_16.67, logistic_reg_p006_16.67)


# Participant 7, Target Duration 16.67
p7_bayes_factor_16.67 <- bayesfactor_models(logistic_int_p007_16.67, logistic_reg_p007_16.67)

# Participant 7, Target Duration 12.5
p7_bayes_factor_12.5 <- bayesfactor_models(logistic_int_p007_12.5, logistic_reg_p007_12.5)


# Participant 10, Target Duration 16.67
p10_bayes_factor_16.67 <- bayesfactor_models(logistic_int_p010_16.67, logistic_reg_p010_16.67)


# Printing the results


print(p1_bayes_factor_16.67)
print(p1_bayes_factor_12.5)
print(p1_bayes_factor_8.33)
print(p1_bayes_factor_4.17)
print(p3_bayes_factor_16.67)
print(p3_bayes_factor_12.5)
print(p3_bayes_factor_8.33)
print(p3_bayes_factor_4.17)
print(p6_bayes_factor_16.67)
print(p7_bayes_factor_16.67)
print(p7_bayes_factor_12.5)
print(p10_bayes_factor_16.67)

