#This script include plots of the sensitivy analysis to check which parameters that suited best prior to all final model simulations
#It involved testing the switchers in NetLogo of habitat depletion and patch meomory, keeping them turned on or off. 
#These combinations were tested for number of seals alive, mean blubber, daily enery expenditure. 

#files are read in based on output from NetLogo
#Scenario 1 =habONpatchON file 
#Scenario 2 =habONpatchOFF file
#Scenario 3 =habOFFpatchOFF file
#Scenario 4 =habOFFpatchON file

# read in processed data, that are used as a proxy for all the four seperate files
read_and_process_data <- function(file_path) {
  data <- read.csv(file_path, header = FALSE)
  colnames(data) <- c("expindex", "ticks", "who", "dayNumber", "currenttime", "activity", "fishConsumed_g",
                      "TotalfishConsumed_g", "daily_ee", "daily_ei", "dailyNetEnergy", "DaysWithNegativeDNE", "typeOfHa", "fishConsumed_g_longResting", "distHaulOutDigestion", "Blength", "Tmass", "LBM",
                      "ResMass", "sex", "xcor", "ycor", "[km_25_id] of patch-here", "[km_5_id] of patch-here", "foraging_trip","durationOfresting", "previous_ho" )
  data <- data[, c("dayNumber", "who")]
  data$unique_id <- data$who  # Assuming 'who' can be used as a unique identifier
  return(data)
}

# Function to plot number of seals alive
plot_seals_alive <- function(data, title, color) {
  seals_alive <- data %>%
    group_by(dayNumber) %>%
    summarize(num_seals = n_distinct(unique_id))
  
  plot <- ggplot(seals_alive, aes(x = dayNumber, y = num_seals)) +
    geom_step(color = color) +
    labs(x = "Time (Day)", y = "Number of Seals Alive", title = title) +
    ylim(0, 50) +
    xlim(0, 90) +
    theme_minimal()
  
  return(plot)
}

# Reading in the file path of the output-results from NetLogo.
# Scenario 1
sensitivity1_seals <- read_and_process_data("/Users/pernillerubinwollan/Documents/master netlogo/AgentSeal -master mine koder/Output/Movement_simulation_sens_lowerhabonpatchon.csv")
habONpatchON <- plot_seals_alive(sensitivity1_seals, "habONpatchON", "orange")

# Scenario 2
sensitivity2_seals <- read_and_process_data("/Users/pernillerubinwollan/Documents/master netlogo/AgentSeal -master mine koder/Output/Movement_simulation_sens_lowerhabonpatchoff.csv")
habONpatchOFF <- plot_seals_alive(sensitivity2_seals, "habONpatchOFF", "lightgreen")

# Scenario 3
sensitivity3_seals <- read_and_process_data("/Users/pernillerubinwollan/Documents/master netlogo/AgentSeal -master mine koder/Output/Movement_simulation_sens_lowerhaboffpatchoff.csv")
habOFFpatchOFF <- plot_seals_alive(sensitivity3_seals, "habOFFpatchOFF", "lightblue")

# Scenario 4
sensitivity4_seals <- read_and_process_data("/Users/pernillerubinwollan/Documents/master netlogo/AgentSeal -master mine koder/Output/Movement_simulation_sens_lowerhaboffpatchon.csv")
habOFFpatchON <- plot_seals_alive(sensitivity4_seals, "habOFFpatchON", "purple")

library(patchwork)
(habONpatchON| habONpatchOFF) / (habOFFpatchOFF | habOFFpatchON)


# testing blubber, for all scenarios Meanblubber for all area was manipulated to a combined dataset in excel, and saved as a csv file
sim_blubber <- read.csv("/Users/pernillerubinwollan/Documents/master netlogo/AgentSeal -master mine koder/AgentSeal1.0.nlogo.newsens Mean blubber_all .csv")

# Create the ggplot
ggplot(sim_blubber, aes(x = dayNumber)) +
  geom_line(aes(y = habONpatchON, color = "habONpatchON"), size = 0.5) +
  geom_line(aes(y = habONpatchOFF, color = "habONpatchOFF"), size = 0.5) +
  geom_line(aes(y = habOFFpatchOFF, color = "habOFFpatchOFF"), size = 0.5) +
  geom_line(aes(y = habOFFpatchON, color = "habOFFpatchON"), size = 0.5) +
  scale_color_manual(values = c("habONpatchON" = "orange",
                                "habONpatchOFF" = "lightgreen",
                                "habOFFpatchOFF" = "lightblue",
                                "habOFFpatchON" = "purple")) +
  labs(x = "Time (day)", y = "Mean Blubber (%)", color = "Sensitivity simulation") +
  ylim(0, 50) +
  theme_minimal()

#Lastly finding out if daily energy expenditure was changed by the different scenarios.
habonpatchon_dailyee <- read.csv("/Users/pernillerubinwollan/Documents/master netlogo/AgentSeal -master mine koder/NLlower_senstesthabonpatchon_Energy expenditure (MJ).csv")
habonpatchon_dailyee <- habonpatchon_dailyee[,c("dayNumber", "dailyee")]

habonpatchoff_dailee <- read.csv("/Users/pernillerubinwollan/Documents/master netlogo/AgentSeal -master mine koder/NLlower_senstesthabonpatchoff_Energy expenditure (MJ).csv")
habonpatchoff_dailee <- habonpatchoff_dailee[,c("dayNumber", "dailyee")]

haboffpatchon_dailee <- read.csv("/Users/pernillerubinwollan/Documents/master netlogo/AgentSeal -master mine koder/NLlower_senstesthaboffpatchon_Energy expenditure (MJ).csv")
haboffpatchon_dailee <- haboffpatchon_dailee[,c("dayNumber", "dailyee")]

haboffpatchoff_dailee <- read.csv("/Users/pernillerubinwollan/Documents/master netlogo/AgentSeal -master mine koder/NLlower_senstesthaboffpatchoff_Energy expenditure (MJ).csv")
haboffpatchoff_dailee <- haboffpatchoff_dailee[,c("dayNumber", "dailyee")]

# Combine all datasets into one
combined_data <- rbind(
  transform(habonpatchon_dailyee, dataset = "habONpatchON"),
  transform(habonpatchoff_dailee, dataset = "habONpatchOFF"),
  transform(haboffpatchon_dailee, dataset = "habOFFpatchOFF"),
  transform(haboffpatchoff_dailee, dataset = "habOFFpatchON")
)

# Create a named color palette
color_palette <- c("habONpatchON" = "orange",
                   "habONpatchOFF" = "lightgreen",
                   "habOFFpatchOFF" = "lightblue",
                   "habOFFpatchON" = "purple")

# Plot the data
ggplot(combined_data, aes(x = dayNumber, y = dailyee, color = dataset)) +
  geom_line() +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 25)) +
  labs(x = "Time (day)", y = "Energy expenditure (MJ/day)", color = "Sensitivity simulation") +
  scale_color_manual(values = color_palette) +
  theme_bw()


#sensitivity analysis regarding activity distribution to test how robust the model was, and to 
#check if days had to be excluded prior to calculation
activity <- fread("/Users/pernillerubinwollan/Documents/master netlogo/AgentSeal -master mine koder/Output/Movement_simulation_new_lowerNL_2test.csv")

day_numbers <- c(0,1,5,15,50,110,160,220,255,290,320,360)

# Create a bar plot of activities for the specified DayNumbers 
ggplot() +
  geom_bar(
    data = activity %>% filter(V4 %in% day_numbers),
    aes(x = factor(V4), fill = V6),
    position = "fill"
  ) +
  labs(x = "DayNumber", y = "Proportion", fill = "Activity") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()


