## Trend analysis for packer pressure telemetry
## AMC 2016-11
## V2 includes packer pressures from all wells
## V3 uses new packer pressure report that includes all wells in one file
## V4 2018-07: adds plots
## V5: 2018-07: tries to improve functionality of finding minima of packer pressure data
## V6: 2018-08: tidying things up a bit
## V7: 2019-01: try finding daily averages instead of daily minimums - some days don't always have clear peaks
## FINAL: 2019-11: Cleaned up version sent to groundwater program for use

# Load packages
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(scales)
library(cowplot)
library(ggpubr)

# Load files
####################################
packers <- read_csv('Data/packers_december_2019.csv') # normal col_types parsing method isn't working well with this dataset - not sure why. This method will be more versatile
  packers$Timestamp <- as_datetime(packers$Timestamp) 


alarms <- read_csv('Data/Packer Pressure Alarm Summary.csv')

# add some date columns
####################################
packers <- packers %>%
  mutate(doy = yday(packers$Timestamp)) %>%
  mutate(hour = hour(packers$Timestamp)) %>%
  mutate(frac.day = (doy + hour/24)) %>%
  drop_na()


# Make the data frame tall so that summary stats can be calculated
#####################################
packers_tall <- gather(packers, Location, Pressure, `R-10`, `R-12`, `R-16`, `R-17`, `R-20`,
                       `R-23i`, `R-33`, `R-37`, `R-40`, `R-41`, `R-43`, `R-44`, `R-45`,
                       `R-49`, `R-50`, `R-51`, `R-52`, `R-53`, `R-54`, `R-55`, `R-56`,
                       `R-57`, `R-61`) 

# Find mean daily packer pressure using the tall dataframe
######################################
packers_summ <- packers_tall %>%
  group_by(Location, doy) %>%
  summarize(mean_daily_pressure = mean(Pressure, na.rm = TRUE))

# Now make the df wide again - this creates a df with nrow = # of days, ncol = number of packers + date column
######################################
packers_wide <- packers_summ %>%
  spread(Location, mean_daily_pressure) %>%
  mutate(doy = doy + 0.5) # makes means plot at noon each day

rm(packers_summ) # remove packer summary df - don't need it anymore


# create a new dataframe with just data and not date - this will be used in the following for loop
#######################################
packer_data <- packers %>%
  select('R-10', 'R-12', 'R-16', 'R-17', 'R-20', 'R-23i', 'R-33', 'R-37', 'R-40',
         'R-41', 'R-43', 'R-44', 'R-45', 'R-49', 'R-50', 'R-51', 'R-52', 'R-53',
         'R-54', 'R-55', 'R-56', 'R-57', 'R-61')

# Find all current pressures and combine into one variable
# Current pressure is used later to calculate "days until action pressure"
well_name <- alarms$`Well No.`

current_p <- data.frame(matrix(data=NA, nrow = length(well_name), ncol = 1))
# Take last value for each well in "file"
# Loop thru each well and pick out last value that is not NA and put into current_p
for (i in 1:length(well_name)){
  test <- !is.na(packer_data[,i])
  current_p[i,1] <- tail(packer_data[test,i], n=1)
}

#####################################################################################
# Fit a regression through daily mean values and save into a new dataframe

# pull out some variables from the alarms df to use in the summary df
ap <- alarms$`Action Pressure`

summ_df <- data.frame(matrix(data = NA, nrow = length(well_name), ncol = 5))
names(summ_df) <- c('Well','Action Pressure','Current Pressure','Slope','Days until Action')
summ_df[,1] <- well_name
summ_df[,2] <- ap
summ_df[,3] <- current_p

for (i in 1:length(well_name)){
  fitline <- lm(packers_wide[[i+1]]~packers_wide[[1]])
  summ_df[i,4] <- as.numeric(coef(fitline)[2])
}

summ_df[,5] <- abs((summ_df$`Current Pressure` - summ_df$`Action Pressure`)/summ_df$Slope)

# Save file
write.csv(summ_df, paste0('Tables/', 'Packer_trends_', format(Sys.time(), "%Y_%m_%d"),'.csv'), na = "NA", row.names = T)

######################################################################################
# Make some plots to check that the code is working correctly

R10 <- ggplot(packers_wide, aes(doy, packers_wide[[2]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[1]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-10 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[1]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[1] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[1],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


#############################################
R12 <- ggplot(packers_wide, aes(doy, packers_wide[[3]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[2]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-12 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[2]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[2] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[2],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


#############################################
R16 <- ggplot(packers_wide, aes(doy, packers_wide[[4]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[3]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-16 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[3]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[3] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[3],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


#############################################
R17 <- ggplot(packers_wide, aes(doy, packers_wide[[5]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[4]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-17 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[4]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[4] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[4],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


#############################################
R20 <- ggplot(packers_wide, aes(doy, packers_wide[[6]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[5]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-20 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[5]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[5] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[5],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


#############################################
R23i <- ggplot(packers_wide, aes(doy, packers_wide[[7]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[6]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-23i Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[6]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[6] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[6],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


#############################################
R33 <- ggplot(packers_wide, aes(doy, packers_wide[[8]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[7]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-33 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[7]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[7] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[7],0), "Days until AP"),
                                     x=-Inf, y=Inf), hjust=-1.5, vjust=4, size=3, col="red")
  }


###############################################
R37 <- ggplot(packers_wide, aes(doy, packers_wide[[9]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[8]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-37 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[8]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[8] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[8],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


###############################################
R40 <-  ggplot(packers_wide, aes(doy, packers_wide[[10]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[9]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-40 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[9]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[9] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[9],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


###############################################
R41 <- ggplot(packers_wide, aes(doy, packers_wide[[11]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[10]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-41 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[10]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[10] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[10],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


###############################################
R43 <- ggplot(packers_wide, aes(doy, packers_wide[[12]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[11]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-43 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[11]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[11] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[11],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


###############################################
R44 <- ggplot(packers_wide, aes(doy, packers_wide[[13]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[12]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-44 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[12]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[12] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[12],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


###############################################
R45 <- ggplot(packers_wide, aes(doy, packers_wide[[14]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[13]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-45 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[13]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[13] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[13],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


###############################################
R49 <- ggplot(packers_wide, aes(doy, packers_wide[[15]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[14]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-49 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[14]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[14] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[14],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


###############################################
R50 <- ggplot(packers_wide, aes(doy, packers_wide[[16]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[15]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-50 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[15]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[15] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[15],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


###############################################
R51 <- ggplot(packers_wide, aes(doy, packers_wide[[17]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[16]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-51 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[16]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[16] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[16],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


###############################################
R52 <- ggplot(packers_wide, aes(doy, packers_wide[[18]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[17]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-52 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[17]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[17] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[17],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


###############################################
R53 <- ggplot(packers_wide, aes(doy, packers_wide[[19]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[18]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-53 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[18]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[18] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[18],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


###############################################
R54 <- ggplot(packers_wide, aes(doy, packers_wide[[20]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[19]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-54 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[19]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[19] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[19],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


###############################################
R55 <- ggplot(packers_wide, aes(doy, packers_wide[[21]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[20]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-55 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[20]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[20] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[20],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


###############################################
R56 <- ggplot(packers_wide, aes(doy, packers_wide[[22]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[21]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-56 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[21]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[21] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[21],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


###############################################
R57 <- ggplot(packers_wide, aes(doy, packers_wide[[23]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[22]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-57 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[22]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[22] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[22],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }


###############################################
R61 <- ggplot(packers_wide, aes(doy, packers_wide[[24]])) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  geom_line(data=packers, aes(frac.day, packer_data[[23]])) + 
  theme_bw() +
  labs(x="Day of year", y = "R-61 Packer Pressure (psi)") +
  geom_label(data=data.frame(), aes(label=paste("Action Pressure=", alarms$`Action Pressure`[[23]], "psi"), 
                                    x=-Inf, y=Inf), hjust=0, vjust=1, size=4) +
  if (summ_df$`Days until Action`[23] < 100) {
    geom_text(data=data.frame(), aes(label = paste(round(summ_df$`Days until Action`[23],0), "Days until action pressure"),
                                     x=-Inf, y=Inf), hjust=-0.8, vjust=2, size=4, col="red")
  }

#########################################################################################
# Arrange all the plots so they can be saved as one file

# print over multiple pages
plot_pages <- ggarrange(R10, R12, R16, R17, R20, R23i, R33, R37, R40, R41, R43, R44, R45, R49, R50,
                        R51, R52, R53, R54, R55, R56, R57, R61, nrow = 3, ncol = 2, align = 'v')

# Save file, appending current date to file name
ggexport(plot_pages, filename = paste0('Figures/', 'all_packer_trends_', format(Sys.time(), "%Y_%m_%d"), '.pdf'))


