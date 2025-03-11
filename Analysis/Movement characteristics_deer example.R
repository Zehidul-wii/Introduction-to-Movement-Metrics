library(lubridate)
library(move)
library(amt) 
library(dplyr)
library(ggplot2)
library(tidyverse)
library(viridis)
library(RColorBrewer)
library(sf)


## Set directory
setwd("C:/Users/student/Desktop/working folder/UMD/Teaching")


# Input CSV file
deer <- read.csv("Movement chareacteristics_Deer.csv")

# Inspect the data
head(deer)
str(deer)

##make a new column
#deer$ts <- as.POSIXct(lubridate::mdy_hm(deer$datetime_local))
deer$ts <- as.POSIXct(lubridate::mdy(deer$Date) + lubridate::hm(deer$GMT_TimeStamp))

#check the local time zone
Sys.timezone(location = TRUE)
deer$ts<-as.POSIXct(deer$ts, format="%Y-%m-%d %H:%M:%0S", tz = "UTC")
#deer$ts<-as.POSIXct(deer$ts, format="%Y-%m-%d %H:%M:%0S", tz = "America/New_York")

#' ### Data cleaning
#' Delete observations where missing lat or long or a timestamp.  There are no missing
#' observations in this data set, but it is still good practice to check.
ind <- complete.cases(deer[, c("location_long", "location_lat", "ts")]) # x= longitude(UTM E)/ y= latitude (UTM N)


# The number of relocation with missing coordinates or timestamp (if any).
# Check how many are complete
table(ind)

# Keep only complete cases
deer <- deer %>% filter(ind)


#' Check for duplicated observations (ones with same lat, long, timestamp,
#'  and individual identifier). There are no duplicate
#' observations in this data set, but it is still good practice to check.
ind2 <- deer %>% 
  select(ts, location_long, location_lat, individual_local_identifier) %>%
  duplicated
sum(ind2) # no duplicates

# Remove duplicates if any
deer <- deer %>% filter(!ind2)


# Using `ggplot2` to plot the data
# Use separate axes for each individual (add scales="free" to facet_wrap)
ggplot(deer, aes(x = location_long, 
                 y = location_lat)) + geom_point() +
  facet_wrap(~individual_local_identifier, scales = "free")


# Now, we plot all individuals in single plot
ggplot(deer, 
       aes(location_long, location_lat, color = individual_local_identifier, 
           group = individual_local_identifier))+
  geom_point() + coord_equal() +
  theme(legend.position = "bottom")


## Creating a track in amt
# Before we can use the `amt` package to calculate step lengths, turn angles and oher movement menterics
# we need to add a class (track) to the data. Then, we can summarize the data by individual, month, etc.
#If we have a data set with  projected coordinates (UTM), we can use:
#trk <- make_track(deer, .x=utm.easting, .y=utm.northing, .t=timestamp, id = individual_local.identifier) 

# When using lat and lon
trk <- make_track(deer, 
                  .x = location_long, 
                  .y = location_lat, 
                  date_local = date_local,
                  study_local_timestamp = study_local_timestamp,
                  .t = ts, 
                  id = individual_local_identifier, 
                  crs = 4326)  # WGS 84 Lat/Lon

# Transform to Projected Coordinated System UTM Zone 18N (EPSG: 32618)
trk <- transform_coords(trk, 32618)

# Now  calculate day/night from the movement track
trk <- trk %>% time_of_day(include.crepuscule = TRUE) 

###
trk.class<-class(trk)

#' ## Movement Metrics
#' - dir_rel will calculate turning angles (relative angles)
#' - step_lengths will calculate distances between points
#' - nsd = Net Squared Displacement (distance from first point)

#' #' Note:  we have to calculate these characteristics separately for each 
#' individual (to avoid calculating a distance between say the last observation
#' of the first individual and the first observation of the second individual)

#' #'To do this, we could loop through individuals, calculate these
#' characteristics for each individual, then rbind the data 
#' back together. Or, use nested data frames and the map function
#' in the `purrr` package to do this with very little code. 
 
 
#' #' To see how nesting works, we can create a nested object by individual
nesttrk<-trk %>% nest(-id) 
nesttrk


#' Each row contains data from an individual.  For example, we can access data
#' from the first individual using:
nesttrk$data[[1]]

#' We could calculate movement characteristics by individual using:
data.steplength <-step_lengths(nesttrk$data[[1]])
head(data.steplength)

# Or we can add a columns to each nested column of data using `purrr::map`
trk1 <-trk %>% nest(-id) %>% 
  mutate(dir_abs = map(data, direction_abs,full_circle=TRUE, zero="N"), 
         dir_rel = map(data, direction_rel), 
         sl = map(data, step_lengths),
         nsd =map(data, nsd))%>%unnest()

###
class(trk1) <- trk.class
trk1

#' Now, calculate month, year, hour, week of each observation and add these to the dataset.
#' Unlike the movement characteristics, these calculations can be done all at once, 
#' since they do not utilize successive observations (like step lengths and turn angles do).
trk1 <- trk1 %>% 
  mutate(
    week = week(t_),
    month = month(t_, label=TRUE), 
    year = year(t_),
    hour = hour(t_))


###save 
write.csv(trk1,"movement_metrics_deer.csv", row.names = FALSE)

###Exploring more movement metrics (https://cran.r-project.org/web/packages/amt/amt.pdf)
#tot_dist()
#cum_dist()
#straightness()
#msd()
#intensity_use()


###  Some plots of movement characteristics ####

#'Step length distribution by day/night, hour, month
ggplot(trk1, aes(x = tod_, y = log(sl), fill = tod_)) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.6) + 
  facet_wrap(~id, scales = "free_y") +             
  theme_minimal(base_size = 14) + 
  scale_fill_manual(values = c("day" = "#FFC300", "night" = "#1F77B4")) +  # Fill colors for day and night
  xlab("Time of Day") + 
  ylab("Step Length (m)") + 
  ggtitle("Step Length Distribution by Day/Night") + 
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"))



#' ### Turning angles 
#' Note: a 0 indicates the animal continued to move in a straight line, a $\pi$ 
#' indicates the animal turned around (but note, resting + measurement error often can
#' make it look like the animal turned around).
ggplot(trk1, aes(x = dir_rel, y = ..density.., fill = ..density..)) + 
  geom_histogram(breaks = seq(-pi, pi, length = 20), color = "black", alpha = 0.8) + 
  coord_polar(start = 0) + 
  theme_minimal() +
  scale_fill_gradient(low = "skyblue", high = "darkblue") +  # Gradient fill for better visualization
  ylab("Density") + 
  ggtitle("Turn Angle Distribution") + 
  scale_x_continuous(
    limits = c(-pi, pi), 
    breaks = c(-pi, -pi/2, 0, pi/2, pi), 
    labels = c("-π", "-π/2", "0", "π/2", "π")
  ) +
  facet_wrap(~id, ncol = 2) +  # Facet by id for individual distributions
  theme(
    panel.grid.major = element_line(color = "gray90"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold")
  )



### Net-squared displacement

ggplot(trk1, aes(x = t_, y = nsd, color = id, group = id)) + 
  geom_path(size = 1) + 
  facet_wrap(~id, scales = "free") +
  labs(
    title = "Net-Squared Displacement",
    x = "Date",
    y = "Net-Squared Displacement (m2)",
    color = "Individual ID"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))


####End####



