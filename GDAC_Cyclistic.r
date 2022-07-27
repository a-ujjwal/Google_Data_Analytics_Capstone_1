# STEP 1: Load the required libraries
library(tidyverse) 
library(lubridate) # to work with dates and times
library(janitor)   # to clean data


# STEP 2: Load the monthly datasets
jan_2021<-read_csv( "data-csv/202101-divvy-tripdata.csv" ) # data for January
feb_2021<-read_csv( "data-csv/202102-divvy-tripdata.csv" ) # data for February
mar_2021<-read_csv( "data-csv/202103-divvy-tripdata.csv" ) # data for March
apr_2021<-read_csv( "data-csv/202104-divvy-tripdata.csv" ) # data for April
may_2021<-read_csv( "data-csv/202105-divvy-tripdata.csv" ) # data for May
jun_2021<-read_csv( "data-csv/202106-divvy-tripdata.csv" ) # data for June
jul_2021<-read_csv( "data-csv/202107-divvy-tripdata.csv" ) # data for July
aug_2021<-read_csv( "data-csv/202108-divvy-tripdata.csv" ) # data for August
sep_2021<-read_csv( "data-csv/202109-divvy-tripdata.csv" ) # data for September
oct_2021<-read_csv( "data-csv/202110-divvy-tripdata.csv" ) # data for October
nov_2021<-read_csv( "data-csv/202111-divvy-tripdata.csv" ) # data for November
dec_2021<-read_csv( "data-csv/202112-divvy-tripdata.csv" ) # data for December


# STEP 3: Merging the datasets
compare_df_cols (jan_2021, feb_2021, mar_2021, apr_2021, may_2021, jun_2021, jul_2021, aug_2021, sep_2021, oct_2021, nov_2021, dec_2021) # to compare the columns of the data frames and check for any outliers

all_trips<- rbind(jan_2021, feb_2021, mar_2021, apr_2021, may_2021, jun_2021, jul_2021, aug_2021, sep_2021, oct_2021, nov_2021, dec_2021) # merge all the data frames together

str(all_trips) # displays the structure of the newly formed dataset


# STEP 4: Cleaning the dataset
all_trips<- drop_na(all_trips) # Removes all NA values from the data frame "all_trips"

rm(jan_2021, feb_2021, mar_2021, apr_2021, may_2021, jun_2021, jul_2021, aug_2021, sep_2021, oct_2021, nov_2021, dec_2021) # Removes all surplus data frames


all_trips<- mutate( all_trips, trip_duration= round(difftime( all_trips$ended_at, all_trips$started_at, units = 'mins' ), 3)) %>%
  mutate( Weekday= weekdays(all_trips$started_at), Month=month.abb[month(all_trips$started_at)])  # Adding the trip_duration, Weekday, and Month columns


range(all_trips$trip_duration) # to check the max and min values of trip duration


all_trips<- all_trips[all_trips$trip_duration>0, ] # delete data points with negative time


all_trips<- arrange(all_trips, all_trips$started_at) # arranging the data in ascending order




# Step 5: Descriptive Analysis
mean(all_trips$trip_duration) # Mean of trip_duration


median(all_trips$trip_duration) # Median of trip_duration


max(all_trips$trip_duration) # Maximum of trip_duration


min(all_trips$trip_duration) # Minimum of trip_duration


mean_by_membership<- aggregate(all_trips$trip_duration~all_trips$member_casual, FUN=mean)  # Mean of trip_duration by membership type
colnames(mean_by_membership)<- c("Membership Type", "Average Trip Duration")
mean_by_membership


median_by_membership<-aggregate(all_trips$trip_duration~all_trips$member_casual, FUN=median) # Median of trip_duration by membership type
colnames(median_by_membership)<- c("Membership Type", "Median of Trip Duration")
median_by_membership


max_by_membership<-aggregate(all_trips$trip_duration~all_trips$member_casual, FUN=max) # Maximum of trip_duration by membership type
colnames(max_by_membership)<- c("Membership Type", "Maximum Duration")
max_by_membership


min_by_membership<-aggregate(all_trips$trip_duration~all_trips$member_casual, FUN=min) # Minimum of trip_duration by membership type
colnames(min_by_membership)<- c("Mebership Type", "Minimum Trip Duration")
min_by_membership


# Step 6: Exporting the Cleaned Dataset
write.csv( all_trips,"C:/Users/Ayushya Ujjwal/Desktop/all_trips.csv")


# Step 7: Visualizations

# Viz of Rider Distribution by Membership 
all_trips %>%  
  group_by(member_casual, nrow(all_trips)) %>%    
  summarise(number_of_trips=n()) %>% # to calculate total number of rides
  ggplot(aes(x=nrow(all_trips), y=number_of_trips, fill=member_casual))+
  geom_col( position="stack", width=0.5)+
  theme_void()+
  coord_polar(theta="y")+
  ggtitle("Rider Distribution by Membership")+
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(round(number_of_trips/nrow(all_trips)*100, 2),"%")),   position = position_stack(vjust = 0.5), size = 4)+
  theme(legend.position= "bottom")+
  scale_fill_discrete(name='Membership Type')



# Viz of Most Popular Bike Type
all_trips %>% 
  group_by(rideable_type) %>% 
  summarise(number_of_trips=n()) %>% # to calculate total number of rides
  ggplot(aes(x=rideable_type, y=number_of_trips, fill=rideable_type))+
  geom_col(width = ) +
  labs(title = "Most Popular Bike Type", x= "Bike Type", y= "Total Trips")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_discrete(name='Type of Bike')+
  geom_text(aes(label = scales::comma(number_of_trips) ), vjust = 0.5, hjust= 1, colour = "black")+
  coord_flip()+
  scale_fill_brewer(palette =  "Set1")+
  theme(legend.position="none")



# Viz of Average Ride Duration by Membership Type
all_trips %>% 
  group_by(member_casual) %>% 
  summarise(Average_Ride_Duration=round(mean(trip_duration),2)) %>%  # to calculate average trip duration
  ggplot(aes(x=member_casual, y=Average_Ride_Duration, fill=member_casual))+
  geom_col(width = 0.7)+
  ggtitle("Average Ride Duration by Membership Type")+
  xlab("Membership Type")+
  ylab("Average Ride Duration (in min)")+
  scale_fill_discrete(name='Membership Type')+
  geom_text(aes(label = Average_Ride_Duration), vjust = 1.5, colour = "black")



all_trips_v2<-all_trips # duplicating the dataset to alter it for further vizualisation
all_trips_v2$Month<-factor(all_trips_v2$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) # Arranging the Months in ascending order to get ordered columns in the visualization


# Viz of Rides Per month by Membership Type
all_trips_v2 %>% 
  group_by(member_casual, Month) %>% 
  summarise(number_of_trips=n()) %>% # to calculate total number of rides
  ggplot(aes(x=Month, y=number_of_trips, fill=member_casual))+
  geom_col(position = "dodge")+
  ggtitle("Rides Per month by Membership Type")+
  xlab("Month")+
  ylab("Total Trips")+
  theme(axis.text.x = element_text(angle = 25))+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_discrete(name='Membership Type')



# Viz of Average Monthly Ride duration by Membership Type
all_trips_v2 %>% 
  group_by(member_casual, Month) %>% 
  summarise(average_rides= round(mean(trip_duration), 0)) %>% # to calculate average trip duration
  ggplot(aes(x=Month, y=average_rides, fill=member_casual))+
  geom_col(width = 0.75)+
  ggtitle("Average Monthly Ride duration by Membership Type")+
  xlab("Month")+
  ylab("Average Rides (in min)")+
  theme(axis.text.x = element_text(angle = 25))+
  scale_fill_discrete(name='Membership Type')+
  geom_text(aes(label = average_rides), vjust = 1.5, colour = "black") 



# Viz of Total Daily Rides by Membership Type
 all_trips %>% 
  group_by(member_casual, Weekday) %>%     
  summarise(number_of_rides=n()) %>%     # to calculate total numebr of rides 
  ggplot(aes(x=Weekday, y=number_of_rides, fill=member_casual))+
  geom_col(position = "stack")+
  ggtitle("Total Daily Rides by Membership Type")+
  xlab("Weekday")+
  ylab("Total Rides")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_discrete(name='Membership Type')



# Viz of Daily Average Trip Duration by Membership Type
all_trips %>% 
  group_by(member_casual, Weekday) %>% 
  summarise(avg_ride_duration=round(mean(trip_duration), 2)) %>%  # calculating the average ride duration and rounding it to 2 decimal places
  ggplot(aes(x=Weekday, y=avg_ride_duration, fill=member_casual))+
  geom_col()+
  ggtitle("Daily Average Trip Duration by Membership Type")+
  xlab("Weekdays")+
  ylab("Average Rides (in min)")+
  scale_fill_discrete(name='Membership Type')+
  geom_text(aes(label = avg_ride_duration), vjust = 1.5, colour = "black")



# Viz of Top 5 Start Stations
all_trips %>% 
  group_by(member_casual, start_station_name) %>%  
  summarise(number_of_trips=n()) %>% 
  arrange(desc(number_of_trips)) %>% 
  slice(1:5) %>% # to keep only the first 5 results
  ggplot(aes(x=reorder(start_station_name, -number_of_trips), y=number_of_trips, fill= member_casual))+
  geom_col()+
  ggtitle("Top 5 Start Stations")+
  xlab("Station Name")+
  ylab("Total Rides")+
  theme(axis.text.x = element_text(angle = 30))+ # inclining the angle of x-axis text for better readability
  scale_fill_discrete(name='Membership Type')+
  geom_text(aes(label = number_of_trips), vjust = 1.5, colour = "black")



# Viz of Top 5 End Stations
all_trips %>% 
  group_by(member_casual, end_station_name) %>% 
  summarise(number_of_trips=n()) %>% 
  arrange(desc(number_of_trips)) %>% 
  slice(1:5) %>% # to keep only the first 5 results
  ggplot(aes(x=reorder(end_station_name, -number_of_trips), y=number_of_trips, fill= member_casual))+
  geom_col()+
  ggtitle("Top 5 End Stations")+
  xlab("Station Name")+
  ylab("Total Rides")+
  theme(axis.text.x = element_text(angle = 30))+ # inclining the angle of x-axis text for better readability
  scale_fill_discrete(name='Membership Type')+
  geom_text(aes(label = number_of_trips), vjust = 1.5, colour = "black")