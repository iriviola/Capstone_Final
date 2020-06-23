if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos= "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos= "http://cran.us.r-project.org")
if(!require(ggmap)) install.packages("ggmap", repos= "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos= "http://cran.us.r-project.org")
if(!require(maptools)) install.packages("maptools", repos= "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos= "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos= "http://cran.us.r-project.org")
if(!require(countrycode)) install.packages("countrycode", repos= "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos= "http://cran.us.r-project.org")
if(!require(recipes)) install.packages("recipes", repos= "http://cran.us.r-project.org")
if(!require(tidymodels)) install.packages("tidymodels", repos= "http://cran.us.r-project.org")
if(!require(themis)) install.packages("themis", repos= "http://cran.us.r-project.org")
if(!require(workflows)) install.packages("workflows", repos= "http://cran.us.r-project.org")
if(!require(tune)) install.packages("tune", repos= "http://cran.us.r-project.org")
if(!require(vip)) install.packages("vip", repos= "http://cran.us.r-project.org")
if(!require(janitor)) install.packages("janitor", repos= "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger" , repos= "http://cran.us.r-project.org")

##################LOAD DATABASE#############################
volcanic_eruption <- read_csv("~/Documents/Rscript/Volcanic eruption Holocene/database.csv")
#If you don't want to download the .cvs on your computer, here is the keggle link to the database page https://www.kaggle.com/smithsonian/volcanic-eruptions 
##############################################################
# Check Database, correct and wrangling
##############################################################
#Database structure
str(volcanic_eruption)
#Check if numbers are unique
length(unique(volcanic_eruption[, "Number"])) == volcanic_eruption[, .N]
#Checking if `Name` is unique 
length(unique(volcanic_eruption[, "Name"])) == volcanic_eruption[, .N]
unique(volcanic_eruption[duplicated(volcanic_eruption[, "Name"]), "Name"])
#Some Volcanoes are repeated because they gives more than one eruption during Holocene
#The names of teh counties are not unique, correct it using `countrycode` package and standardize
#Select the countries that define borderline volcanoes
volcanic_eruption[, Country_First = gsub("([[:alnum:]\\s]+)\\s?-.+", "\\1", "Country")]
#standardize using ISO3C
volcanic_eruption[, Iso3c = countrycode(Country_First, "country.name", "iso3c")]
volcanic_eruption[, Country_Name = countrycode(Iso3c, "iso3c", "country.name")]
#Check if there are any NAs in the data set
is.na(volcanic_eruption)
#How many NAs are present?
sum(is.na(volcanic_eruption))
#Check Where the NAs are
sum(is.na(volcanic_eruption$Number))
sum(is.na(volcanic_eruption$Name))
sum(is.na(volcanic_eruption$Country))
sum(is.na(volcanic_eruption$Region))
sum(is.na(volcanic_eruption$Type))
sum(is.na(volcanic_eruption$`Activity Evidence`))
sum(is.na(volcanic_eruption$`Last Known Eruption`))
sum(is.na(volcanic_eruption$Latitude))
sum(is.na(volcanic_eruption$Longitude))
sum(is.na(volcanic_eruption$`Elevation (Meters)`))
sum(is.na(volcanic_eruption$`Dominant Rock Type`))
sum(is.na(volcanic_eruption$`Tectonic Setting`))
#Replace NAs in Activity Evidence  with "Evidence Uncertain"
volcanic_eruption$`Activity Evidence`[is.na(volcanic_eruption$`Activity Evidence`)] <- "Evidence Uncertain"
#Replace NAs in Dominant Rock Type with "No Data"
volcanic_eruption$`Dominant Rock Type`[is.na(volcanic_eruption$`Dominant Rock Type`)] <- "No Data"
#Replace NAs in Tectonic Setting  with "Unknown"
volcanic_eruption$`Tectonic Setting`[is.na(volcanic_eruption$`Tectonic Setting`)] <- "Unknown"
#the eruption type are written differently
Type_sum <- volcanic_eruption %>% 
  select(Country) %>%
  group_by(volcanic_eruption$Type) %>%
  summarize(count = n())
data.table(Type_sum)
#To have the best and clear correction let's correct each tipe "manually"
volcanic_eruption <- volcanic_eruption %>%
  mutate(Type = ifelse(Type == "Caldera(s)", "Caldera", Type))
volcanic_eruption <- volcanic_eruption %>%
  mutate(Type = ifelse(Type == "Complex(es)", "Complex", Type))
volcanic_eruption <- volcanic_eruption %>%
  mutate(Type = ifelse(Type == "Fissure vent(s)", "Fissure vent", Type))
volcanic_eruption <- volcanic_eruption %>%
  mutate(Type = ifelse(Type == "Fissure vent\t", "Fissure vent", Type))
volcanic_eruption <- volcanic_eruption %>%
  mutate(Type = ifelse(Type == "Lava cone(s)", "Lava cone", Type))
volcanic_eruption <- volcanic_eruption %>%
  mutate(Type = ifelse(Type == "Lava dome(s)", "Lava dome", Type))
volcanic_eruption <- volcanic_eruption %>%
  mutate(Type = ifelse(Type == "Maar(s)", "Maar", Type))
volcanic_eruption <- volcanic_eruption %>%
  mutate(Type = ifelse(Type == "Pyroclastic cone(s)", "Pyroclastic cone", Type))
volcanic_eruption <- volcanic_eruption %>%
  mutate(Type = ifelse(Type == "Shield", "Pyroclastic shield", Type))
volcanic_eruption <- volcanic_eruption %>%
  mutate(Type = ifelse(Type == "Shield(s)", "Pyroclastic shield", Type))
volcanic_eruption <- volcanic_eruption %>%
  mutate(Type = ifelse(Type == "Stratovolcano?", "Stratovolcano", Type))
volcanic_eruption <- volcanic_eruption %>%
  mutate(Type = ifelse(Type == "Stratovolcano(es)", "Stratovolcano", Type))
volcanic_eruption <- volcanic_eruption %>%
  mutate(Type = ifelse(Type == "Submarine(es)", "Submarine", Type))
volcanic_eruption <- volcanic_eruption %>%
  mutate(Type = ifelse(Type == "Tuff cone(s)", "Tuff cone", Type))
volcanic_eruption <- volcanic_eruption %>%
  mutate(Type = ifelse(Type == "Volcanic field(s)", "Volcanic field", Type))
#Now that all the same type or eruption have the same name, let's see how many different typer of eruption happened during Holocene
Type_eruption <- volcanic_eruption %>% 
  select(Country) %>%
  group_by(volcanic_eruption$Type) %>%
  summarize(count = n())
data.table(Type_eruption) 
#correct last known eruption in Years (geological notation)
#First let's put NAs instead of "Unknown", this way, in the next step is possible to convert as.numeric and do calculation to have the correct notation
last_eruption <- ifelse(volcanic_eruption$`Last Known Eruption` == "Unknown", NA , volcanic_eruption$`Last Known Eruption`)
#Define Years as numeric
year <- as.numeric(gsub("(^\\d+).+", "\\1", last_eruption))
#Define the factor of addiction for BCE and CE
adding_year <- ifelse(grepl("BCE", last_eruption), +2020,0) & ifelse(grepl("CE", last_eruption), -2016,0)
#Let's perform addition of 2020 to BCEs to have the kY used in geological scale and add 0 to the CEs.
year_geo <- adding_year + year
#Let's relace teh column values
volcanic_eruption$`Last Known Eruption`<-year_geo 
#Now all the datas are in the correct geological and geographical notation.
####################################################################################
#Start data analysis
##############################################################################
#first step, let's consider different frequancies to understand the phenomenon of eruption in holocene
Frq_Activity<-   volcanic_eruption  %>% 
  select(`Last Known Eruption`, `Activity Evidence`) %>%
  group_by(`Last Known Eruption`, `Activity Evidence`) %>%
  summarize(count = n())
data.table(Frq_Activity)

#Holocene eruption plot
ggplot(Frq_Activity, aes(x = `Last Known Eruption`, y = count,
                                   colour = `Activity Evidence` , fill = `Activity Evidence`)) + 
    geom_bar(stat = "identity") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.text = element_text(size=8)) + 
    ggtitle("Eruptive activity")

ggplot(Frq_Activity, aes(x = `Last Known Eruption`)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_x_discrete(breaks = seq(0, 11700, by = 500)) +
  geom_histogram(stat="count")


####Before going on, let's replace NAs in "Last known Eruption" with "unknown" as it was before
volcanic_eruption$`Last Known Eruption`[is.na(volcanic_eruption$`Last Known Eruption`)] <- "Unknown"

##### Which Country has been more active in the last 11700 years?
Frq_Activity_country <-   volcanic_eruption  %>% 
  select(Country, `Activity Evidence`) %>%
  group_by(Country, `Activity Evidence`) %>%
  summarize(count = n())
data.table(Frq_Activity_country)
#######Plot the activity frequancy by country 
ggplot(Frq_Activity_country, aes(x =  Country, y = count, 
                                 colour = `Activity Evidence` , fill = `Activity Evidence`)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5), legend.text = element_text(size=8)) + 
  ggtitle("Activity Evidence Frequency")

####### Which Region has been more active in the last 11700 years?
Frq_Activity_region <-   volcanic_eruption  %>% 
  select(Region, `Activity Evidence`) %>%
  group_by(Region, `Activity Evidence`) %>%
  summarize(count = n())
data.table(Frq_Activity_region)
######Polt the region and the activity evicence
ggplot(Frq_Activity_region, aes(x = Region , y = count, 
                                colour = `Activity Evidence` , fill = `Activity Evidence`)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.text = element_text(size=8)) + 
  ggtitle("Region Activity Frequency")

#######What Type of volcanoes are present and have been active in the different region?
Frq_Type_region <-   volcanic_eruption  %>% 
  select(Region, Type) %>%
  group_by(Region, Type) %>%
  summarize(count = n())
data.table(Frq_Type_region)
#plot the region and type of volcanoes
ggplot(Frq_Type_region, aes(x = Region , y = count, 
                            colour = Type , fill = Type)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.text = element_text(size=8)) + 
  ggtitle("Regional Volcanoes Type Frequency")

######Let's consider Volcano type depending on tectonic settings
Frq_Type_Tectonic <-   volcanic_eruption  %>% 
  select(`Tectonic Setting`, Type) %>%
  group_by(`Tectonic Setting` , Type) %>%
  summarize(count = n())
data.table(Frq_Type_Tectonic)
######plot the region and type of volcanoes
ggplot(Frq_Type_Tectonic, aes(x = Type, y = count, 
                              colour = `Tectonic Setting` , fill = `Tectonic Setting`)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.text = element_text(size=8)) + 
  ggtitle("Volcanoes Type in Tectonic Settings")

#####Let's consider Volcano type and the Dominat Rock type
Frq_Type_Rock <-   volcanic_eruption  %>% 
  select(`Dominant Rock Type`, Type) %>%
  group_by(`Dominant Rock Type` , Type) %>%
  summarize(count = n())
data.table(Frq_Type_Rock)
#####plot the Dominant Rock type and type of volcanoes
ggplot(Frq_Type_Rock, aes(x =Type, y = `Dominant Rock Type`, 
                          colour = `Dominant Rock Type` , fill = `Dominant Rock Type`)) + 
  geom_point(stat = "identity", show.legend = FALSE) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.text = element_text(size=8)) + 
  ggtitle("Volcanoes Rocks vs Type")

#####Let's investigate tectonic setting and activity

Frq_Tect_activity <-   volcanic_eruption  %>% 
  select(`Tectonic Setting`, `Activity Evidence`) %>%
  group_by(`Tectonic Setting` , `Activity Evidence`) %>%
  summarize(count = n())
data.table(Frq_Tect_activity)
#####plot tectonic setting and activity
ggplot(Frq_Tect_activity, aes(x =`Tectonic Setting`, y = count, 
                          colour = `Activity Evidence` , fill = `Activity Evidence`)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.text = element_text(size=8)) + 
  ggtitle("Tectonic settings activity")

###################Let's consider how the eruption are distributed by tectonic settings around the world
world <- map_data("world")
ggplot() + geom_polygon(data = world, aes(x =  long, y = lat, group = group))+  
  geom_point(data = volcanic_eruption, aes(x = Longitude, y = Latitude, colour = `Tectonic Setting` , 
                                           alpha = 0.3 )) +
  theme(legend.text = element_text(size=8)) +
  stat_density2d(show.legend = FALSE) + xlab("Longitude") +ylab("Latitude") +
  coord_quickmap()
#############Let's consider how the eruption are distributed by ACTIVITY around the world
world <- map_data("world")
ggplot() + geom_polygon(data = world, aes(x =  long, y = lat, group = group))+  
  geom_point(data = volcanic_eruption, aes(x = Longitude, y = Latitude, colour = `Activity Evidence`, 
                                           alpha = 0.3 )) +
  stat_density2d(show.legend = FALSE) + xlab("Longitude") +ylab("Latitude") +
  coord_quickmap()
#############################################################################################
## BUILDING MODEL
#############################################################################
# Objective: Let's pedict volcan type eruption
###############################################################
##Eruption for each volcanos
Frq_avtivity_volc <-   volcanic_eruption  %>% 
  select(Name, `Activity Evidence`) %>%
  group_by(`Activity Evidence` , Name) %>%
  summarize(count = n())
data.table(Frq_avtivity_volc)
#Which volcano type is the more abundant and erupted more during holocene?
#Since every row is an eruption, just count the $type
volcanic_eruption %>% count(Type, sort = TRUE)
# Most adbundant are Stratovolcanoes and Pyroclastic Shield.
#since there is a large number of volcanoes types, let's group in  five classes: Stratovolcanoes, pyroclastic shield, submarine Pyroclastic cone and others
volcanoes_work <-
  volcanic_eruption %>% 
  transmute(
    volcano_type = 
      case_when(
        str_detect(Type, "Stratovolcano") ~ "Stratovolcano",
        str_detect(Type, "Pyroclastic shield")        ~ "Pyroclastic shield",
        str_detect(Type, "Submarine")        ~ "Submarine",
        str_detect(Type, "Pyroclastic cone")        ~ "Pyroclastic cone",
        TRUE                                              ~ "Other"
      ) ,
    Number,
    Latitude,
    Longitude,
    `Elevation (Meters)`,
    `Last Known Eruption`,
    `Tectonic Setting` ,
    `Dominant Rock Type`
  ) %>% 
  mutate_if(is.character, factor)
#Let's consider the Tettonic settings
volcanoes_work %>% 
  count(`Tectonic Setting`, sort = TRUE)
#Goup the Thectonic setting based tectonic type
volcanoes_work3 <-
  volcanoes_work %>% 
  transmute(
    volcano_type,
    Number,
    Latitude,
    Longitude,
    `Elevation (Meters)`,
    `Last Known Eruption`,
    `Tectonic` = 
      case_when(
        `Tectonic Setting` == "Subduction Zone / Continental Crust (>25 km)" ~ "Subduction Zone",
        `Tectonic Setting` == "Intraplate / Continental Crust (>25 km)" ~ "Intraplate",
        `Tectonic Setting` == "Subduction Zone / Oceanic Crust (< 15 km)" ~ "Subduction Zone",
        `Tectonic Setting` == "Rift Zone / Continental Crust (>25 km)" ~ "Rift Zone",
        `Tectonic Setting` == "Rift Zone / Oceanic Crust (< 15 km)" ~ "Rift Zone",
        `Tectonic Setting` == "Subduction Zone / Intermediate Crust (15-25 km)" ~ "Subduction Zone",
        `Tectonic Setting` == "Rift Zone / Intermediate Crust (15-25 km)" ~ "Rift Zone",
        `Tectonic Setting` == "Intraplate / Oceanic Crust (< 15 km)" ~ "Intraplate",
        `Tectonic Setting` == "Intraplate / Intermediate Crust (15-25 km)" ~ "Intraplate",
        `Tectonic Setting` == "Subduction Zone / Crust Thickness Unknown" ~ "Subduction Zone",
        TRUE                                              ~ "Unknown"
      )  ,
    `Dominant Rock Type`
  ) %>% 
  mutate_if(is.character, factor)
#dominant rock type
volcanoes_work %>% 
  count(`Dominant Rock Type`, sort = TRUE)

#LEt's group dominant rock type according to the general igneus rock types in geology

volcanoes_work4 <-
  volcanoes_work3 %>% 
  transmute(
    volcano_type,
    Number,
    Latitude,
    Longitude, 
    `Elevation (Meters)`,
    `Last Known Eruption`,
    `Tectonic`,
    Rock_Type= 
      case_when(
        `Dominant Rock Type` == "Andesite / Basaltic Andesite" ~ "Andesite",
        `Dominant Rock Type` == "Basalt / Picro-Basalt" ~ "Basalt",        
        `Dominant Rock Type` == "Dacite" ~ "Dacite",  
        `Dominant Rock Type` == "Trachybasalt / Tephrite Basanite" ~ "Trachybasalt",  
        `Dominant Rock Type` == "Rhyolite" ~ "Rhyolite",  
        `Dominant Rock Type` == "Trachyte / Trachydacite " ~ "Trachyte",  
        `Dominant Rock Type` == "Trachyandesite / Basaltic Trachyandesite" ~ "Trachyandesite",  
        TRUE                                              ~ "No Data"
      )  ,
  ) %>% 
  mutate_if(is.character, factor)

#Change name for the column "Elevation (meter)" to Elevation and "Last Known Eruprion" in Last_eruption
names(volcanoes_work4)[names(volcanoes_work4) == "Elevation (Meters)"] <- "Elevation"
names(volcanoes_work4)[names(volcanoes_work4) == "Last Known Eruption"] <- "Last_eruption"
#Create partition
index <- createDataPartition(volcanoes_work4$volcano_type, p = 0.9, list = FALSE)
volcano_work_training <- volcanoes_work4[index,]
volcano_work_testing  <- volcanoes_work4[-index,]
###########################################################################
#####    RUNNING METHODS PART 1 ####################################
############################################################################

#First Method: Knn method
fit <- train(volcano_type ~ ., method = "knn",
             tuneGrid = data.frame(k = seq(1, 15, 2)), data = volcano_work_training)
fit

############################
#Second Method: Random Forest
rf_fit <- train(as.factor(volcano_type)~ .,
                data = volcano_work_training, 
                method = "ranger")
rf_fit
#PRediction for voclavoes eruption with random forest
volcanoes_rf_pred <- predict(rf_fit, volcano_work_testing)
#Check confusion matrix
confusionMatrix(volcanoes_rf_pred, as.factor(volcano_work_testing$volcano_type))
#We can see that the values for single volcanoeser are too perfec to be real(0.99 for submarine?). So we have to preProcess
#Let's use prePRocess from caret package to center, scale and 
# identify and remove variables with near zero variance and perform pca
volcano_nzv_pca <- preProcess(select(volcanoes_work4, - volcano_type), 
                              method = c("center", "scale", "nzv", "pca"))
#The result tells that some components have been ignored. 
volcano_nzv_pca
# identify which variables were ignored, centered, scaled, etc
volcano_nzv_pca$method
# variables have been ignored.
class(volcanoes_work4$Tectonic)
class(volcanoes_work4$Rock_Type)
class(volcanoes_work4$Last_eruption)
###########three variables are factor

#Use step_dummy to change factor in numeric, this way the random forest model will consider also these variables
volcano_recipe <- recipe(volcano_type ~ ., data = volcanoes_work4)
volcano_dummies<- volcano_recipe %>% step_dummy(Tectonic, Rock_Type, Last_eruption, one_hot = TRUE,) 
volcano_dummies <-prep(volcano_dummies, training= volcanoes_work4)
volcano_work_d<- bake(volcano_dummies, new_data = volcanoes_work4)
unique(volcanoes_work4$Tectonic,volcanoes_work4$Rock_Type, volcanoes_work4$Last_eruption)
grep("^Tectonics","^Rock_Type","^Last_eruption", names(volcanoes_work4), value = TRUE)
#Now the database is correct. 
###########################################################################
####### RUNNING METHODS PART 2
###########################################################################
#We can split it in Test and Trainig and perform the Random Forest again:
set.seed(1) #set.seed(1, sample.kind="Rounding") for recent version of R 3.5 
test_index <- createDataPartition(volcano_work_d$volcano_type, p = 0.9, list = FALSE)
volcano_d_training <- volcano_work_d[test_index,]
volcano_d_testing  <- volcano_work_d[-test_index,]

# Resampling method using traincontrol to control of printing and resampling for train
fit_control <- trainControl(
  method = "oob", #oob beacuse we 'll use random forest
  number = 10)
# Run Random Forest 
set.seed(1)
volcano_rf_fit <- train(as.factor(volcano_type) ~ ., 
                        data = volcano_d_training, 
                        method = "ranger",
                        trControl = fit_control)

volcano_rf_fit
#THe Accurancy improved!
#Re-predict outcome on a test set
volcanoes_rf_pred2 <- predict(volcano_rf_fit, volcano_d_testing)
# compare predicted outcome and true outcome
confusionMatrix(volcanoes_rf_pred2, as.factor(volcano_d_testing$volcano_type))
#better!!! 
########## Do another random forest in a different way using as set_mode (Classification)

#Set the method
volcano_rf_fit2 <-
  rand_forest(trees = 1000) %>% 
  set_mode("classification") %>% 
  set_engine("ranger")
volcano_rf_fit2
#Since "classification" as been used as Mode in RF, a workflow is needed to keep this working
#First do a new recipe
volcano_recipe2 <- recipe(volcano_type ~ ., data = volcano_work_d)
#this time use the workflow function to constrain recipe and model
volcano_wf <-
  workflow() %>% 
  add_recipe(volcano_recipe2) %>% 
  add_model(volcano_rf_fit2)
#Bootstrap will define the resamples in the analysis
volcano_boot <- 
  volcano_work_d %>% 
  bootstraps()
#fit resample will run the method using the workflow defined and resaple using boothstrap instead of test and straining
volcano_res <-
  fit_resamples(
    volcano_wf,
    resamples = volcano_boot,
    control = control_resamples(save_pred = TRUE)
  )

#Here the new results
volcano_res %>% 
  collect_metrics()

volcano_res %>% 
  collect_predictions() %>% 
  conf_mat(volcano_type, .pred_class)

volcano_res %>% 
  collect_predictions() %>% 
  ppv(volcano_type, .pred_class)

#################################
#make a map with the bootstrapped samples of the workflow random forest pred 
volcano_pred <- 
  volcano_res %>% 
  collect_predictions() %>% 
  mutate(correct = volcano_type == .pred_class) %>% 
  left_join(
    volcano_work_d %>% 
      mutate(.row = row_number()))

volcano_pred

ggplot() + geom_map(data = world, map = world,aes( long, lat, map_id = region),
    colour = "white", fill = "gray50", alpha = 0.2 ) +
  stat_summary_hex( data = volcano_pred, aes(Longitude, Latitude, z = as.integer(correct)),
    fun = "mean", alpha = 0.7, bins = 70 ) +
  scale_fill_gradient( high = "green", low = "red", labels = scales::percent ) +
  labs(
    title = "Distribution of Volcanoes and their Prediction Accuracy",
    fill = "Percent classified \ncorrectly")

