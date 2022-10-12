library(readr)
library(haven)
library(ggplot2)
library(dplyr)
library(car)
library(lme4)
library(arm)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(tibble)
library(dataverse)
library(stringr)
library(purrr)
library(tidycensus)
library(tidyr)
library(usmap)
library(scales)
library(ggrepel)

#Install tidycensus from Walkerke
#remotes::install_github("walkerke/tidycensus")
library(tidycensus)
#load census api key
#census_api_key("API KEY", install = TRUE)


#GET BLW DATA ################################################
#https://brightlinewatch.org/survey-data-and-replication-material/
BLWwave13 <- read_csv("http://brightlinewatch.org/wp-content/uploads/2021/05/Public_Wave13.csv")
BLWwave13.trim <- BLWwave13[, c("caseid", "weight", "vote2020", "biden_winner", "pres_vote", "birthyr", "age", "gender", "employ", "faminc_new", "pid3", "pid7", "inputstate", "hispanic", "ethnicity", "votereg", "presvote16post", "inputzip", "educ7", "wave")]
BLWwave14 <- read_csv("http://brightlinewatch.org/wp-content/uploads/2021/02/Public_Wave14.csv")
BLWwave14.trim <- BLWwave14[, c("caseid", "weight", "vote2020", "biden_winner", "pres_vote", "birthyr", "age", "gender", "employ", "faminc_new", "pid3", "pid7", "inputstate", "hispanic", "ethnicity", "votereg", "presvote16post", "inputzip", "educ7", "wave")]
data <- rbind(BLWwave13.trim, BLWwave14.trim)
dat <- data

#Recode rejection of election results ot binary: 0 = accept, 1 = reject
dat$biden_winner[dat$biden_winner=="Definitely not the rightful winner"] <- "1"
dat$biden_winner[dat$biden_winner=="Probably not the rightful winner"] <- "1"
dat$biden_winner[dat$biden_winner=="Probably the rightful winner"] <- "0"
dat$biden_winner[dat$biden_winner=="Definitely the rightful winner"] <- "0"
dat$biden_winner <- as.numeric(dat$biden_winner)
table(dat$biden_winner)

#Age
dat$over45 <- NA
dat$over45 <- 0
dat$over45[dat$age > 44] <- 1

#Black
dat$black <- NA
dat$black <- 0
dat$black[dat$ethnicity=="Black or African American"] <- 1
table(dat$black)

#Hispanic
dat$hispanx <- NA
dat$hispanx <- 0
dat$hispanx[dat$hispanic=="Yes"] <- 1
dat$hispanx[dat$ethnicity=="Hispanic/Latino/Chicano/a"] <- 1
table(dat$hispanx)

#Education
dat$hsmax <- NA
dat$hsmax <- 0
dat$hsmax[dat$educ7=="Did not graduate from high school"] <- 1
dat$hsmax[dat$educ7=="High school diploma or the equivalent (GED)"] <- 1
table(dat$hsmax)
dat$white <- NA
dat$white <- "No"
dat$white[dat$ethnicity=="White" & dat$hispanic != "Yes"] <- "Yes"
dat$no_college <- NA
dat$no_college <- "No"
dat$no_college[dat$educ7=="Did not graduate from high school"] <- "Yes"
dat$no_college[dat$educ7=="High school diploma or the equivalent (GED)"] <- "Yes"
table(dat$no_college)
dat$whiteNC <- NA
dat$whiteNC <- 0
dat$whiteNC[dat$white=="Yes" & dat$no_college=="Yes"] <- 1

#Income (over $100k)
table(dat$faminc_new)
dat$over100k <- NA
dat$over100k <- 0
dat$over100k[dat$faminc_new=="$100,000 - $119,999" | dat$faminc_new=="$120,000 - $149,999" | dat$faminc_new=="$150,000 - $199,999" | dat$faminc_new=="$200,000 - $249,999" | dat$faminc_new=="$250,000 - $349,999" | dat$faminc_new=="$350,000 - $499,999" | dat$faminc_new=="$500,000 or more"] <- 1

#Labor force participation
table(dat$employ)
dat$not_in_labor_force <- NA
dat$not_in_labor_force <- 1
dat$not_in_labor_force[dat$employ=="Full-time" | dat$employ=="Unemployed" | dat$employ=="Part-time" | dat$employ=="Temporarily laid off"] <- 0
table(dat$not_in_labor_force)

#Education
table(dat$educ7)

#Vote
#Vote for Trump
dat$trump <- NA
dat$trump <- 0
dat$trump[dat$pres_vote=="Donald Trump"] <- 1

#Did not vote
table(dat$vote2020)
dat$no_vote <- NA
dat$no_vote <- 0
dat$no_vote[dat$vote2020 !="I am sure I voted"] <- 1
table(dat$no_vote)

#Change input_states to NAME to match tidycensus
dat$State <- dat$inputstate
dat$STATE <- tolower(dat$State)
dat <- subset(dat, STATE != "district of columbia")



#MAKE MODEL ################################################
#Model Used for prediction
Model <- lm(biden_winner ~ hsmax + black + hispanx + not_in_labor_force + over100k + trump + no_vote + over45, data=dat)
Model
summary(Model)

#Inverse Logit
state_model <- glmer(formula = biden_winner ~  (1 | STATE) + hsmax + black + hispanx + not_in_labor_force + over100k + trump + no_vote + over45, data=dat, family=binomial(link="logit"))
state_model
summary(state_model)



#GET CENSUS DATA ################################################
#Create string of U.S. States
head(tidycensus::fips_codes)
us <- unique(fips_codes$state)[1:51]

#Create state targets (all)
my_states <- fips_codes %>%
  filter(state %in% us)


#Get AgeXGender data 
age_vars <- c(
  over45 = "B29001_004",
  over65 = "B29001_005"
)

age_dat <- map_dfr(
  my_states,
  ~ get_acs(
    geography = "state",
    variables = age_vars,
    state = us,
    summary_var = "B29001_001" #total citizen voting age pop
  )
)

age_dat = unique(age_dat)

agedat <- age_dat %>%
  group_by(NAME) %>%
  summarize(over45N = sum(estimate, na.rm=TRUE),
            total = summary_est)

age.dat = unique(agedat)
age.dat$over45 = age.dat$over45N/age.dat$total

age_data = age.dat[, c("NAME", "over45")]


#Get HHI data
hhi_vars <- c(
  less100k = "B19001_013", #100k
  less125k = "B19001_014", #125k
  less150k = "B19001_015", # 150k
  less200k = "B19001_016", #200k
  over200k = "B19001_017" # +200k+
)

hhi_dat <- map_dfr(
  my_states,
  ~ get_acs(
    geography = "state",
    variables = hhi_vars,
    state = us,
    summary_var = "B19001_001" 
  )
)

hhi_dat = unique(hhi_dat)
hhidat <- hhi_dat %>%
  group_by(NAME) %>%
  summarize(over100kN = sum(estimate, na.rm=TRUE),
            total = summary_est)
hhi.dat = unique(hhidat)
hhi.dat$over100k = hhi.dat$over100kN/hhi.dat$total
hhi.data = hhi.dat[, c("NAME", "over100k")]


#Get Labor Force data 
lab_dat <- map_dfr(
  my_states,
  ~ get_acs(
    geography = "state",
    variables = "B23025_007", #not in the labor force
    state = us,
    summary_var = "B23025_001"
  )
)

lab_dat$not_in_labor_force = lab_dat$estimate/lab_dat$summary_est
labor.data = lab_dat[c(1:51), c("GEOID", "NAME", "not_in_labor_force")]
head(labor.data)


#Get white no college data
edu_vars_gen <- c(
  Mnone = "B15002_003",
  M4 = "B15002_004",
  M6 = "B15002_005",
  M8 = "B15002_006",
  M9 = "B15002_007",
  M10 = "B15002_008",
  M11 = "B15002_009",
  M12 = "B15002_010",
  MHS = "B15002_011",
  MC1 = "B15002_012",
  MC2 = "B15002_013",
  MAss = "B15002_014",
  MBachelors = "B15002_015",
  MMasters = "B15002_016",
  MProf = "B15002_017",
  MDr = "B15002_018",
  Fnone = "B15002_020",
  F4 = "B15002_021",
  F6 = "B15002_022",
  F8 = "B15002_023",
  F9 = "B15002_024",
  F10 = "B15002_025",
  F11 = "B15002_026",
  F12 = "B15002_027",
  FHS = "B15002_028",
  FC1 = "B15002_029",
  FC2 = "B15002_030",
  FAss = "B15002_031",
  FBachelors = "B15002_032",
  FMasters = "B15002_033",
  FProf = "B15002_034",
  FDr = "B15002_035"
)

edu_dat <- map_dfr(
  my_states,
  ~ get_acs(
    geography = "state",
    variables = edu_vars_gen, 
    state = us,
  )
)

edudat = edu_dat[, c(2:4)]
edudat = unique(edudat)
edu.dat <- edudat %>%
  pivot_wider(names_from = variable, values_from = estimate)

edu_dat <- edu.dat

edu_dat$hsmax = 0
edu_dat$hsmax = edu_dat$Fnone +  edu_dat$F4 + edu_dat$F6 + edu_dat$F8 + edu_dat$F9 + edu_dat$F10 + edu_dat$F11 + edu_dat$F12 + edu_dat$FHS + edu_dat$Mnone +  edu_dat$M4 + edu_dat$M6 + edu_dat$M8 + edu_dat$M9 + edu_dat$M10 + edu_dat$M11 + edu_dat$M12 + edu_dat$MHS

edu_dat$base = edu_dat$FC1 + edu_dat$FC2 + edu_dat$FAss + edu_dat$FBachelors + edu_dat$FProf + edu_dat$FDr + edu_dat$FMasters + edu_dat$MC1 + edu_dat$MC2 + edu_dat$MAss + edu_dat$MBachelors + edu_dat$MProf + edu_dat$MDr + edu_dat$MMasters + edu_dat$hsmax

edu_dat$highschool_only = edu_dat$hsmax/edu_dat$base

edu_data = edu_dat[, c("NAME", "highschool_only")]


#Get black and Hispanic data
race_vars <- c(
  blackman18 = "B05003B_008",
  blackman18_noncit = "B05003B_012",
  blackwoman18 = "B05003B_019",
  blackwoman18_noncit = "B05003B_023",
  hispman18 = "B05003I_008",
  hispman18_noncit = "B05003I_012",
  hispwoman18 = "B05003I_019",
  hispwoman18_noncit = "B05003I_023",
  M18 = "B05003_008", #Male 18 + 
  M18noncit = "B05003_012", #Male non-citizen 18+
  F18 = "B05003_019", #Female 18 + 
  F18noncit = "B05003_023"# Female non-citizen 18+
)

race_dat <- map_dfr(
  my_states,
  ~ get_acs(
    geography = "state",
    variables = race_vars, 
    state = us
  )
)
head(race_dat)
race.dat = race_dat[, c(2:4)]
head(race.dat)
race.dat = unique(race.dat)
racedat <- race.dat %>%
  pivot_wider(names_from = variable, values_from = estimate)
head(racedat)

racedat$blackN = racedat$blackman18 + racedat$blackwoman18 - racedat$blackman18_noncit - racedat$blackwoman18_noncit

racedat$hispanicN = racedat$hispman18 + racedat$hispwoman18 - racedat$hispman18_noncit - racedat$hispwoman18_noncit

racedat$totalVAPcitizens = racedat$M18 + racedat$F18 - racedat$M18noncit - racedat$F18noncit

racedat$black = racedat$blackN/racedat$totalVAPcitizens
racedat$hispanic = racedat$hispanicN/racedat$totalVAPcitizens
race_data = racedat[, c("NAME", "black", "hispanic")]
totalvap = racedat [, c("NAME", "totalVAPcitizens")]


#dataframes to merge 
#race_data, white_no_college_data, hhi.data, age_data, labor.data
merge1 = merge(labor.data, age_data, by="NAME")
merge2 = merge(merge1, hhi.data, by="NAME")
merge3 = merge(merge2, edu_data, by="NAME")
merge4 = merge(merge3, race_data, by="NAME")
merge5 = merge(merge4, totalvap, by="NAME")

Census <-  merge5


#GET ELECTION DATA ################################################
library(dataverse)
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")


election_dat.tab <- get_dataframe_by_name(
  filename = "1976-2020-president.tab",
  dataset = "10.7910/DVN/42MVDX",
  server = "dataverse.harvard.edu")


results2020 <- subset(election_dat.tab, year==2020)
trump.results <- subset(results2020, candidate=="TRUMP, DONALD J.")

trump.results$trumpN <- trump.results$candidatevotes
trump.results$trumpP <- trump.results$trumpN/trump.results$totalvotes
trump.results$NAME <- str_to_title(trump.results$state)

election_data <- trump.results[, c("NAME", "trumpN", "trumpP", "totalvotes")]

Census1 = merge(Census, election_data, by="NAME")
Census <- Census1

#MAKE PREDICTIONS ########################################
#Create state random effects for MrP predictions
state_ranefs <- array(NA, c(50, 1))
Census$STATE <- Census$NAME
dimnames(state_ranefs) <- list(c(Census$STATE), 'effect')
# assign state random effects to array while preserving NAs
for (i in Census$STATE) {
  
  state_ranefs[i, ] <- ranef(state_model)$STATE[i, 1]
  
}
state_ranefs[, 1][is.na(state_ranefs[, 1])] <- 0

#make a no_voted variable for reciprocal turnout 
Census$no_voted <- NA
Census$no_voted = Census$totalvotes/Census$totalVAPcitizens
Census$no_vote = 1 - Census$no_voted
Census$trump <- Census$trumpN/Census$totalVAPcitizens

Census$prediction <- invlogit(fixef(state_model)['(Intercept)']  + state_ranefs[Census$STATE, 1] + (fixef(state_model)['hsmax'] *Census$highschool_only)  + (fixef(state_model)['black'] *Census$black) + (fixef(state_model)['hispanx'] *Census$hispanic) + (fixef(state_model)['not_in_labor_force'] *Census$not_in_labor_force) + (fixef(state_model)['over100k'] *Census$over100k) + (fixef(state_model)['over45'] *Census$over45) + (fixef(state_model)['trump'] *Census$trumpP) + (fixef(state_model)['no_vote'] *Census$no_vote))
state_model
summary(Census$prediction)

cor.test(Census$prediction, Census$trump) #check for total VAP's correlation with total_pop vote for trump



#MAKE MAP ################################################

library(albersusa)
library(sf)
library(ggplot2)
library(sf)
library(albersusa)
Census$prediction
Census$predictionP = Census$prediction*100
Census$PredictionP = round(Census$prediction, digits = 2)


usa_sf <-
  st_as_sf(usa_composite("laea")) %>%
  mutate(
    CENTROID = map(geometry, st_centroid),
    COORDS = map(CENTROID, st_coordinates),
    COORDS_X = map_dbl(COORDS, 1),
    COORDS_Y = map_dbl(COORDS, 2)
  ) %>%
  as_tibble() %>%
  st_as_sf()

usa_sf$nudge_x <- 0
usa_sf$nudge_y <- 0



x_range <- abs(Reduce("-", range(usa_sf$COORDS_X)))
y_range <- abs(Reduce("-", range(usa_sf$COORDS_Y)))

ix <- usa_sf$name %in% c("New Hampshire", "Vermont", "Massachusetts")
usa_sf$nudge_x[ix] <- -1 * 0.15 * x_range
usa_sf$nudge_y[ix] <- 1 * 0.15 * y_range

ix <- usa_sf$name %in% c(
  "Massachusetts",
  "Rhode Island", "Connecticut", "New Jersey",
  "Maryland", "Delaware", "New Hampshire"
)
usa_sf$nudge_x[ix] <- 1 * 0.2 * x_range
usa_sf$nudge_y[ix] <- -1 * 0.15 * y_range
head(usa_sf)




usa_sf2 <- subset(usa_sf, name != "District of Columbia")
Census$name <- Census$STATE
data_to_map <- merge(usa_sf2, Census, by="name")

data_to_map$predictionP <- round(data_to_map$predictionP, digits = 0)


data_to_map$predicted_value <- as.character(data_to_map$predictionP)
data_to_map$predicted_value
data_to_map$predicted_value <- paste(data_to_map$predicted_value, "%", sep="")

data_to_map$predicted_value[data_to_map$name=="Massachusetts"] <- "MA: 23%"
data_to_map$predicted_value[data_to_map$name=="Connecticut"] <- "CT: 31%"
data_to_map$predicted_value[data_to_map$name=="New Jersey"] <- "NJ: 30%"
data_to_map$predicted_value[data_to_map$name=="Maryland"] <- "MD: 21%"
data_to_map$predicted_value[data_to_map$name=="Delaware"] <- "DE: 30%"
data_to_map$predicted_value[data_to_map$name=="New Hampshire"] <- "NH: 35%"
data_to_map$predicted_value[data_to_map$name=="Rhode Island"] <- "RI: 34%"
data_to_map$predicted_value[data_to_map$name=="Hawaii"] <- "Hawaii: 34%"
data_to_map$predicted_value[data_to_map$name=="Vermont"] <- "VT: 22%"



data_to_map$predicted_value
data_to_map$nudge_x[data_to_map$name=="Michigan"] <- 45000
data_to_map$nudge_y[data_to_map$name=="Michigan"] <- 45000*(-1)
data_to_map$name

data_to_map$nudge_y[data_to_map$name=="Louisiana"] <- 45000*(-1)
data_to_map$nudge_x[data_to_map$name=="Florida"] <- 90000

data_to_map$nudge_x[data_to_map$name=="Hawaii"] <- 200000*(-1)
data_to_map$nudge_y[data_to_map$name=="Hawaii"] <- 200000*(-1)

table(data_to_map$prediction)
data_to_map$prediction_percent <- 100*data_to_map$prediction
class(data_to_map$COORDS_X)
data_to_map$predicted_value
ggplot(data = data_to_map) +
  geom_sf(aes(fill = prediction_percent)) +
  geom_text_repel(
    mapping = aes(
      x = COORDS_X,
      y = COORDS_Y,
      label = predicted_value
    ),
    nudge_x = data_to_map$nudge_x,
    nudge_y = data_to_map$nudge_y,
    size = 3,
    min.segment.length = 1,
    point.size = NA,
    segment.color = "grey50"
  ) +
  coord_sf(crs = st_crs(data_to_map), datum = NA) +
  theme_void() +
  xlim(min(usa_sf$COORDS_X) * 1.12, max(usa_sf$COORDS_X) * 1.11) + 
  ylim(min(usa_sf$COORDS_Y) * 1.1, max(usa_sf$COORDS_Y) * 1.2) + 
  labs(title = "Predicted percentage of eligible voters who doubt Biden was the rightful winner of the 2020 election", caption = "Error: ±3%.
  Figure by Brendan Hartnett and Brian Schaffner.
       Values predicted derive from multilevel regression and post-stratification model.
       Survey data: Bright Line Watch, 2020, Bright Line Watch Public Survey Wave 13.
       Bright Line Watch, 2021, Bright Line Watch Public Survey Wave 14.
       
       ") +  
  scale_fill_continuous(limits = c(20,75), breaks = c(30, 50, 70), labels = c("30%", "50%", "70%"), low = "white", high = "brown4", name = " ") + theme(legend.position = "right")