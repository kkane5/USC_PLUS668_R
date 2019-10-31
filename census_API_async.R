##################################
###### Using the Census API ######
##################################
# by Kevin Kane, PhD
# for use in USC Price's PLUS668: Big Data for Planning and Development

setwd("C:/Users/kane/Dropbox/usc_emup/R_tutorials/api")
options(scipen=999)

#### Install TidyCensus & tigris, and load other packages we'll need today ####
install.packages("tidycensus")
install.packages("tigris")
library(tidycensus)
library(tigris)
library(sf)
library(doBy)

#### Request a key from the Census to access their data #### 
# You'll need to request a Census Key to access their API. 
# This takes only one minute. Then, enter your key below
# http://api.census.gov/data/key_signup.html
census_api_key("5de05eeaa3869c3180eed1726a07e0f27e6c9e20", install=TRUE, overwrite=TRUE)

# Extract your first variable from the decennial census using the function get_decennial
medrent90 = get_decennial(geography = "state", variables = "H043A001", year = 1990)

# Make a barplot
barplot(medrent90$value, names.arg=medrent90$NAME, cex.names=0.7, las=2)

# Make a better barplot
medrent90b = medrent90[order(-medrent90$value),]		# sort descending by making a new data frame
barplot(medrent90b$value, names.arg=medrent90b$NAME, cex.names=0.7, las=2,
        main="State median rent, 1990", col="dodgerblue")   #cex.names makes labels smaller, las=2 rotates them.
abline(h=seq(0,700,100), col="darkgrey", lty=3)
box()


# Grab a blank state-level shapefile at https://www2.census.gov/geo/tiger/TIGER2018/STATE/tl_2018_us_state.zip 
# Unzip and place into your working directory
# Restrict to lower 48 states to keep the map looking good (sorry Alaska, Hawaii, and territories!)
states = read_sf(dsn = ".", layer = "tl_2018_us_state")
states = subset(states, GEOID!="78" & GEOID!="69" & GEOID!="66" & GEOID!="02" & GEOID!="60" & GEOID!="72" & GEOID!="15")

# Plot a basic map of the states 
plot(st_geometry(states), col = "grey")

# Match rent data to the shapefile and plot
states$medrent90 = medrent90$value[match(states$GEOID, medrent90$GEOID)]
plot(states["medrent90"])

# pick a good way to display your data
# with 'breaks', make sure you have zero and a high value too. 
hist(states$medrent90)
plot(states["medrent90"], breaks=c(0, 350, 500, 600, 1000))

# You can explicitly make quintiles, or use Jenks optimization 
states$rent90q = cut(states$medrent90, 5)
plot(states["rent90q"])
plot(states["medrent90"], breaks="jenks", main="1990 Median Rent by state")

# You can also get a county-level variable for just counties in California.
# On your own, you might try and bind this to your CA county data from the previous lab, and map it 
medrentcounties90 <- get_decennial(geography = "county", state = "CA", variables = "H043A001", year = 1990)



#### FINDING GOOD CENSUS VARIABLES TO USE #### 
# Focusing on the most recent ACS 5-year estimates (2017 at the time of this writing) is a good way to go. The 2010 Decennial is getting fairly old.
# https://api.census.gov/data/2017/acs/acs5/profile/variables.html 
# All the Census API available datasets are at https://api.census.gov/data.html 
# You can also extract a list of variables using the code here.
acsvars = load_variables(2017, "acs5", cache = TRUE)
head(acsvars)
nrow(acsvars)   # tells us how many variables
length(unique(unlist(acsvars$concept)))   # tells us how many unique "concepts" there are
write.csv(acsvars, "acsvars.csv")   # export to .csv so you explore easily in Excel 


#### ASSEMBLE A GOOD SET OF TRACT-LEVEL VARIABLES FOR A COUNTY #### 
# Get a single variable (Median home value)
vent = get_acs(geography="tract", state="CA", county="Ventura", variables="B25007_001", 
              year=2017, geometry=TRUE)
vent$medhoval = vent$estimate  # create a renamed to avoid future confusion
vent$estimate = NULL   # get rid of the old one 

# Add additional variables with a single extraction by putting them into a list. 
# Total population: B00101_001
# Median age: B01002_001
# Median household income: B19013_001
# Median age of housing: B25035_001

varlist = c("B01001_001", "B01002_001", "B19013_001", "B25035_001")
out =  get_acs(geography="tract", state="CA", county="Ventura", variables=varlist, year=2017)

# Use a match command to bind each new variable to the original data frame (vent). name the variables something intuitive.
# Since the data are stored long, each match operation requires a subset operation first.
venta = subset(out, variable=="B01001_001")
vent$totpop = venta$estimate[match(vent$GEOID, venta$GEOID)]

ventb = subset(out, variable=="B01002_001")
vent$medage = ventb$estimate[match(vent$GEOID, ventb$GEOID)]

ventc = subset(out, variable=="B19013_001")
vent$medhhinc = ventc$estimate[match(vent$GEOID, ventc$GEOID)]

ventd = subset(out, variable=="B25035_001")
vent$medhomeage = ventd$estimate[match(vent$GEOID, ventd$GEOID)]

# Make some more variables using a loop. 
# Total commuters: B08101_001
# Single-occupant vehicle commuters: B08101_009
# Work at home: B08101_001

varlist2 = c("B08101_001", "B08101_009", "B08101_049")
varnames2 = c("totcommute", "SOVcommute", "workathome")
out2 =  get_acs(geography="tract", state="CA", county="Ventura", 
                variables=varlist2, year=2017)

# Try looping through so as to avoid having to repeat for each variable.
# The loop command iterates through the sequence of numbers from 1 to 3 using "i" as the wildcard
# The first line subsets the extraction to just the ith item in varlist2
# The second line adds a new column to vent (one column higher than its current number of columns...). This structure avoids having to declare a specific variable name and allows you to loop through variables.
# The third line renames the last column to the ith element in varnames2, which you made above.  
for(i in 1:3){
  join = subset(out2, variable==varlist2[i])
  vent[,ncol(vent)+1] = join$estimate[match(vent$GEOID, join$GEOID)]
  colnames(vent)[ncol(vent)] = varnames2[i]  
}

# Many census variables have a restricted universe, e.g. those over age 16 are isolated to gauge labor force participation, or the variable just otherwise needs to be a percentage.
vent$sovshare = vent$SOVcommute/vent$totcommute
vent$wahshare = vent$workathome/vent$totcommute


#### AUTOMATICALLY EXTRACT SHAPEFILES FROM CENSUS #### 
# We will use the tigris package to extract Census shapefiles directly. 
# See details at http://rstudio-pubs-static.s3.amazonaws.com/90665_de25062951e540e7b732f21de53001f0.html 

# The function is the name of the geography you want - tracts, in this case. 
# Let's grab all of southern California, then just Ventura county to continue from above 
scagtr = tracts(state = 'CA', county = c('Imperial', 'Los Angeles', 'Orange', 'Riverside', 'San Bernardino', 'Ventura'))
plot(scagtr)
venttr = tracts(state = 'CA', county = 'Ventura')
plot(venttr)


##!?!?!?## RESET FOR A SECOND ##!?!?!?##
# We can also read in the data ... as a shapefile! 
# Previously we'd run the below.  BUT, if you add geometry=TRUE at the end, the upload will include spatial reference data.
ventnew = get_acs(geography="tract", state="CA", county="Ventura", variables="B25007_001", 
               year=2017, geometry=TRUE)
plot(ventnew['estimate'])

# Now, go back to the section titled 'assembling a good set of tract-level...' and 
# add 'geometry=TRUE' to the end of the get_acs command.
# Re-run everything through the end of that section

# Try some plotting
plot(vent['sovshare'])

# If the island is messing up your view, remove that tract, and plot something else.
vent_noisland = subset(vent, GEOID!="06111980000")
plot(vent_noisland['medhomeage'], main="Median home ages across Ventura County, CA")


# Finally, we can export this to a shapefile so we can use it elsewhere, too.
st_write(vent, "vent_merge.shp")





