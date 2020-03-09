#####################
#### Lab 3 guide ####
#####################
# PLUS668


#### (0) GET SET UP ####

# Set your working directory.  This is less important for Lab 3 because your input data will come from an API.  But, you will need to write data to your computer later in the lab, so it's good practice to do this up front.
setwd("C:/users/kevin/Dropbox/usc_emup/2020_spring/Lab3")   # Kevin's working directory. Modify to match yours.


# Read in packages.  This needs to be done each time you open RStudio.  You'll have had to install the packages for this to work (see Async/tutorial).
library(tidycensus)
library(tigris)
library(sf)
library(doBy)
options(scipen=999)  # run this only if you want to turn scientific notation OFF for the whole time you're using RStudio

# Read in your Census API key.  Put it inside the empty quotes are below:
# Sign up at http://api.census.gov/data/key_signup.html
census_api_key("5de05eeaa3869c3180eed1726a07e0f27e6c9e20", install=TRUE, overwrite=TRUE)
# Depending on your computer, some red text may appear indicating you need to run another line.  Do this if prompted.


#### (1) EXTRACT AND MANIPULATE CENSUS VARIABLES ####

# Try to get a single variable. Use your "useful_census_vars_KKguide," or the API guide (see Async/tutorial) to find the variable codes. 
# The command is get_acs.  You'll need to specify a geography, variables, and year. 
# Notice how this is a data frame (I've called it 'states'), and the population is actually in the column called "estimate"
states = get_acs(geography="state", variables="B01001_001", year=2017)
head(states)
nrow(states)
states$estimate

# Next, you'll want to assemble a 52-row data frame of the several variables in your Lab. 
# "estimate" is not a good column name, as each data extraction will have that name.  Rename the variables something more intuitive (I've chosen 'totpop')
# You can start with your first data extraction above, then use the match command to join more fields.
# The below code extracts the universe for race/ethnicity and the white, non-Hispanic population (I used wnh as the abbreviation), and appends to statepop.  Then, this is done for median age.

states$totpop = states$estimate
states2 = get_acs(geography="state", variables="B03002_003", year=2017)  # extract white, non-Hisp.
states$wnh = states2$estimate[match(states$GEOID, states2$GEOID)]  # match white, non-Hisp.
states3 = get_acs(geography="state", variables="B01002_001", year=2017)  # extract median age
states$medage = states3$estimate[match(states$GEOID, states3$GEOID)]  # match median age
summary(states)

# Make percentage variables as needed (these can be more informative - especially if states vary widely in size).  You'll need to make "percent white, non-Hispanic" here.
# You'll also need to add up 4 educational categories and divide by total (>25 years old) population to get "percent B.A. and above."
# Eduation is not shown here.  But, it requires (i) five get_acs commands, (ii) five match commands, (iii) making a new variable which adds the four > B.A. variables, and (iv) make "percent B.A. and above" variable.
states$wnh_pct = states$wnh/states$totpop
summary(states$wnh_pct)

# Make nice-looking barplots. You can make a new data frame - let's call it s - with the states IN DESCENDING ORDER to make the barplot look nicer
# A histogram shows the range of values of a variable.
# A barplot is different in that it generates one bar for each observation (in this case, each state)
# The barplot command takes a column/vector as its first input.  You can then tell it to extract a column which has state names (names.arg), scale them to a proportion of their size so they'll fit (cex.names), and change their orientation so they read vertically (las=2)
s = states[order(-states$totpop),]  # tip - make sure to do this (i.e. reorder) for each variable separately. 
head(s)   # just to make sure the order is right.
barplot(s$totpop, names.arg=s$NAME, cex.names=0.7, las=2, main="Population by state", col="dodgerblue")

# Make a scatterplot with a best-fit line and legend.  The easy way is:
plot(states$totpop, states$wnh_pct)
# But this does not effectively use data to make a point!  I suggest renaming the variables x and y, per below, then running the subsequent several lines of code (through the legend command) to get a nice scatterplot.
# Then, you can reuse the code by replacing x= and y= in the two lines below. Make sure to update the xlab, ylab, and main arguments in the plot command, and change the position of the legend (it's "topright" now) if needed.
# Don't forget to write a description of each relationship.  For a quick refresher on correlation and best-fit lines, see https://www.dummies.com/education/math/statistics/how-to-interpret-a-correlation-coefficient-r/
x = states$totpop
y = states$wnh_pct
model = lm(y ~ x)   
plot(x, y, xlab="Total Population", ylab = "White, non-Hispanic share", 
     main = "Comparison of state size and White, non-Hispanic population (2017)", pch=20, font.main=4) 
abline(model, col="blue", lwd=2)
rp = vector('expression', 3)
rp[1] = substitute(expression(italic(B)[1] == MYVALUE3), list(MYVALUE3=format(summary(model)$coefficients[2], dig=4)))[2]
rp[2] = substitute(expression(italic(p) == MYVALUE2), list(MYVALUE2=format(summary(model)$coefficients[2,4], dig=3)))[2]
rp[3] = substitute(expression(italic(r) == MYVALUE), list(MYVALUE=format(sqrt(summary(model)$r.squared), dig=4)))[2]
legend("topright", legend=rp, bty='n')
# For an added touch, add state names! Uses the text command; the labels= argument points to the state name field; cex/pos/font are all graphical parameters - type ?par for help. 
text(x, y, labels=states$NAME, cex=0.6, pos=2, font=3)



#### (2) COUNTY-LEVEL: EXTRACT AND MANIPULATE CENSUS VARIABLES ####

# Extract a single variable to start with.  
# To only get counties in California, for example (for future reference-not part of Lab), see the second line here.
counties = get_acs(geography="county", variables="B01001_001", year=2017)
CAcounties = get_acs(geography="county", variables="B01001_001", state="CA", year=2017)

# Example of extracting a second variable and putting it into the first data frame.  Repeat for all needed variables. 
counties$totpop = counties$estimate
c2 = get_acs(geography="county", variables="B01002_001", year=2017)
counties$median_age = c2$estimate[match(counties$GEOID, c2$GEOID)]

# Put multiple histograms into the same image.  This mini-guide only has 2 variables, so I'll repeat them each twice, since the lab asks for 4.
# Before running hist, run par to change the number of rows and columns in your histogram (example has 2 rows x 2 columns)
# Then, just run hist four times - once for each variable.  Add colors, breaks, labels, etc. as you see fit. 
# ONCE YOU'RE DONE - sometimes RStudio gets 'stuck' with these 4-part graphics.  Rerun the par command with 1,1 instead of 2,2 to return to normal.
par(mfrow=c(2,2))
hist(counties$totpop)
hist(counties$median_age)
hist(counties$totpop, breaks=20, xlab="Total Pop")
hist(counties$median_age, breaks=20, xlab="Median Age by County", col="chartreuse", border="blue")

# For a county scatterplot, reuse the state code above, but replace 'state' with 'county.' 


#### (3) CENSUS TRACT-LEVEL AND WITH GEOMETRY: EXTRACT AND MANIPULATE CENSUS VARIABLES ####
# Follow the same general procedure.  Just make sure to specify a state, and if you're analyzing a county, specify a state AND a county, in the get_acs command
tr = get_acs(geography="tract", state="CA", county="Sonoma", variables="B01001_001", year=2017, geometry=TRUE)
tr$totpop = tr$estimate

# Extract the other varaibles you need (GEOMETRY is NOT needed for these...) and use match to append it to what you just extracted.  Repeat for all needed variables. 
tr2 = get_acs(geography="tract", state="CA", county="Sonoma", variables="B01002_001", year=2017)
tr$median_age = tr2$estimate[match(tr$GEOID, tr2$GEOID)]

# You can plot a variable in R using the syntax below.  The second line adds a title:
plot(tr["totpop"])
plot(tr["median_age"], main="Median age in Sonoma county census tracts (2017)")


#### (4) EXPORT AND ANALYZE IN GEODA #### 
# Finally, we can export this to a shapefile so we can use it elsewhere, too.
# This output will be written to whatever your working directory is.  If you're unsure, type getwd(). 
st_write(tr, "Sonoma_tracts.shp")

# Troubleshooting - if you are unable to make spatial weights in GeoDa due to "NULL geometry," there is a row in your data frame which does not have an actual polygon associated with it.
# To fix this, open the data using View(tr), and identify which row(s) is missing a bunch of coordinates in the 'geometry' field. In this instance, it's row #100
# Then, run the below code to remove this row (do multiple times if there is more than one problematic row).  Then, write to a shapefile again. 
tr = tr[-100,]








