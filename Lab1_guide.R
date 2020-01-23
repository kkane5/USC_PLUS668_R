#####################
#### Lab 1 guide ####
#####################
# PLUS668


#### (0) GET SET UP ####

# Set your working directory. It's good practice to do this up front.
# You can do this in the session drop-down menu, but I recommend hard-coding it and running the below line as well. 
setwd("C:/Users/kane/Dropbox/usc_emup/2020_spring/Lab1")   # Kevin's working directory. Modify to match yours.


#### (1) GRAB CENSUS DATA FROM FACTFINDER ####
# Advanced search, Topics -> People -> Basic Count/Estimate -> Population Total.
# Then, Geographics -> Counties -> All counties in the US
# Select B01003, 2017 5-year estimates. 
# Modify table to transpose (so the counties are rows), and download to "Use" the data as a .csv
# Open first in Excel. Delete line 2 (there are 2 lines' worth of header). Resave and close before reading into R.
d = read.csv("ACS_17_1YR_B01003_with_ann.csv")   # make sure the file name matches. Your data will become an object in R called 'd'

# Take a look at your data in a few ways
head(d)
nrow(d)
summary(d)
View(d)

#### (2) DOWLOAD BLS DATA #### 
# Learn more about the data at https://www.bls.gov/cew/downloadable-data-files.htm 
# Detailed documentation is also available at https://www.bls.gov/cew/about-data/documentation-guide.htm  
bls = read.csv("xxxxxx.csv", stringsAsFactors=F)  # replace xxxxx with the name of the right file! 

# Restrict your sample to just the rows representing total employment (excluding the subcategories which are also found in this)
blss = subset(bls, own_code==0)


#### (3) MERGING DATA USING THE MATCH COMMAND ####
# first you need to find the "key" or common identifier in the Census data (it's GEO.id2), and the BLS data (it's area_fips)
# you'll also need to make sure they're the same variable type.  Often, numbers are stored as text (also known as a 'string')
# To coerce area_fips into a numeric value, try the line below to make a new variable:
blss$area_fips_num = as.numeric(as.character(blss$area_fips))
# Then, match.  Here you're making a new variable in the data frame 'd' which is equal to the weekly wage variable in the BLS data... and you're using brackets and the match command to inform R as to what the "key" variables are.
d$avg_wkly_wage = blss$avg_wkly_wage[match(d$GEO.id2, blss$area_fips_num)]
# copy this, and modify it for the other variable you need to merge. 


# One of many ways to get the top ten (there are easier ways, too...)
d[order(d$avg_wkly_wage),][1:10,]

# One way to get the mean WEEKLY wage.  
mean(d$avg_wkly_wage, na.rm=T)

# But, you can also make another variable to convert weeks to years
d$avg_ann_wage = d$avg_wkly_wage*52


#### (4) CORRELATIONS ####
# Fundamentally, how is one variable related to another?
# Here's a very brief overview of the concept: https://www.statisticshowto.datasciencecentral.com/probability-and-statistics/correlation-analysis/
cor.test(d$variable1, d$variable2)  # you'll need to swap out the variables names. The correlation coefficient is the bottom number in the output ('cor')



#### (5) HISTOGRAMS ####
# An example of a basic histogram of population - it doesn't loook great yet... 
hist(d$HD01_VD01)
# Add additional arguments to the hist() command, separated by commas.  Here's the same histogram with a color added:
hist(d$HD01_VD01, col="dodgerblue")
box()   # run this line to put a nice box around your graphic

# Nifty trick - take the 10th-11th characters of the ID variable to get a code for each state!
# They're in alphabetical order, CA is '06', and you can see the full list at https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696
d$state = substr(d$GEO.id,10,11)
ca = subset(d, state=='06')   # a new dataframe of just California counties 



#### READ IN AN FORMAT YOUR HEALTH DATA ####
# This is tricky, but I do want you to dowload this from the source (it's good practice!)
# Unzip the 3 files you've downloaded into your folder, make sure the three .csv files listed below are in your folder and have the same names as below, and run the 18 lines of code below EXACTLY
# You'll have a data frame called 'cdc' with all you need.  Check out the code to see what it's doing! 
demo = read.csv("DEMOGRAPHICS.csv")
risk = read.csv("RISKFACTORSANDACCESSTOCARE.csv")
vuln = read.csv("VUNERABLEPOPSANDENVHEALTH.csv")
demo_vars = c("State_FIPS_Code", "County_FIPS_Code","CHSI_County_Name","CHSI_State_Abbr","Population_Size","Population_Density","Poverty","Age_19_Under","Age_19_64","Age_65_84","Age_85_and_Over")
demo1 = demo[demo_vars]
risk_vars = c("No_Exercise","Smoker","High_Blood_Pres","Obesity","Diabetes","Prim_Care_Phys_Rate","Dentist_Rate")
risk1 = risk[risk_vars]
unemp_cap = vuln$Unemployed/demo$Population_Size   # Make 'per capita' variables using population size. 
disable_cap = vuln$Sev_Work_Disabled/demo$Population_Size
depress_cap = vuln$Major_Depression/demo$Population_Size
drug_cap = vuln$Recent_Drug_Use/demo$Population_Size
uninsured_cap = risk$Uninsured/demo$Population_Size
cdc = cbind(demo1,risk1,unemp_cap,disable_cap,depress_cap,drug_cap,uninsured_cap)
cdc$fips = 0
cdc$len = nchar(cdc$County_FIPS_Code)
cdc$fips[cdc$len==3] <- as.numeric(paste(cdc$State_FIPS_Code, cdc$County_FIPS_Code, sep=""))[cdc$len==3]
cdc$fips[cdc$len==2] <- as.numeric(paste(cdc$State_FIPS_Code, "0", cdc$County_FIPS_Code, sep=""))[cdc$len==2]
cdc$fips[cdc$len==1] <- as.numeric(paste(cdc$State_FIPS_Code, "00", cdc$County_FIPS_Code, sep=""))[cdc$len==1]

# Note that the variable 'fips' is comparable to the county code found in your other datasets so that you can add wage/population data :) 

# The CDC, and many data providers use missing value codes. In this instance, anything negative is actually missing, so change it to R's missing value code NA
cdc[cdc<0] = NA


#### MAKING A SCATTERPLOT #### 
# An easy scatterplot 
plot(cdc$avg_ann_wage, cdc$depress_cap)

# Make a more sophisticated scatterplot 
x = cdc$avg_ann_wage   # giving your variables convenient names can be helpful when making a fancier scatterplot# giving your variables convenient names can be helpful when making a fancier scatterplot
y = cdc$depress_cap
model = lm(y ~ x)   # runs a 2-variable regression and saves the output to an object I've called 'model' - this is needed for the best-fit line
plot(x,y, xlab="Name of variable 1", ylab = "Name of variable 2", 
     main = "Variable 1 vs. Variable 2 Comparison")
abline(model, col="blue", lwd=2)   # abline can add model results (the best fit line!) to the plot
legend("topright", inset=0.045, legend=c("r =", round(cor.test(x, y)$estimate,4)))  # note how we snuck in the correlation test results into the legend


