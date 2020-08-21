#Setting up R

setwd("C:/Users/Kelly/Documents/ELPC/Soil Health Index")  #setwd (set working directory) tells R where to look for files and to save. You copy the path from the file explorer - for some reason on windows you have to change the forward slashes to backslashes 

IA_18_19 <- read.csv("iowa_18_19_land_value_yield.csv")  #nominal land value (average $/acre for the county). read.csv imports your file and then to the left of the arrow is how you want to save your file in RStudio

#This code sets up R for your working session - these are all of the different packages. To use a package for the first time you have to use install.packages("name")
library("tidyverse") #basic package for doing pipes and stats stuff
library("ggplot2")    #graphing

#these packages are for Rmarkdown so don't worry about them - it is for writing R code with a pdf output. 
library("stargazer")    
library("kableExtra")    
library("knitr")    

#these packages allow us to do the standard clustered error and give it a nice output 
library("plm")    
library("broom")    
library("lmtest")    

# Helpful for R = if you are unsure about a command you can always type in ?"name" for example:
# ?t.test and all of the options for different calls for the command will come up in the help screen

# Check the Validity of our Data. 

# this code below is thanks to tidyverse. whenever you see the <- I am saving the dataframe as a new dataframe to maintain the original data in case there is some type of error.
# This is called a pipe %>% and allows you to write one or multiple commands. So when there is a df and then a pipe, the command after the pipe is how the data is being manipulated.
# mutate creates a new column in the df
# predicted yield on the left is the name of the new column
# to the right is the command that you want to run for each row

data_quality_check <- IA_18_19 %>% 
  mutate(predicted_yield = ((1.6*CSR2)+80))

# print command shows your results in the console - instead of print you could also save your results using the arrow but this format is easier
# for t test you have the two groups you are comparing - before the $ is the df and after the $ is the column from that df
print(t.test(data_quality_check$yield, data_quality_check$predicted_yield)) 


Good news, the predicted yield and the yield data from USDA are not significantly different. We will use this as a sign that our data is decent quality.    

# Let's get to the good stuff and do some regressions  

#Since we are using panel data (repeated observations over a period of time), we need to use clustered standard errors. If we do not use clustered standard errors, the software will interpret each observation as completely unique, which is incorrect because we are analyzing the same counties over time. More about CSE:  https://blog.theleapjournal.org/2016/06/sophisticated-clustered-standard-errors.html 

#First model is to see relationship between yield and land value. 
#this runs a linear reg model (lm). you put the dependent var first then list your covariates after the ~ 
reg1 <- lm(LAND_VALUE ~ yield, data = IA_18_19) 

#the code below creates a regression table for pdf - dont worry about this. 
#you can click your regression in the global environment to view it or do (print(reg1))
stargazer(reg1, type = "latex", title = "Relationship between yield and land value", 
          header = FALSE, table.placement = "H") #this creates reg table

#cluster standard error to check against normal OLS SE
#use plm instead of lm - more commands
pooled_1 <- plm(LAND_VALUE ~ yield, data = IA_18_19,
                model = "pooling", #there are other model options, fixed effects for example
                index= "County", #this is what you want to group by 
                effect = "individual") 

#this creates your dataframe with the correct standard errors. 
#all you really need to change about this code is the name of your regression (pooled_1)
#I always just copy and paste this code so I can't really break this down more
pooled_1_SE <- as.data.frame(tidy(coeftest(pooled_1, 
                                           vcov=vcovHC(pooled_1, 
                                                       type = "HC0", cluster = "group"))))
#pdf output code
stargazer(t(pooled_1_SE), type = "latex", 
          title = "Relationship between yield and land value, Pooled OLS, Correct SE", 
          header = FALSE, table.placement = "H")

#this code is base R for graphing, you have you x var and y var
# xlab and ylab are the label names, main is the plot titles. naming things and characters in R usually need to be surrounded by "" 
plot(x = IA_18_19$yield, y = IA_18_19$LAND_VALUE, 
     xlab = "Yield (bu/acre)", 
     ylab = "Land Value ($/acre)", main = "Yield against land value (OLS)") 

#this creates a trendline on the plot we just created from the regression model ran earlier
abline(reg1, col = "blue") 

#Interpretation of this model: as yield increases by one bushel per acre, the land value increases by $37.48 per acre.   

#code same as above
reg2 <- lm(LAND_VALUE ~ CSR2, data = IA_18_19)
stargazer(reg2, type = "latex", title = "Relationship between CSR2 and land value", 
          header = FALSE, table.placement = "H")
plot(x = IA_18_19$CSR2, y = IA_18_19$LAND_VALUE, xlab = "CSR2", 
     ylab = "Land Value ($/acre)", main = "CSR2 against Land Value (OLS)")
abline(reg2, col = "blue") 

#the only way this differs is by adding two covariates in the regression model - which you just do by adding a + sign
reg3 <- lm(LAND_VALUE ~ yield + CSR2, data = IA_18_19)
stargazer(reg3, type = "latex", title = 
            "Relationship between yield and CSR2 on land value, OLS", 
          header = FALSE, table.placement = "H")


pooled_2 <- plm(LAND_VALUE ~ yield + CSR2, data = IA_18_19,
                model = "pooling", 
                index= c("County"),
                effect = "individual")

pooled_2_SE <- as.data.frame(tidy(coeftest(pooled_2, 
                                           vcov=vcovHC(pooled_2, 
                                                       type = "HC0", cluster = "group"))))

stargazer(t(pooled_2_SE), type = "latex", title = 
            "Relationship between yield, CSR2 and land value, Pooled OLS Model, Correct SE", 
          header = FALSE, table.placement = "H")
 
#  Let's check how well our model is at predicting land value for each observation. 

#create new column using the equation for each observation, made a new column called "estimate" and then it will plug in each row of observations into our model eqn
check_df <- IA_18_19 %>%
  mutate(estimate = -2824.537 + 22.578*yield + 85.662*CSR2) 

# run and view our t test
print(t.test(check_df$LAND_VALUE, check_df$estimate, paired = FALSE))  




# Add more observations and see if model improves!  

IA_50_15 <- read.csv("iowa_50_15_land_value_yield.csv") 
#the land value has been adjusted for inflation using 2015 as baseline 

IA_16_17 <- read.csv("iowa_16_17_land_value_yield.csv") #nominal land value

IA_complete <- rbind(IA_50_15, IA_16_17, IA_18_19) #rbind connects rows together - so I am combining all of my dataframes into 1

plot(x = IA_complete$yield, y = IA_complete$LAND_VALUE, xlab = "Yield (bu/acre)", 
     ylab = "Land Value ($/acre)", main = "Yield against Land Value") 
#this code is base R for graphing
abline(lm(LAND_VALUE ~ yield, data = IA_complete), col = "blue")

reg4 <- lm(LAND_VALUE ~ yield + CSR2, data = IA_complete)
stargazer(reg4, type = "latex", 
          title = "Relationship between yield and CSR2 on land value All years, OLS", 
          header = FALSE, table.placement = "H")


pooled_3 <- plm(LAND_VALUE ~ yield + CSR2, data = IA_complete,
                  model = "pooling", 
                  index= c("County"),
                  effect = "individual")

pooled_3_SE <- as.data.frame(tidy(coeftest(pooled_3, 
                                           vcov=vcovHC(pooled_3, 
                                                       type = "HC0", cluster = "group"))))

stargazer(t(pooled_3_SE), type = "latex", 
          title = "Relationship between yield, CSR2 and land value, Pooled OLS Model All Years, Correct SE", 
          header = FALSE, table.placement = "H")

# Let's check again now with more observations how well our model is at predicting land value for each observation. From the t-test below, we can see that there are no statistically significant differences between the land value data from the USDA and our predicted land value from the model. 

#create new column using the equation for each observation
check_df_2 <- IA_complete %>%
  mutate(estimate = -2724.618 + 26.281*yield + 47.951*CSR2) 
print(t.test(check_df_2$LAND_VALUE, check_df$estimate, paired = FALSE)) #see if differences are sig.
#from this point forward I use check_df_2 as my complete dataframe bc it has all observations and the historical and estimated land value - this is kind of lazy naming on my part

#The final equation for this preliminary data exploration is as given:
  
 # $$Land~Value = -2724.618 + 26.281~Yield + 47.951~CSR2 + \beta_3Soil ~health + \varepsilon$$
 # this is writen so oddly because this is Latex format to give you nice equations for your pdf output 
  
  # Graphing
  
#this code exports the dataframe that I created in R to the working directory that I assigned earlier as a csv
write.csv(check_df_2, "IA_complete.csv") #saving my final dataframe into Excel so others can use the data

#filter command is good to restrict your dataframe - works just like you would think it would like Excel
butler_plot <- check_df_2 %>%
  filter(County == "BUTLER") #on the left is the column you want R to filter based on, you can use ==, >=, > etc. and to the left is what you want it to select based on. characters must be in parenthesis but numbers dont have to be

land_value_color <- "333CCC" #ignore this, left over

#ggplot is how you can create really cool graphing - there is TONS of stuff online about this
#aes is for aesthetic - certain commands have to go inside of it but this is usually trial and error for me. 
ggplot(data = butler_plot, aes(x = as.factor(Year))) + #between each line of ggplot you have to add + so it knows its all for one plot. as.factor is used when the numbers are showing up out of order - it tells R to store the data in a diff format
  geom_line(aes(y = LAND_VALUE, group = "County", color ="b"))+ #geom_line tells to make line graph. Did this twice bc I wanted 2 lines
  geom_line(aes(y = estimate, group = "County", color = "r")) +
  labs(x = "Year", y = "Land Value ($/acre)", title = "Butler Land Value over time") + #labels
  scale_color_manual(name = "Value Type", values = c("b" = "black", "r" = "red"), #this was to make the lines diff colors. 
                     labels = c("Historical", "Estimate")) + #this names the lines in the key
  theme(axis.text.x = element_text(size=7, angle=90)) + #rotates the years to 90 degree angle on x axis
  theme(axis.text.x = element_text(vjust = 0)) + #slightly adjust the axis labels to be easier to read
  theme(axis.text.x = element_text(hjust = 0.5))


#averaged all the counties for each year
counties_averaged <- check_df_2 %>%
  group_by(Year) %>% #will assign all of your observations to diff groups based on whatever col you put in parenthesis
  summarise(value = mean(LAND_VALUE), estimate = mean(estimate)) #after you use group_by it is almost always followed by summarise
#summarise creates a new column similar to mutate, but instead of adding a col to the existing df, the only columns will be what you grouped by above and whatever columns you create in summarise
#value and estimate is the new columnn, and then you write the command on the right. So it gives us the average for historical and estimate for each year. 

#same thing as before
ggplot(data = counties_averaged, aes(x = as.factor(Year))) +
  geom_line(aes(y = value, group = "value", color ="b"))+
  geom_line(aes(y = estimate, group = "estimate", color = "r")) +
  labs(x = "Year", y = "Land Value ($/acre)", title = "County Averaged Land Value over time") +
  scale_color_manual(name = "Value Type", values = c("b" = "black", "r" = "red"), 
                     labels = c("Historical", "Estimate")) +
  theme(axis.text.x = element_text(size=7, angle=90)) + 
  theme(axis.text.x = element_text(vjust = 0)) +
  theme(axis.text.x = element_text(hjust = 0.5))
