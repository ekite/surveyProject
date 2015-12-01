## survey project in R

#install and load library
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr) #load
library(ggplot2) #load

# download a file
download.file("http://files.figshare.com/2236372/combined.csv",  "data/portal_data_joined.csv")

#import a file
surveys <- read.csv('data/portal_data_joined.csv')


## Ideas for Project: 
#### Relationship, Distribution and Comparison

### Relationship graph: Relationship between Weight and Hindfoot in Female Neotoma albigula w/ best fit line
### Distribution: Rodent (Dipodomys merriami) hindfoot length
### Comparison Graph: sex and month only looking at 1981 look at genus and species Dipodomys spectabillis
##### Stats on Overall and Female Dipodomys spectabillis weight differences in 1981.pdf graph (t-test)



#####Data Parsing Ideas
##Possible ways to filter Data:

# filtering years
years <- c(1981, 1982)

#filtering hindfoot length
Hindfoot_length_noNA <- surveys %>%
filter (!is.na(hindfoot_length))

#taxa=Rodent
#Rodent
Rodent <- surveys %>%
  filter (taxa == "Rodent")

##### look at M and F diff in weight
select(surveys, sex, weight)
# look at M weight
maleWeight <- surveys %>%
  filter(sex == "M") %>%
  filter (!is.na(weight))
# look at F weight
femaleWeight <- surveys %>%
  filter(sex == "F") %>%
  filter (!is.na(weight))


# look at genus and species Dipodomys spectabillis
year_D_spect <- surveys %>%
  filter(genus == "Dipodomys") %>%
  filter (species == "spectabilis" ) %>%
  filter (!sex =="") %>%
  filter (year %in% years ) # this selects for the 2 specific years we picked and assigned to "years"


# look at Dipodomys merriami
year_D_merri <- surveys %>%
  filter(genus == "Dipodomys") %>%
  filter (species == "merriami" ) %>%
  filter (!sex =="") %>%
  filter (year %in% years )




#Graphs:

##### Comparison Graph: sex and month only looking at 1981, look at Dipodomys spectabillis
# Parsing Data: 

# filtering years
years <- c(1981, 1982)

# look at genus and species Dipodomys spectabillis
year_D_spect <- surveys %>%
  filter(genus == "Dipodomys") %>%
  filter (species == "spectabilis" ) %>%
  filter (!sex =="") %>%
  filter (year %in% years ) # this selects for the 2 specific years we picked and assigned to "years"

###Graph: BoxPlot, facet wrapped
# Comparison Graph 1: 
pdf("Month by Month Male and Female Dipodomys spectabillis weight differences in 1981.pdf") # start saving as pdf
ggplot(data = year_D_spect, aes(x = 1981, y = weight, color = sex)) + geom_boxplot()+ facet_grid(sex ~ month) + ggtitle("Month by Month Male and Female Dipodomys spectabillis\nweight differences in 1981") + theme(plot.title = element_text(lineheight=.8, face="bold")) + xlab ("1981") + ylab ("Weight (g)") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
dev.off() # stop saving as pdf

# Comparison Graph 2: 
### to look at overall weight differences in 1981
## Will use this graph to run stats/t-test
pdf("Overall and Female Dipodomys spectabillis weight differences in 1981.pdf") # start saving as pdf
ggplot(data = year_D_spect, aes(x = "1981", y = weight, color = sex)) + geom_boxplot()+ facet_grid(~sex ) + ggtitle("Overall Male and Female Dipodomys spectabillis weight\ndifferences 1981") +  theme(plot.title = element_text(lineheight=.8, face="bold")) + xlab ("Year : 1981") + ylab ("Weight (g)") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
dev.off() # stop saving as pdf


##### Stats on Overall Male and Female Dipodomys spectabillis weight differences in 1981.pdf graph:
##### look at male and female differences in weight(g)
select(surveys, sex, weight)

# looking at Dipodomys spectabilis Male weight in 1981
maleWeight_1981<- surveys %>%
  filter(sex == "M") %>%
  filter (!is.na(weight)) %>%
  filter(genus == "Dipodomys") %>%
  filter (species == "spectabilis" ) %>%
  filter (year == "1981")
# save as value (male data)
Mweight_D_Spec <- maleWeight_1981$weight
# statistical analysis on male
mean (Mweight_D_Spec) # 125.287
max(Mweight_D_Spec) # 170
min(Mweight_D_Spec) #65
sd(Mweight_D_Spec) # 22.823

# looking at Dipodomys spectabilis Female weight in 1981
femaleWeight_1981 <- surveys %>%
  filter(sex == "F") %>%
  filter (!is.na(weight)) %>%
  filter(genus == "Dipodomys") %>%
  filter (species == "spectabilis" ) %>%
  filter (year == "1981")
# save as value (female data)
Fweight_D_Spec <- femaleWeight_1981$weight
#statistical tests:
mean (Fweight_D_Spec) # 126.566
max(Fweight_D_Spec) # 172
min(Fweight_D_Spec) # 47
sd(Fweight_D_Spec) # 23.105


#t-test on Fweight_D_Spec and Mweight_D_Spec
t.test(Fweight_D_Spec, Mweight_D_Spec)




###### Relationship graph: Relationship between Weight and Hindfoot in Female Neotoma albigula w/ best fit line
# Parsing Data:
# look at F weight
W_H_FemaleNeo <- surveys %>%
  filter(sex == "F") %>%
  filter (!is.na(weight)) %>%
  filter (!is.na(hindfoot_length)) %>%
  filter (genus =="Neotoma") %>%
  filter (species == "albigula")  %>%
  filter (taxa == "Rodent")
  
# Graph: 
pdf("Relationship between Weight and Hindfoot in Female Neotoma albigula with best fit line.pdf") # start saving as pdf
ggplot(data = W_H_FemaleNeo, aes(x = hindfoot_length, y = weight)) + geom_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + geom_point() + ggtitle("Relationship between Weight and Hindfoot \nin Female Neotoma albigula") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) + xlab ("Hindfoot Length (mm)") + ylab ("Weight (g)") 
dev.off() # stop saving as pdf


######Distribution graph: Rodent (Dipodomys merriami) hindfoot length
#Parsing Data:
distrabution <- surveys %>%
  filter (!is.na(hindfoot_length)) %>%
  filter (taxa == "Rodent") %>%
  filter (!sex =="F") %>%
  filter (species == "merriami")

# to look at end of data bc some data is omitted in table to pick proper labels
distrabution %>%
  tail

pdf("Rodent (Dipodomys merriami) hindfoot length.pdf") # start saving as pdf
ggplot(distrabution, aes(x=hindfoot_length)) + geom_histogram(binwidth=1, aes(fill = ..count..)) +
  scale_fill_gradient("Rodent count", low = "green", high = "red") + xlab ("Hindfoot length (mm)") + ylab ("Rodent (Dipodomys merriami)") + ggtitle("Rodent (Dipodomys merriami) hindfoot length (mm)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))
dev.off() # stop saving as pdf


##Citations:
citation ("ggplot2")
citation("dplyr")