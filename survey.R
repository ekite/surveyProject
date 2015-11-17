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

## ideas: 

#look at could look at weight of M vs F in Neotoma over the different years
#F and M weight differences between diff Dipodomys merriami and Dipodomys spectabilis. over time

## maybe composition of data in sample

# data parsing

# build figure

# look at M and F diff in weight
select(surveys, sex, weight)

# select sex, weight for only the genus Nematoda
surveys %>%
  filter(genus == Neotoma) %>%
  select(sex , weight)

