library(dplyr)
library(ggplot2)
library(plotly)
library(grid)
library(gridExtra)
library(DT)
library(GGally)
library(randomForest)

primary <- read.csv("C:/Users/tsaig/OneDrive/Documents/00 - Data Sets/2016_presidential_election_2016-03-25-21-27-54/2016_presidential_election/primary_results.csv", stringsAsFactors = FALSE)
demographics <- read.csv("C:/Users/tsaig/OneDrive/Documents/00 - Data Sets/2016_presidential_election_2016-03-25-21-27-54/2016_presidential_election/county_facts.csv", stringsAsFactors = FALSE)

votes <- primary %>%   #get the winners and the fraction of votes the won
  filter(party == "Democrat") %>% 
  group_by(state_abbreviation, county) %>% 
  summarize(winner = candidate[which.max(fraction_votes)],
            Vote = max(fraction_votes),
            votes = max(votes))

demographics %<>%
  filter(state_abbreviation %in% c("IA", "NV", "SC")) %>% 
  select(state_abbreviation = state_abbreviation, county = area_name, 
         income = INC110213, hispanic = RHI725214,
         white= RHI825214, college = EDU685213, density = POP060210) %>% 
  # This particular mutate function is finding all the Counties name from the county_facts data, removing the 'County' term from every counties name and replacing it with no space and finally saving it in the column name County. For example, We had the county's name in the data as 'Adair County'. So the function found out the ' County", replaced it with "" and then saved it back in the coulmn name 'county' to be used in the present analysis. 
  mutate(county = gsub(" County", "", county))

votes <- inner_join(votes, demographics, by = c("state_abbreviation","county"))

datatable(votes, class = 'compact')

votes %>% 
  group_by(winner) %>% 
  summarize(round(mean(income)), round(mean(white)), 
            round(mean(college),1),round(mean(density)),round(mean(hispanic),1))%>%      
  datatable( colnames = c(" ",  "Winner", "Income", "White (non-Hispanic)", "Colege", "Density (pop/sq m)", "Hispanic"), class = 'compact', caption = "Average County Demographics by Winner")

ggplotly(qplot(x =  white, y = college, data = votes, 
               color = winner, size = Vote) +
           ggtitle("Counties by Winner, Whiteness and Educational Attainment"))

ggplotly(qplot(x =  white, y = hispanic, data = votes, 
               color = winner, size = Vote) +
           ggtitle("Counties by Winner, Percentage of Whites and Non-Hispanic Whites"))

ggplotly(qplot(x =  white, y = hispanic, data = votes, 
               color = winner, size = Vote, xlim = c(92, 98), ylim = c(1,5)))

ggplotly(qplot(x =  income, y = college, data = votes, 
               color = winner, size = Vote) +
           ggtitle("Counties by Income, Educational Attainment colored by Winner"))

votes$winner <- as.factor(votes$winner)
ggplot(data = votes, aes(x = winner, y = density, fill = winner)) +
  geom_boxplot() + coord_flip()

ggplotly(qplot(x =  income, y = college, data = votes, 
               color = winner, size = votes) +
           ggtitle("Counties by Income, Educational Attainment colored by Winner"))

### Hillary's fraction of votes
Hillary <- primary %>%
  filter(candidate == "Hillary Clinton")
Hillary <- inner_join( Hillary, demographics, by = c("state_abbreviation","county"))
Hillary$income <- Hillary$income/1000
Hillary$fraction_votes <- Hillary$fraction_votes *100 #normalize a bit
g1 <- qplot(x = income, y = fraction_votes, data = Hillary, ylab = "fraction of votes") +
  geom_smooth(method='lm',formula=y~x)

g2 <- qplot(x = college, y = fraction_votes, data = Hillary, ylab = "fraction of votes") +
  geom_smooth(method='lm',formula=y~x)

g3 <- qplot(x = white, y = fraction_votes, data = Hillary, ylab = "fraction of votes") +
  geom_smooth(method='lm',formula=y~x)

g4 <- qplot(x = density, y = fraction_votes, data = Hillary, ylab = "fraction of votes") +
  geom_smooth(method='lm',formula=y~x)

grid.arrange(g1, g2, g3, g4, nrow =2, ncol = 2)

summary(lm(fraction_votes ~ college+white+hispanic+income+density, data = trump))

summary(lm(fraction_votes ~ college+white+hispanic+income+density, data = rubio))
