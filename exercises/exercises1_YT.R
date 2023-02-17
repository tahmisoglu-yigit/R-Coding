# Name of submitter: <YIGIT TAHMISOGLU>
library(dplyr)
library(stringr)

# Exercise 1: Define the function sq that squares a single number x
sq <- function(x) {
  x^2
}

# Exercise 2: From the `starwars` data, get all the non-human characters with yellow or blue-gray eyes.
# Keep all the columns.
View(starwars)
non_human_eyes <- filter(starwars, species != 'Human',  eye_color == 'yellow' | eye_color == 'blue-gray')
View(non_human_eyes)

# Exercise 3: write the body of the function `non_human_hair` that takes a single argument.
# This argument is a subset from the `starwars` data, and your function should return all the
# non-human characters who could possibly have brown, auburn, or no hair
# Keep only the following columns: name, species, eye_color, homeworld, and hair_color IN THAT ORDER
# Order the rows by species, then eye_color, both ascending alphabetically

non_human_hair <- function(df) {
  df <- filter(starwars, species != 'Human', hair_color == 'brown'| hair_color == 'auburn'| hair_color == 'none')  
  df = subset(df, select = c(name, species, eye_color, homeworld, hair_color)) #checked stackoverflow on how to sort columns
  arrange(df, species, eye_color)
}
View(non_human_hair())

#Use the `msleep` data (built-in dataset in the ggplot2 package) for Exercises 4-7

#Exercise 4. Get all the animals who are heavier than the average bodyweight in the data
#Keep the "name" and "bodywt" of these animals
#Order the rows by bodyweight in a descending order
library(ggplot2)
View(msleep)
heavy_animals <- filter(msleep, bodywt > mean(bodywt))
heavy_animals <- select(heavy_animals, name, bodywt)
arrange(heavy_animals, desc(bodywt))
View(heavy_animals)

#Exercise 5. Create a new column called brainwt_ratio showing the ratio of
# of brain mass to the total body weight. Round the ratio to 4 digits. Keep the name and brainwt colums
# and keep the 10 animals with the highest relative brain mass.

clever_animals <- msleep |>
  mutate(
    brainwt_ratio = round(brainwt / bodywt, 4)
  )
clever_animals <- select(clever_animals, name, brainwt, brainwt_ratio)
clever_animals <- top_n(clever_animals, 10, brainwt_ratio) # top_n function from stackoverflow
clever_animals

#Exercise 6 Create a new column called brainwt_ratio, and keep only this column.
# Use the transmute command

brainweight <- msleep |>
  transmute(
    brainwt_ratio = (brainwt / bodywt)
  )

#Exercise 7 Check whether carnivores, herbivores, insectivores, or omnivores sleep more.
# First, remove the rows where vore is missing (NA)
# Create a data table with 4 rows and 3 columns showing the average,
# and the standard deviation for total sleep time for these 4 groups

meansleep_by_vore <- filter(msleep, vore != 'NA') 
unique(meansleep_by_vore$vore)
meansleep_by_vore <- group_by(meansleep_by_vore, vore)
meansleep_by_vore <- summarise(
  meansleep_by_vore,
  sleep_avg = mean(sleep_total, na.rm = TRUE),
  sleep_sd = sd(sleep_total, na.rm = TRUE)
  )
meansleep_by_vore