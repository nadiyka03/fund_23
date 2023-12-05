# Topic 1: Data Frames in R
# 
# 1. You have been provided with a data set nobel.csv.
# Read it into R and assign it to the variable nobel.
# Choose the correct separator and header option. Choose stringsAsFactors = FALSE.
nobel<- read.table("~/documents/programming_code/csv_files_oct/nobel.csv", sep=",", header = TRUE, stringsAsFactors = FALSE)
# 2. Get familiar with the data set. Print out:
#   1. Only the column names (as vector)
print(colnames(nobel))
#   2. The first 5 entries of the data frame. (no need to paste as comment)
print(head(nobel))
#   3. The total number of rows.
print(nrow(nobel))
#   4. Write in a few words what this data set contains
# it contains information about Nobel prize winners
# 3. The column category should rather be a factor, since only few values are possible.
#   1. Convert it into a factor column.
nobel[["category"]] <- as.factor(nobel[["category"]])
#   2. Display all possible values this new column can take
print(levels(nobel$category))
# 4. Answer the following questions (by code) :
#   1. Who won the nobel prize in literature in 1990? (Show firstname and surname)
print(nobel[nobel$category== "literature" & nobel$year=="1990", c("firstname", "surname") ])
#   2. In what category and year did Barack Obama win? 
print(nobel[nobel$firstname=="Barack"& nobel$surname=="Obama", "category" ])
#   3. How many people (num of rows) have won the price in chemistry (in the list)?
print(nrow(nobel[nobel$category=="chemistry",]))
#   4. When was the first time that 3 people shared a nobel prize?
print(min(nobel[nobel$share== 3, "year"]))
#   5. What is the approximate average number of laureates sharing the literature prize? (you can leave in double rows)
print(mean(nobel[nobel$category =="literature", "share"]))
        
# 5.One entry in the data set is actually fraudulent. This person has NOT won a prize.
# Find it by comparing the number of rows to the number given in share
# for each year, for each category. You may use two nested for-loops.
years <- nobel$year
shares <- nobel$share
categories <- levels(nobel$category)
for(year in years) {
  for (category in categories){
  for ( share in shares) { 
    if(sum(1/nobel[(nobel$year == year) & (nobel$category== category), "share"]) > 1) {
      print(paste("Imposter found! Its ", nobel[(nobel$year == year) & (nobel$category== "economics"),  "surname" ] ))
      break
    } 
    }
  }
}
# Once you found year and category, investigate the motivation to find the imposter!
print(nobel[(nobel$surname == "Fogel"),  "motivation" ])
print(nobel[(nobel$surname == "North"),  "motivation" ])
print(nobel[(nobel$surname == "Doe"),  "motivation" ])


# Topic 2: Visual Conditions

# 1. You are provided with the functions showPicture() and getColor(x,y) that you know from the lecture.
# showPicture() can be called to paint a plot of random dots with different colors.
# The color of each dot is set by the rules (conditions on x and y coordinates) inside 	getColor(x,y).
# In the example given, one half of the picture is red, one half blue.
showPicture <- function() {
  plot(x=0.5,y=0.5,xlim=c(0,1),ylim=c(0,1),xlab="x",ylab="y")
  for (i in 1:10000) {
    x <- runif(1)
    y <- runif(1)	
    points(x,y,col=getColor_1(x,y),cex=0.2,pch=19)
  }
}

# Recreate the following pictures by writing the functions getColor_number(x,y).
# Make sure in order to test your functions to change the call
# of getColor inside showPicture to your current task's function.

# 1.
getColor_1 <- function(x,y) {
  if ((x < 0.5) & (y < 0.5)) {
    return("red")
  }
  return("blue")
}
# 2.
getColor_2 <- function(x,y) {
  if ((x < y) & (x < 0.5) & (x+y<1)){
    return("red")
  }
  return("blue")
}
# 3.
getColor_3 <- function(x,y) {
  if ( (x > 0.4) & (x < 0.6) & (y > 0.4) & (y < 0.6)) {
    return("red")
  } else {
    return("blue")
  }
}
# 4. 
getColor_4 <- function(x,y) {
  if (floor(x * 5) %% 2 == 0) {
    return("blue")
  }
  return("red")
}
#5 
getColor_5 <- function(x,y) {
  if ((floor(x * 5) %% 2 == 0) & (floor(y * 5) %% 2 == 0)){
    return("blue")
  }
  return("red")
}

#7
# I don't know how to do the picture, but I tried to write is.inRectangle function 
# AND it pretty much works
is.inRectangle <- function(x, y, x_mid, x_length, y_mid, y_length ){
  x1 <- x_mid - x_length / 2
  x2 <- x_mid + x_length / 2
  y1 <- y_mid - y_length / 2
  y2 <- y_mid + y_length / 2
  if((x >= x1) & (x <= x2) & (y >= y1) & (y <= y2)) {
    return(TRUE)
  }
}
