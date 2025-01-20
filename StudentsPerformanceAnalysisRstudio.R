# Core R library
library(stats)

# Data Manipulation and Visualization
library(tidyverse)

# Upload of csv file path
file_path <- "C:/Users/user/OneDrive/Documents/Students performance case study/StudentsPerformance.csv"

# Assigning file to a variable
Students_Performance <- read_csv(file_path)

print(Students_Performance)

view(Students_Performance)

# Gender
# Count the number of males and females
male_count <- sum(Students_Performance$Gender == "Male")
female_count <- sum(Students_Performance$Gender == "Female")
# Filter for students who scored above 67
high_scorers <- Students_Performance[Students_Performance$Exam_Score > 67, ]
# Count male and female high scorers
male_high_scorers <- sum(high_scorers$Gender == "Male")
female_high_scorers <- sum(high_scorers$Gender == "Female")
# Calculate the percentage of male and female high scorers
male_percentage <- (male_high_scorers / male_count) * 100
female_percentage <- (female_high_scorers / female_count) * 100
# Print the results
print(paste("Number of male students:", male_count))
print(paste("Number of female students:", female_count))
print(paste("Number of male high scorers:", male_high_scorers))
print(paste("Number of female high scorers:", female_high_scorers))
print(paste("Percentage of male high scorers:", round(male_percentage, 2), "%"))
print(paste("Percentage of female high scorers:", round(female_percentage, 2), "%"))


# Parental involvement
# Count the parental involvement categories
PIlowcount <- sum(Students_Performance$Parental_Involvement == "Low")
PImediumcount <- sum(Students_Performance$Parental_Involvement == "Medium")
PIhighcount <- sum(Students_Performance$Parental_Involvement == "High")
# Filter for students who scored above 67
high_scorers <- Students_Performance[Students_Performance$Exam_Score > 67, ]
# Analyze how parent involvement categories influenced high scores
PIlowcount_highscorers<- sum(high_scorers$Parental_Involvement == "Low")
PImediumcount_highscorers <- sum(high_scorers$Parental_Involvement == "Medium")
PIhighcount_highscorers <- sum(high_scorers$Parental_Involvement == "High")
# Parent involvement influence on high scores in percentages
PIlowpercentage <- (PIlowcount_highscorers / PIlowcount) * 100
PImediumpercentage <- (PImediumcount_highscorers / PImediumcount) * 100
PIhighpercentage <- (PIhighcount_highscorers / PIhighcount) * 100
# Print the results
print(paste("Number of low parental involvement:", PIlowcount))
print(paste("Number of medium parental involvement:", PImediumcount))
print(paste("Number of high parental involvement:", PIhighcount))
print(paste("Number of low parental involvement highscorers:", PIlowcount_highscorers))
print(paste("Number of medium parental involvement highscorers:", PImediumcount_highscorers))
print(paste("Number of high parental involvement highscorers:", PIhighcount_highscorers))
print(paste("Percentage of low parental involvement highscorers:", round(PIlowpercentage, 2), "%"))
print(paste("Percentage of medium parentalinvolvement highscorers:", round(PImediumpercentage, 2), "%"))
print(paste("Percentage of high parentalinvolvement highscorers:", round(PIhighpercentage, 2), "%"))


# Family income
# Count the family income categories
FIlowcount <- sum(Students_Performance$Family_Income == "Low")
FImediumcount <- sum(Students_Performance$Family_Income == "Medium")
FIhighcount <- sum(Students_Performance$Family_Income == "High")
# Filter for students who scored above 67
high_scorers <- Students_Performance[Students_Performance$Exam_Score > 67, ]
# Analyze how family income categories influenced high scores
FIlowcount_highscorers<- sum(high_scorers$Family_Income == "Low")
FImediumcount_highscorers <- sum(high_scorers$Family_Income == "Medium")
FIhighcount_highscorers <- sum(high_scorers$Family_Income == "High")
# Family income influence on high scores in percentages
FIlowpercentage <- (FIlowcount_highscorers / FIlowcount) * 100
FImediumpercentage <- (FImediumcount_highscorers / FImediumcount) * 100
FIhighpercentage <- (FIhighcount_highscorers / FIhighcount) * 100
# Print the results
print(paste("Number of low family income:", FIlowcount))
print(paste("Number of medium family income:", FImediumcount))
print(paste("Number of high family income:", FIhighcount))
print(paste("Number of low family income highscorers:", FIlowcount_highscorers))
print(paste("Number of medium family income highscorers:", FImediumcount_highscorers))
print(paste("Number of high family income highscorers:", FIhighcount_highscorers))
print(paste("Percentage of low  family income highscorers:", round(FIlowpercentage, 2), "%"))
print(paste("Percentage of medium family income highscorers:", round(FImediumpercentage, 2), "%"))
print(paste("Percentage of high family income highscorers:", round(FIhighpercentage, 2), "%"))


# Parental education level
# Count parental education levels 
highschool <- sum(Students_Performance$Parental_Education_Level == "High School")
college <- sum(Students_Performance$Parental_Education_Level == "College")
postgraduate <- sum(Students_Performance$Parental_Education_Level == "Postgraduate")
# Filter for students who scored above 67
high_scorers <- Students_Performance[Students_Performance$Exam_Score > 67, ]
# Analyze how parents education level influenced high scores
highschool_above67 <- sum(high_scorers$Parental_Education_Level == "High School")
college_above67 <- sum(high_scorers$Parental_Education_Level == "College")
postgraduate_above67 <- sum(high_scorers$Parental_Education_Level == "Postgraduate")
# Family income influence on high scores in percentages
highschoolpercentage <- (highschool_above67 / highschool) * 100
collegepercentage <- (college_above67 / college) * 100
postgraduatepercentage <- (postgraduate_above67 / postgraduate) * 100
# Print the results
print(paste("Number of highschool parents:", highschool))
print(paste("Number of college parents:", college))
print(paste("Number of postgraduate parents:", postgraduate))
print(paste("Number of highschool parents and above67:", highschool_above67))
print(paste("Number of college parents and above67:", college_above67))
print(paste("Number of postgraduate parents and above67:", postgraduate_above67))
print(paste("Percentage of highschool parents and above 67:", round(highschoolpercentage, 2), "%"))
print(paste("Percentage of college parents and above 67:", round(collegepercentage, 2), "%"))
print(paste("Percentage of postgraduate parents and above 67:", round(postgraduatepercentage, 2), "%"))


# Access to resources
# Count access to resources categories
low <- sum(Students_Performance$Access_to_Resources == "Low")
medium <- sum(Students_Performance$Access_to_Resources == "Medium")
high <- sum(Students_Performance$Access_to_Resources == "High")
# Filter for students who scored above 67
high_scorers <- Students_Performance[Students_Performance$Exam_Score > 67, ]
# Analyze how access to resources levels influenced high scores
low_above67<- sum(high_scorers$Access_to_Resources == "Low")
medium_above67 <- sum(high_scorers$Access_to_Resources == "Medium")
high_above67 <- sum(high_scorers$Access_to_Resources == "High")
# Family income influence on high scores in percentages
lowpercentage <- (low_above67 / low) * 100
mediumpercentage <- (medium_above67 / medium) * 100
highpercentage <- (high_above67 / high) * 100
# Print the results
print(paste("Number of low access to resources:", low))
print(paste("Number of medium access to resources:", medium))
print(paste("Number of high access to resources:", high))
print(paste("Number of low access to resources and above67:", low_above67))
print(paste("Number of medium access to resources and above 67:", medium_above67))
print(paste("Number of high access to resources and above 67:", high_above67))
print(paste("Percentage of low access to resources and above 67  :", round(lowpercentage, 2), "%"))
print(paste("Percentage of medium access to resources and above 67:", round(mediumpercentage, 2), "%"))
print(paste("Percentage of high access to resources and above 67:", round(highpercentage, 2), "%"))


# Teacher quality
# Count teacher quality categories
low <- sum(Students_Performance$Teacher_Quality == "Low")
medium <- sum(Students_Performance$Teacher_Quality == "Medium")
high <- sum(Students_Performance$Teacher_Quality == "High")
# Filter for students who scored above 67
high_scorers <- Students_Performance[Students_Performance$Exam_Score > 67, ]
# Analyze how teacher quality levels influenced high scores
low_above67<- sum(high_scorers$Teacher_Quality == "Low")
medium_above67 <- sum(high_scorers$Teacher_Quality == "Medium")
high_above67 <- sum(high_scorers$Teacher_Quality == "High")
# Teacher quality influence on high scores in percentages
lowpercentage <- (low_above67 / low) * 100
mediumpercentage <- (medium_above67 / medium) * 100
highpercentage <- (high_above67 / high) * 100
# Print the results
print(paste("Number of low teacher quality:", low))
print(paste("Number of medium teacher_quality:", medium))
print(paste("Number of high teacher quality:", high))
print(paste("Number of low teacher quality and above67:", low_above67))
print(paste("Number of medium teacher quality and above 67:", medium_above67))
print(paste("Number of high teacher quality and above 67:", high_above67))
print(paste("Percentage of low teacher quality and above 67  :", round(lowpercentage, 2), "%"))
print(paste("Percentage of medium teacher quality and above 67:", round(mediumpercentage, 2), "%"))
print(paste("Percentage of high teacher quality and above 67:", round(highpercentage, 2), "%"))


# Motivation level
# Count motivation levels
low <- sum(Students_Performance$Motivation_Level == "Low")
medium <- sum(Students_Performance$Motivation_Level == "Medium")
high <- sum(Students_Performance$Motivation_Level == "High")
# Filter for students who scored above 67
high_scorers <- Students_Performance[Students_Performance$Exam_Score > 67, ]
# Analyze how motivation levels influenced high scores
low_above67<- sum(high_scorers$Motivation_Level == "Low")
medium_above67 <- sum(high_scorers$Motivation_Level == "Medium")
high_above67 <- sum(high_scorers$Motivation_Level == "High")
# Motivation levels influence on high scores in percentages
lowpercentage <- (low_above67 / low) * 100
mediumpercentage <- (medium_above67 / medium) * 100
highpercentage <- (high_above67 / high) * 100
# Print the results
print(paste("Number of low motivation level:", low))
print(paste("Number of medium motivation level:", medium))
print(paste("Number of high motivation level:", high))
print(paste("Number of low motivation level and above67:", low_above67))
print(paste("Number of medium motivation level and above 67:", medium_above67))
print(paste("Number of high motivation level and above 67:", high_above67))
print(paste("Percentage of low motivation level and above 67:", round(lowpercentage, 2), "%"))
print(paste("Percentage of medium motivation level and above 67:", round(mediumpercentage, 2), "%"))
print(paste("Percentage of high motivation level and above 67:", round(highpercentage, 2), "%"))


# Peer influence
# Count peer influence categories
negative <- sum(Students_Performance$Peer_Influence == "Negative")
neutral <- sum(Students_Performance$Peer_Influence == "Neutral")
positive <- sum(Students_Performance$Peer_Influence == "Positive")
# Filter for students who scored above 67
high_scorers <- Students_Performance[Students_Performance$Exam_Score > 67, ]
# Analyze how peer influence influenced high scores
negative_above67 <- sum(high_scorers$Peer_Influence == "Negative")
neutral_above67 <- sum(high_scorers$Peer_Influence == "Neutral")
positive_above67 <- sum(high_scorers$Peer_Influence == "Positive")
# Peer influence on high scores in percentages
negativepercentage <- (negative_above67 / negative) * 100
neutralpercentage <- (neutral_above67 / neutral) * 100
positivepercentage <- (positive_above67 / positive) * 100
# Print the results
print(paste("negative peer influence:", negative))
print(paste("neutral peer influence:", neutral))
print(paste("positive peer influence:", positive))
print(paste("negative peer influence and above67:", negative_above67))
print(paste("neutral peer influence and above 67:", neutral_above67))
print(paste("positive peer influence and above 67:", positive_above67))
print(paste("Percentage of negative peer influence and above 67:", round(negativepercentage, 2), "%"))
print(paste("Percentage of neutral peer influence and above 67:", round(neutralpercentage, 2), "%"))
print(paste("Percentage of positive peer influence and above 67:", round(positivepercentage, 2), "%"))


# Distance from home
# Count distance from home categories
near <- sum(Students_Performance$Distance_from_Home == "Near")
moderate <- sum(Students_Performance$Distance_from_Home == "Moderate")
far <- sum(Students_Performance$Distance_from_Home == "Far")
# Filter for students who scored above 67
high_scorers <- Students_Performance[Students_Performance$Exam_Score > 67, ]
# Analyze how distance from home influenced high scores
near_above67 <- sum(high_scorers$Distance_from_Home == "Near")
moderate_above67 <- sum(high_scorers$Distance_from_Home == "Moderate")
far_above67 <- sum(high_scorers$Distance_from_Home == "Far")
# Distance from home influence on high scores in percentages
nearpercentage <- (near_above67 / near) * 100
moderatepercentage <- (moderate_above67 / moderate) * 100
farpercentage <- (far_above67 / far) * 100
# Print the results
print(paste("students living near:", near))
print(paste("students living in a moderate distance:", moderate))
print(paste("students living far:", far))
print(paste("students living near and above67:", near_above67))
print(paste("students living in a moderate distance and above 67:", moderate_above67))
print(paste("students living far and above 67:", far_above67))
print(paste("Percentage of students living near and above 67:", round(nearpercentage, 2), "%"))
print(paste("Percentage of students living in a moderate distance and above 67:", round(moderatepercentage, 2), "%"))
print(paste("Percentage of students living far and above 67:", round(farpercentage, 2), "%"))


# School type
# Count school type categories
private <- sum(Students_Performance$School_Type == "Private")
public <- sum(Students_Performance$School_Type == "Public")
# Filter for students who scored above 67
high_scorers <- Students_Performance[Students_Performance$Exam_Score > 67, ]
# Analyze how school type influenced high scores
private_above67 <- sum(high_scorers$School_Type == "Private")
public_above67 <- sum(high_scorers$School_Type == "Public")
# School influence on high scores in percentages
privatepercentage <- (private_above67 / private) * 100
publicpercentage <- (public_above67 / public) * 100
# Print the results
print(paste("private students:", private))
print(paste("Public students:", public))
print(paste("private students and above67:", private_above67))
print(paste("public students and above 67:", public_above67))
print(paste("Percentage of private students and above 67:", round(privatepercentage, 2), "%"))
print(paste("Percentage of public students and above 67:", round(publicpercentage, 2), "%"))


# Extracurricular activities
# Count extracurricular activities categories
yes <- sum(Students_Performance$Extracurricular_Activities == "Yes")
no <- sum(Students_Performance$Extracurricular_Activities == "No")
# Filter for students who scored above 67
high_scorers <- Students_Performance[Students_Performance$Exam_Score > 67, ]
# Analyze how extracurricular activities influenced high scores
yes_above67 <- sum(high_scorers$Extracurricular_Activities == "Yes")
no_above67 <- sum(high_scorers$Extracurricular_Activities == "No")
# School influence on high scores in percentages
yespercentage <- (yes_above67 / yes) * 100
nopercentage <- (no_above67 / no) * 100
# Print the results
print(paste("extracurricular activities:", yes))
print(paste("no extracurricular activity:", no))
print(paste("extracurricular activities and above67:", yes_above67))
print(paste("no extracurricular activity and above 67:", no_above67))
print(paste("Percentage of extracurricular activity and above 67:", round(yespercentage, 2), "%"))
print(paste("Percentage of no extracurricular activity and above 67:", round(nopercentage, 2), "%"))


# Internet access
# Count internet access categories
yes <- sum(Students_Performance$Internet_Access == "Yes")
no <- sum(Students_Performance$Internet_Access == "No")
# Filter for students who scored above 67
high_scorers <- Students_Performance[Students_Performance$Exam_Score > 67, ]
# Analyze how internet access influenced high scores
yes_above67 <- sum(high_scorers$Internet_Access == "Yes")
no_above67 <- sum(high_scorers$Internet_Access == "No")
# Internet access influence on high scores in percentages
yespercentage <- (yes_above67 / yes) * 100
nopercentage <- (no_above67 / no) * 100
# Print the results
print(paste("internet access:", yes))
print(paste("no internet access:", no))
print(paste("internet access and above67:", yes_above67))
print(paste("no internet access and above 67:", no_above67))
print(paste("Percentage of internet access and above 67:", round(yespercentage, 2), "%"))
print(paste("Percentage of no internet access and above 67:", round(nopercentage, 2), "%"))



# Learning disabilities
# Count learning disability categories
yes <- sum(Students_Performance$Learning_Disabilities == "Yes")
no <- sum(Students_Performance$Learning_Disabilities == "No")
# Filter for students who scored above 67
high_scorers <- Students_Performance[Students_Performance$Exam_Score > 67, ]
# Analyze how learning disability influenced high scores
yes_above67 <- sum(high_scorers$Learning_Disabilities == "Yes")
no_above67 <- sum(high_scorers$Learning_Disabilities == "No")
# Learning disabilities influence on high scores in percentages
yespercentage <- (yes_above67 / yes) * 100
nopercentage <- (no_above67 / no) * 100
# Print the results
print(paste("learning disability:", yes))
print(paste("no learning disability:", no))
print(paste("learning disability and above67:", yes_above67))
print(paste("no learning disability and above 67:", no_above67))
print(paste("Percentage of learning disability and above 67:", round(yespercentage, 2), "%"))
print(paste("Percentage of no learning disability and above 67:", round(nopercentage, 2), "%"))

# Hours studied
# Categorize hours studied
Students_Performance$Hours_Category <- cut(Students_Performance$Hours_Studied, breaks = c(0, 15, 30, Inf), labels = c("Short", "Medium", "Long"))
short <- sum(Students_Performance$Hours_Category =="Short")
medium <- sum(Students_Performance$Hours_Category == "Medium")
long <- sum(Students_Performance$Hours_Category == "Long")
# Filter for students who scored above 67
high_scorers <- Students_Performance[Students_Performance$Exam_Score > 67, ]
# Hours studied influence on high scores 
short_above67 <- sum(high_scorers$Hours_Category == "Short")
medium_above67 <- sum(high_scorers$Hours_Category == "Medium")
long_above67 <- sum(high_scorers$Hours_Category == "Long")
#Hours studied influence on high scores in percentages
shortpercentage <- (short_above67 / short) * 100
mediumpercentage <- (medium_above67 / medium) * 100
longpercentage <- (long_above67 / long) * 100
#Print the results
print(paste("short study hours:", short ))
print(paste("medium study hours:", medium ))
print(paste("long study hours:", long ))
print(paste("short study hours and above 67:", short_above67))
print(paste("medium study hours and above 67:", medium_above67))
print(paste("long study hours and above 67:", long_above67))
print(paste("percentage of short study hours and above 67:", shortpercentage))
print(paste("percentage of medium study hours and above 67:", mediumpercentage))
print(paste("percentage of long study hours and above 67:", longpercentage))


# Attendance
# Categorize attendance categories
Students_Performance$Attendance_Category <- cut(Students_Performance$Attendance, breaks = c(59, 74, 89, Inf), labels = c("Poor", "Average", "Good"))
poor <- sum(Students_Performance$Attendance_Category =="Poor")
average <- sum(Students_Performance$Attendance_Category == "Average")
good <- sum(Students_Performance$Attendance_Category == "Good")
# Filter for students who scored above 67
high_scorers <- Students_Performance[Students_Performance$Exam_Score > 67, ]
# Hours studied influence on high scores 
poor_above67 <- sum(high_scorers$Attendance_Category == "Poor")
average_above67 <- sum(high_scorers$Attendance_Category == "Average")
good_above67 <- sum(high_scorers$Attendance_Category == "Good")
# Attendance influence on high scores in percentages
poorpercentage <- (poor_above67 / poor) * 100
averagepercentage <- (average_above67 / average) * 100
goodpercentage <- (good_above67 / good) * 100
#Print the results
print(paste("poor attendance:", poor))
print(paste("average attendance:", average))
print(paste("good attendance:", good))
print(paste("poor attendance and above 67:", poor_above67))
print(paste("average attendance and above 67:", average_above67))
print(paste("good attendance and above 67:", good_above67))
print(paste("percentage of poor attendance and above 67:", poorpercentage))
print(paste("percentage of average attendance and above 67:", averagepercentage))
print(paste("percentage of good attendance and above 67:", goodpercentage))


# Tutoring sessions
# Categorize tutoring sessions categories
Students_Performance$TutoringSessions_Category <- cut(Students_Performance$Tutoring_Sessions, breaks = c(-1, 2, 5, Inf), labels = c("Low", "Moderate", "High"))
low <- sum(Students_Performance$TutoringSessions_Category == "Low")
moderate <- sum(Students_Performance$TutoringSessions_Category == "Moderate")
high <- sum(Students_Performance$TutoringSessions_Category == "High")
# Filter for students who scored above 67
high_scorers <- Students_Performance[Students_Performance$Exam_Score > 67, ]
# Tutoring sessions influence on high scores 
low_above67 <- sum(high_scorers$TutoringSessions_Category == "Low")
moderate_above67 <- sum(high_scorers$TutoringSessions_Category == "Moderate")
high_above67 <- sum(high_scorers$TutoringSessions_Category == "High")
# Tutoring sessions influence on high scores in percentages
lowpercentage <- (low_above67 / low) * 100
moderatepercentage <- (moderate_above67 / moderate) * 100
highpercentage <- (high_above67 / high) * 100
#Print the results
print(paste("low tutoring sessions:", low))
print(paste("moderate tutoring sessions:", moderate))
print(paste("high tutoring sessions:", high))
print(paste("low and above 67:", low_above67))
print(paste("low tutoring sessions and above 67:", low_above67))
print(paste("moderate tutoring sessions and above 67:", moderate_above67))
print(paste("percentage of low tutoring sessions and above 67:", round(lowpercentage, 2), "%"))
print(paste("percentage of moderate tutoring sessions and above 67:", round(moderatepercentage, 2),"%"))
print(paste("percentage of high tutoring sessions and above 67:", round(highpercentage, 2), "%"))


# Physical activities
# Categorize physical activity categories
Students_Performance$PhysicalActivity_Category <- cut(Students_Performance$Physical_Activity, breaks = c(-1, 2, 4, Inf), labels = c("Low", "Moderate", "High"))
low <- sum(Students_Performance$PhysicalActivity_Category == "Low")
moderate <- sum(Students_Performance$PhysicalActivity_Category == "Moderate")
high <- sum(Students_Performance$PhysicalActivity_Category == "High")
# Filter for students who scored above 67
high_scorers <- Students_Performance[Students_Performance$Exam_Score > 67, ]
# Physical activity influence on high scores 
low_above67 <- sum(high_scorers$PhysicalActivity_Category == "Low")
moderate_above67 <- sum(high_scorers$PhysicalActivity_Category == "Moderate")
high_above67 <- sum(high_scorers$PhysicalActivity_Category == "High")
# Physical activities influence on high scores in percentages
lowpercentage <- (low_above67 / low) * 100
moderatepercentage <- (moderate_above67 / moderate) * 100
highpercentage <- (high_above67 / high) * 100
#Print the results
print(paste("low physical activity:", low))
print(paste("moderate physical activity:", moderate))
print(paste("high physical activity:", high))
print(paste("low physical activity and above 67:", low_above67))
print(paste("moderate physical activity and above 67:", low_above67))
print(paste("high physical activity and above 67:", moderate_above67))
print(paste("percentage of low physical activity and above 67:", round(lowpercentage, 2), "%"))
print(paste("percentage of moderate physical activity and above 67:", round(moderatepercentage, 2),"%"))
print(paste("percentage of high physical activity and above 67:", round(highpercentage, 2), "%"))


# Sleep hours
# Categorize sleep hours categories
Students_Performance$Sleephours_Category <- cut(Students_Performance$Sleep_Hours, breaks = c(3, 6, Inf), labels = c("Short", "Long"))
short <- sum(Students_Performance$Sleephours_Category == "Short")
long <- sum(Students_Performance$Sleephours_Category == "Long")
# Filter for students who scored above 67
high_scorers <- Students_Performance[Students_Performance$Exam_Score > 67, ]
# Sleep hours influence on high scores 
short_above67 <- sum(high_scorers$Sleephours_Category == "Short")
long_above67 <- sum(high_scorers$Sleephours_Category == "Long")
# Physical activities influence on high scores in percentages
shortpercentage <- (short_above67 / short) * 100
longpercentage <- (long_above67 / long) * 100
#Print the results
print(paste("short study hours:", short))
print(paste("long study hours:", long))
print(paste("short study hours and above 67:", short_above67))
print(paste("long study hours and above 67:", long_above67))
print(paste("percentage of short study hours and above 67:", round(shortpercentage, 2), "%"))
print(paste("percentage of long study hours and above 67:", round(longpercentage, 2),"%"))



