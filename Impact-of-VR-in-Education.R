# Read .csv file
library(readr)
VR_Dataset <- read_csv('C:/Users/nadle/Downloads/Virtual_Reality_in_Education_Impact.csv')
View(VR_Dataset)

# Create subset of data of students who used VR in the classroom
yes_vr <- subset(VR_Dataset, Usage_of_VR_in_Education == 'Yes')
View(yes_vr)

# Create scatter plots for students who did experience academic improvement
library(ggplot2)

ggplot(yes_vr, aes(x = Field_of_Study, fill = Improvement_in_Learning_Outcomes)) +
  geom_bar(position = "fill")+ 
  theme(plot.title = element_text(hjust = 0.5))