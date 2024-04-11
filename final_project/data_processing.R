library(dplyr)
library(ggplot2)
# read data
df = read.csv("final_project/SelfRef_Maths_Exp1.csv")

# randomization seed 
set.seed(713)

# we used a subset of 75 children who answered the simple algebra word question 
# incorrectly and in each case inspected the length of the question. 
# in interest of classification, we separated them into 4 categories dictating
# the question length as short, average, long or very long

wrong = df[df$ResponseCode == 0 & nchar(df$QuestionText)>0, ]
i = sample(1:nrow(wrong), 75)
subdf = wrong[i, ]
# get lengths of question
lengths = as.numeric(sapply(subdf$QuestionText, nchar))
# time to answer question, in miliseconds
time = subdf$ResponseRT
# categorization of question length
category <- case_when(
  lengths <= 75 ~ "short",
  lengths <= 90 ~ "average",
  lengths <= 100 ~ "long",
  TRUE ~ "very long"  
)
# out df for model
out_df = data.frame(
  K = category, 
  # convert to seconds
  Y = time*0.001,
  X = lengths
)
# plotting to make sure it's all working
print( ggplot(out_df, aes(x = X, y = Y, col = K)) + geom_point() + theme_bw() )

# write to CSV
write.csv( out_df, file = "main_data.csv", row.names = FALSE)
