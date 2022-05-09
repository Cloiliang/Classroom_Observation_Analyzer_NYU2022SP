answers <- read.csv("data/Answer.csv")
questions <- read.csv("data/Question.csv")
results <- read.csv("data/Result.csv")
qas <- cbind(questions, answers[,-c(1)])
results$SectionName