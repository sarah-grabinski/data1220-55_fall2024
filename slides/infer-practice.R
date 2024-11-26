library(readxl)
LungCapData <- read_excel("john-carroll/data1220/quiz/files/LungCapData.xls")
View(LungCapData)

library(infer)

phat_smoke <- LungCapData |>
  observe(response = Smoke, 
          success = 'yes', 
          stat = 'prop')

phat_smoke

dist_smoke <- LungCapData |>
  specify(response = Smoke, 
          success = 'yes') |>
  assume(distribution = "z")

dist_smoke

ci_smoke <- get_ci(dist_smoke,
                    point_estimate = phat_smoke)

ci_smoke

