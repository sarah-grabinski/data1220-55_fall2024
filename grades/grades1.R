rep_points <- read.csv('~/john-carroll/data1220/participation/elementary-statistics-fall-2024-10-13-2024.csv')

rep_points <- rep_points |>
  dplyr::mutate(points = dplyr::case_when(
    rep_points == 0 ~ 0, 
    rep_points < quantile(df$rep_points, 0.1) ~ 1, 
    rep_points < quantile(df$rep_points, 0.2) ~ 2, 
    rep_points < quantile(df$rep_points, 0.3) ~ 3, 
    rep_points < quantile(df$rep_points, 0.4) ~ 4, 
    rep_points < quantile(df$rep_points, 0.5) ~ 5, 
    rep_points < quantile(df$rep_points, 0.6) ~ 6, 
    rep_points < quantile(df$rep_points, 0.7) ~ 7, 
    rep_points < quantile(df$rep_points, 0.8) ~ 8, 
    rep_points < quantile(df$rep_points, 0.9) ~ 9, 
    T ~ 10
  )) |> View()


grades <- read.csv('~/john-carroll/data1220/grades/DATA1220-55 Fall 2024 Grades - Totals.csv')

grades <- grades |>
  dplyr::mutate(letter = dplyr::case_when(
    grade < 60 ~ 'F', 
    grade < 63 ~ 'D-', 
    grade < 67 ~ 'D', 
    grade < 70 ~ 'D+', 
    grade < 73 ~ 'C-', 
    grade < 77 ~ 'C', 
    grade < 80 ~ 'C+', 
    grade < 83 ~ 'B-', 
    grade < 87 ~ 'B', 
    grade < 90 ~ 'B+', 
    grade < 93 ~ 'A-', 
    grade < 97 ~ 'A', 
    T ~ 'A+'
  ))
