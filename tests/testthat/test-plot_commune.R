test_that("plot.commune génère un graphique sans erreur", {
  df_test <- data.frame(
    Libelle_commune = rep("Paris", 5),
    Date_de_naissance = as.Date(c("1980-01-01", "1990-05-10", "2000-07-20", "1975-03-30", "1985-12-15"))
  )
  expect_silent(plot.commune(df_test))
})

test_that("plot.commune retourne une erreur si plusieurs communes sont présentes", {
  df_test <- data.frame(
    Libelle_commune = c("Paris", "Lyon"),
    Date_de_naissance = as.Date(c("1980-01-01", "1990-05-10"))
  )
  expect_error(plot.commune(df_test))
})
