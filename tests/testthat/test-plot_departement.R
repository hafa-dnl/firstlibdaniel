test_that("plot.departement génère un graphique sans erreur", {
  df_test <- data.frame(
    Libelle_departement = c("Seine-Saint-Denis", "Seine-Saint-Denis", "Paris", "Val-de-Marne", "Paris"),
    Date_de_naissance = as.Date(c("1980-01-01", "1990-05-10", "2000-07-20", "1975-03-30", "1985-12-15"))
  )
  expect_silent(plot.departement(df_test))
})

test_that("plot.departement retourne une erreur si les données sont vides", {
  df_test <- data.frame(Libelle_departement = character(0))
  expect_error(plot.departement(df_test))
})


test_that("plot.departement retourne une erreur si plusieurs départements sont présents", {
  df_test <- data.frame(
    Libelle_departement = c("Seine-Saint-Denis", "Val-de-Marne"),
    Date_de_naissance = as.Date(c("1980-01-01", "1990-05-10"))
  )
  expect_error(plot.departement(df_test))
})
