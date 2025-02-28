df_test <- data.frame(
  Libelle_departement = rep("Seine-Saint-Denis", 5),
  Libelle_commune = rep("Paris", 5),
  Date_de_naissance = as.Date(c("1980-01-01", "1990-05-10", "2000-07-20", "1975-03-30", "1985-12-15"))
)


test_that("summary.commune retourne une erreur si plusieurs communes sont présentes", {
  df_test <- data.frame(
    Libelle_commune = c("Paris", "Lyon"),
    Date_de_naissance = as.Date(c("1980-01-01", "1990-05-10"))
  )
  expect_error(summary.commune(df_test))
})
