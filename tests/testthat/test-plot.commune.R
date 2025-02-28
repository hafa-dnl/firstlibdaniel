test_that("plot.commune génère un graphique pour une commune valide", {
  # Créer un exemple de dataframe représentant une commune
  data <- data.frame(
    Libelle.de.la.commune = rep("Paris", 6),
    Libelle.du.departement = rep("Paris", 6),
    Code.de.la.categorie.socio.professionnelle = c("A1", "B2", "A1", "C3", "B2", "A1"),
    Code.de.la.commune = rep("75056", 6)  # Ajout de la colonne manquante
  )

  # Ajouter la classe "commune"
  class(data) <- c("commune", class(data))

  # Appeler la fonction plot.commune
  plot <- plot.commune(data)

  # Vérifier que le graphique est un objet ggplot
  expect_s3_class(plot, "gg")

  # Vérifier que le titre du graphique contient le nom de la commune et du département
  expect_true("Paris - Paris" %in% plot$labels$title)

  # Vérifier que les codes professionnels sont présents dans les données
  expect_true(any(grepl("A1", plot$data$Code.de.la.categorie.socio.professionnelle)))
  expect_true(any(grepl("B2", plot$data$Code.de.la.categorie.socio.professionnelle)))
  expect_true(any(grepl("C3", plot$data$Code.de.la.categorie.socio.professionnelle)))
})

test_that("plot.commune génère une erreur si l'objet n'est pas de classe 'commune'", {
  # Créer un dataframe sans ajouter la classe "commune"
  data_invalid <- data.frame(
    Libelle.de.la.commune = rep("Paris", 6),
    Libelle.du.departement = rep("Paris", 6),
    Code.de.la.categorie.socio.professionnelle = c("A1", "B2", "A1", "C3", "B2", "A1")
  )

  # Vérifier que la fonction renvoie une erreur si l'objet n'est pas de classe "commune"
  expect_error(plot.commune(data_invalid))
})
