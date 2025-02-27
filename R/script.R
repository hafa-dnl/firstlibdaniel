# Script pour le projet cours_r_semaine_3
# 1 ----
# Importation du fichier CSV
data_exercice <- read.csv("data/elus-conseillers-municipaux-cm.csv", sep=";", stringsAsFactors = FALSE)

# Vérification des 6 premières lignes du fichier
head(data_exercice)

# Renommer les colonnes du fichier pour faciliter leur utilisation
colnames(data_exercice) <- c("Code_departement", "Libelle_departement", "Code_collectivite_statut", 
                             "Libelle_collectivite_statut", "Code_commune", "Libelle_commune", 
                             "Nom_elu", "Prenom_elu", "Code_sexe", "Date_de_naissance", 
                             "Code_categorie_socio_professionnelle", "Libelle_categorie_socio_professionnelle", 
                             "Date_debut_mandat", "Libelle_fonction", "Date_debut_fonction", "Code_nationalite")



# 2 ----
# Données pour communes : Nantes et Faverelles
df_Nantes <- subset(data_exercice, Libelle_commune == "Nantes")
df_Faverelles <- subset(data_exercice, Libelle_commune == "Faverelles")

# Données pour départements : Loire_Atlantique et Gers
df_Loire_Atlantique <- subset(data_exercice, Libelle_departement == "Loire-Atlantique")
df_Gers <- subset(data_exercice, Libelle_departement == "Gers")



# 3 ----
#fonction compter_nomdre_d_elus()
compter_nombre_d_elus <- function(df) {

  triplet_unique <- paste(df$Nom_elu, df$Prenom_elu, df$Date_de_naissance)
  
  nombre_unique <- length(unique(triplet_unique))
  
  return(nombre_unique)
}

# La fonction sur les différents data.frames
nombre_Nantes <- compter_nombre_d_elus(df_Nantes)
nombre_Faverelles <- compter_nombre_d_elus(df_Faverelles)
nombre_Loire_Atlantique <- compter_nombre_d_elus(df_Loire_Atlantique)
nombre_Gers <- compter_nombre_d_elus(df_Gers)

# Résultats
cat("Nombre d'élus uniques à Nantes:", nombre_Nantes, "\n")
cat("Nombre d'élus uniques à Faverelles:", nombre_Faverelles, "\n")
cat("Nombre d'élus uniques en Loire-Atlantique:", nombre_Loire_Atlantique, "\n")
cat("Nombre d'élus uniques dans le Gers:", nombre_Gers, "\n")



# 4 ----

#Fonction compter_nombre_d_adjoints
compter_nombre_d_adjoints <- function(df) {
  
  adjoints <- grepl("adjoint", df$Libelle_fonction, ignore.case = TRUE)
  
  nombre_adjoints <- sum(adjoints)
  
  return(nombre_adjoints)
}

# Fonction sur les 4 data.frames
nombre_adjoints_Nantes <- compter_nombre_d_adjoints(df_Nantes)
nombre_adjoints_Faverelles <- compter_nombre_d_adjoints(df_Faverelles)
nombre_adjoints_Loire_Atlantique <- compter_nombre_d_adjoints(df_Loire_Atlantique)
nombre_adjoints_Gers <- compter_nombre_d_adjoints(df_Gers)

# Résultats
cat("Nombre d'adjoints à Nantes:", nombre_adjoints_Nantes, "\n")
cat("Nombre d'adjoints à Faverelles:", nombre_adjoints_Faverelles, "\n")
cat("Nombre d'adjoints en Loire-Atlantique:", nombre_adjoints_Loire_Atlantique, "\n")
cat("Nombre d'adjoints dans le Gers:", nombre_adjoints_Gers, "\n")



# 5 ----

#Fonction trouver_l_elu_le_plus_age
trouver_l_elu_le_plus_age <- function(df){
  
  data_exercice$Date_de_naissance
  df$Date_de_naissance <- as.Date(df$Date_de_naissance, format = "%d/%m/%Y")
  
  df[which.min(df$Date_de_naissance),c("Nom_elu", "Prenom_elu", "Date_de_naissance")]
  
}

# Fonction sur les différents data.frames
elu_plus_age_Nantes <- trouver_l_elu_le_plus_age(df_Nantes)
elu_plus_age_Faverelles <- trouver_l_elu_le_plus_age(df_Faverelles)
elu_plus_age_Loire_Atlantique <- trouver_l_elu_le_plus_age(df_Loire_Atlantique)
elu_plus_age_Gers <- trouver_l_elu_le_plus_age(df_Gers)

#Résultats
print(elu_plus_age_Nantes)
print(elu_plus_age_Faverelles)
print(elu_plus_age_Loire_Atlantique)
print(elu_plus_age_Gers)



# 6 ----

#Fonction calcul_distribution
calcul_distribution_age <- function(df) {
  
  df$Date_de_naissance <- as.Date(df$Date_de_naissance, format = "%d/%m/%Y")
  
  df$Age <- as.numeric(difftime(Sys.Date(), df$Date_de_naissance, units = "weeks")) %/% 52.25
  
  quantile(df$Age, probs = c(0, 0.25, 0.5, 0.75, 1))
  
}

# Appliquer la fonction sur les différents data.frames
quantiles_Nantes <- calcul_distribution_age(df_Nantes)
quantiles_Faverelles <- calcul_distribution_age(df_Faverelles)
quantiles_Loire_Atlantique <- calcul_distribution_age(df_Loire_Atlantique)
quantiles_Gers <- calcul_distribution_age(df_Gers)

# Résultats
cat("Quantiles d'âge à Nantes:", quantiles_Nantes, "\n")
cat("Quantiles d'âge à Faverelles:", quantiles_Faverelles, "\n")
cat("Quantiles d'âge en Loire-Atlantique:", quantiles_Loire_Atlantique, "\n")
cat("Quantiles d'âge dans le Gers:", quantiles_Gers, "\n")



# 7 ----

#plot_code_professions
plot_code_professions <- function(df){
  
  Nbre_Maire_ad <- grepl("Maire", df$Libelle_fonction, fixed  = TRUE) 

  df_Maire_ad <- df[Nbre_Maire_ad,]
  
  Nbre_Maire_d <- !grepl("adjoint", df_Maire_ad$Libelle_fonction, fixed  = TRUE)
  
  df_Maire_d <- df_Maire_ad[Nbre_Maire_d,]
  
  Nbre_Maire <- !grepl("délégué", df_Maire_d$Libelle_fonction, fixed  = TRUE)
  
  df_Maire <- df_Maire_d[Nbre_Maire,]
  
  print(
    barplot(
      table(df_Maire$Code_categorie_socio_professionnelle),
      horiz = TRUE,
      col = "steelblue",
      xlab = "frqc d'apparition d'élu de la catégorie socio",
      ylab = "code catégorie socio",
      main = paste("Département:", df[1, 2])
      
    )
  )
  
}

# Appliquer la fonction sur les différents data.frames
plot_code_professions(df_Nantes)
plot_code_professions(df_Faverelles)
plot_code_professions(df_Loire_Atlantique)
plot_code_professions(df_Gers)



# 8 ----

#summary.commune
summary.commune <- function(x){
  
  if (length(table(x$Libelle_commune)) != 1) {
    stop("L'objet doit contenir 1 seule commune")
  }
  
  nom_de_la_commune <- x[1, "Libelle_commune"]
  
  Nbre_elu <- sum(grepl("Maire", x$Libelle_fonction, fixed = TRUE))
  
  nom_vieux <- trouver_l_elu_le_plus_age(x)[1, "Nom_elu"]
  age_vieux <- calcul_distribution_age(x)[5]
  
  distribution_age <- calcul_distribution_age(x)
  
  cat("Nom de la commune: ", nom_de_la_commune, "\n")
  cat("Nombre d'élus: ", Nbre_elu, "\n")
  cat("Distribution des âges: ", "min", distribution_age[1],
      ", 25% à", distribution_age[2],
      ", 50% à", distribution_age[3],
      ", 75% à", distribution_age[4],
      ", 100% à", distribution_age[5], "\n")
  cat("Nom et âge de l'élu le plus âgé: ", nom_vieux, age_vieux, "ans", "\n", "\n")
}

# Affecter la classe 'commune' aux data.frames
class(df_Nantes) <- c("commune", class(df_Nantes))
class(df_Faverelles) <- c("commune", class(df_Faverelles))

# Test
summary.commune(df_Nantes)
summary.commune(df_Faverelles)



# 9 ----

#summary.departement

summary.departement <- function(x){
  
  if (length(table(x$Libelle_commune)) == 1) {
    stop("L'objet doit contenir plus d'une commune")
  }
  
  if (length(table(x$Libelle_departement)) != 1) {
    stop("L'objet doit contenir 1 département unique")
  }
  
  nom_du_departement <- x[1, "Libelle_departement"]
  
  nbre_communes <- length(table(x$Libelle_commune))
  
  Nbre_elu <- sum(grepl("Maire", x$Libelle_fonction, fixed  = TRUE))
  
  vect_distribution_age <- calcul_distribution_age(x)
  
  x$Date_de_naissance <- as.Date(x$Date_de_naissance, format = "%d/%m/%Y")
  x$Age <- as.numeric(difftime(Sys.Date(), x$Date_de_naissance, units = "weeks")) %/% 52.25
  
  jeune <- x[which.min(x$Age), c("Nom_elu", "Age", "Libelle_commune")]
  jeune_nom <- jeune[1, "Nom_elu"]
  jeune_Age <- jeune[1, "Age"]
  jeune_commune <- jeune[1, "Libelle_commune"]
  
  vieux <- x[which.max(x$Age), c("Nom_elu", "Age", "Libelle_commune")]
  vieux_nom <- vieux[1, "Nom_elu"]
  vieux_Age <- vieux[1, "Age"]
  vieux_commune <- vieux[1, "Libelle_commune"]
  
  moyenne_par_commune <- aggregate(Age ~ Libelle_commune, data = x, FUN = mean)
  haute_moy_age <- moyenne_par_commune[which.max(moyenne_par_commune$Age),]
  faible_moy_age <- moyenne_par_commune[which.min(moyenne_par_commune$Age),]
  
  commune_haute <- x[grepl(haute_moy_age[1, "Libelle_commune"], x$Libelle_commune), ]
  distribution_age_haute <- calcul_distribution_age(commune_haute)
  
  commune_faible <- x[grepl(faible_moy_age[1, "Libelle_commune"], x$Libelle_commune), ]
  distribution_age_faible <- calcul_distribution_age(commune_faible)
  
  cat("Nom du département:", nom_du_departement, "\n",
      "Nombre de communes: ", nbre_communes, "\n",
      "Nombre d'élus: ", Nbre_elu, "\n")
  
  cat("Distribution des âges: ", "min", vect_distribution_age[1],
      ", 25% à", vect_distribution_age[2],
      ", 50% à", vect_distribution_age[3],
      ", 75% à", vect_distribution_age[4],
      ", 100% à", vect_distribution_age[5],"\n",
      "Le plus ancien:", vieux_nom, "\n", vieux_Age, "ans", "\n", vieux_commune, "\n",
      "Le plus jeune:", jeune_nom, "\n", jeune_Age, "ans", "\n", jeune_commune, "\n")
  
  cat("Commune avec la plus haute moyenne d'âge: ", haute_moy_age[1, "Libelle_commune"], 
      round(haute_moy_age[1, "Age"]), "ans", "\n",
      "Distribution des âges: ", "min", distribution_age_haute[1],
      ", 25% à", distribution_age_haute[2],
      ", 50% à", distribution_age_haute[3],
      ", 75% à", distribution_age_haute[4],
      ", 100% à", distribution_age_haute[5],"\n")
  
  cat("Commune avec la plus faible moyenne d'âge: ", faible_moy_age[1, "Libelle_commune"], 
      round(faible_moy_age[1, "Age"]), "ans", "\n",
      "Distribution des âges: ", "min", distribution_age_faible[1],
      ", 25% à", distribution_age_faible[2],
      ", 50% à", distribution_age_faible[3],
      ", 75% à", distribution_age_faible[4],
      ", 100% à", distribution_age_faible[5], "\n", "\n")
}


#test
class(df_Loire_Atlantique) <- c("departement", class(df_Loire_Atlantique))
class(df_Gers) <- c("departement", class(df_Gers))

summary.departement(df_Loire_Atlantique)
summary.departement(df_Gers)



# 10 ----

#plot_commune
plot.commune <- function(df){
  
  # Vérifier que le dataframe contient bien 16 colonnes
  if (ncol(df) < 16) {
    stop("Le dataframe doit contenir 16 colonnes")
  }
  
  # Je ne sélectionne que les lignes avec Maire (ex: maire-adjoint, Maire, ...)
  Nbre_Maire_ad <- grepl("Maire", df$Libelle_fonction, fixed = TRUE) 
  
  # Je crée un nouveau data frame qui ne prendra que les occurrences précédentes
  df_Maire_ad <- df[Nbre_Maire_ad, ]
  
  # Sélectionner les Maire sans "adjoint"
  Nbre_Maire_d <- !grepl("adjoint", df_Maire_ad$Libelle_fonction, fixed = TRUE)
  df_Maire_d <- df_Maire_ad[Nbre_Maire_d, ]
  
  # Sélectionner les Maire sans "délégué"
  Nbre_Maire <- !grepl("délégué", df_Maire_d$Libelle_fonction, fixed = TRUE)
  df_Maire <- df_Maire_d[Nbre_Maire, ]
  
  # Afficher un barplot des catégories socio-professionnelles des Maires
  print(
    barplot(
      table(df_Maire$Code_categorie_socio_professionnelle),
      horiz = TRUE,
      col = "lightgreen",
      xlab = "Fréquence d'apparition d'élu de la catégorie socio-professionnelle",
      ylab = "Libellé des codes professionnels pour les élus",
      main = paste("Commune: ", df[1, "Libelle_commune"], "Département:", df[1, "Libelle_departement"])
    )
  )
}


#test
plot.commune(df_Nantes)
plot.commune(df_Faverelles)



# 11 ----

#plot.departement
plot.departement <- function(df){
  
  # Vérification du nombre de colonnes
  if (ncol(df) < 16) {
    stop("Le dataframe doit contenir 16 colonnes")
  }
  
  # Sélectionner les lignes avec Maire (ex: Maire-adjoint, Maire, ...)
  Nbre_Maire_ad <- grepl("Maire", df$Libelle_fonction, fixed = TRUE) 
  # Créer un nouveau dataframe ne contenant que ces lignes
  df_Maire_ad <- df[Nbre_Maire_ad,]
  
  # Sélectionner les Maire (sans les adjoints)
  Nbre_Maire_d <- !grepl("adjoint", df_Maire_ad$Libelle_fonction, fixed = TRUE)
  df_Maire_d <- df_Maire_ad[Nbre_Maire_d,]
  
  # Exclure les Maire délégués
  Nbre_Maire <- !grepl("délégué", df_Maire_d$Libelle_fonction, fixed = TRUE)
  df_Maire <- df_Maire_d[Nbre_Maire,]
  
  # Nombre de communes dans le département
  Nbre_co <- length(unique(df$Libelle_commune))
  
  # Afficher le barplot des 10 catégories socio-professionnelles les plus fréquentes
  print(
    barplot(
      head(table(df_Maire$Code_categorie_socio_professionnelle), n = 10),
      horiz = TRUE,
      col = "orange",
      xlab = "Fréquence d'apparition d'élus dans la catégorie socio-professionnelle",
      ylab = "Les 10 codes professionnels les plus représentés",
      main = paste("Nombre de communes: ", Nbre_co,
                   "Département:", df[1, "Libelle_departement"])
    )
  )
}

#test
plot.departement(df_Loire_Atlantique)
plot.departement(df_Gers)





