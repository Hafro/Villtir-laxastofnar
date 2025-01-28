# Villtir-laxastofnar
Veiðigögn og teljarar

# Skrár
* stofnstaerdar.Rmd
  * Ath dreifingu og staðalfrávik út frá teljaratölum og veiðitölum
  * Input: teljaragogn.csv og sótt í SQL-grunn Hafró
* veidihlutfall.R
  * Nota veiðitölur og teljaragögn til að skoða veiðihlutfall eftir árum og svæðum
  * Input: teljargogn.csv
  * Output: veidihlutfall.csv
* veidi.R
  * Reiknar mat á stofnstærð út frá veiðitölum og veiðihlutfalli
  * Input: veidihlutfall.csv, ar.csv (hnit og upplýsingar um síðasta mat) og veiðigögn í SQL-grunni Hafró
  * Output: ar2025.csv (helstu upplýsingar fyrir hverja á), ahaetta.csv (input í áhættumatslíkan)
* breytingar.Rmd
  * Ár sem eru á mörkunum að teljast nytjastofn
  * Ár sem koma nýjar inn
  * Ár sem detta út
  * Byggir á veidi.R
