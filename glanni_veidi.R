library(mar)
mar <- connect_mar()

##Sækjum veiðigögn frá 2002
ain_nr <- 1861

##samband lengdar og þyngdar úr skýrslu frá Guðna
len2weight <- function(len, a = .0000215, b = 2.83307){
  w <- a*len^b
  w
}

weight2len <- function(w, a = .0000215, b = 2.83307){
  d <- 1/b
  c <- (1/a)^d
  l <- c*w^d
  l
}

##Sería yfir veiði
veidibok_veidibok(mar) %>%
  left_join(tbl_mar(mar,"ops$johannes.VEIDIBOK_TO_WATER"), by=c("vatnsfall_id"="id")) |>
  filter(water_no==ain_nr) |>   
  filter(ar < 2023) |>
  #filter(vatnsfall== ain) %>%
  collect(n=Inf) ->
  veidi_allt

tbl(mar, sql(paste("select id as fishing_area_id, 
             area_name as veidisvaedi,
             water_name as vatnsfall,
             water_id as water_no
             from freshwater_fishing.fishing_area
             where water_id =", ain_nr))) |>
  left_join(tbl(mar, sql("select id as fishing_spot_id, 
                         fishing_area_id,
                         spot_number as veidistadur_numer,
                         name as veidistadur
                         from freshwater_fishing.fishing_spot"))) |>
  left_join(tbl(mar, sql("select id, 
                         fishing_spot_id, 
                         gender as kyn,
                         local_zone as stada,
                         released as sleppt_kodi,
                         weight as thyngd_kg,
                         length as lengd_cm,
                         name as veidimadur,
                         catch_date as dags,
                         bait_name as heiti_beitu,
                         bait_size as staerd_beitu,
                         catch_comment as athugasemd,
                         species_id as fisktegund_id,
                         icelandic_name as fisktegund,
                         gear as veidarfaeri
                         from freshwater_fishing.catch"))) |>
  select_all(tolower) |>
  rename(veidisvaedi_id = fishing_area_id, veidistadur_id=fishing_spot_id) |>
  collect(n=Inf) |> 
  mutate(veidistadur_id = as.numeric(veidisvaedi_id),
         veidisvaedi_id = as.numeric(veidisvaedi_id),
         veidistadur_numer = as.numeric(veidistadur_numer),
         id = as.numeric(id),
         kyn_kodi = recode(kyn, FEMALE = 2, MALE = 1),
         kyn = recode(kyn, FEMALE = "Hrygna", MALE = "Hængur", UNKNOWN = "Óþekkt"),
         stada_kodi = recode(stada, FROM_SEA = 2, LOCAL=1),
         stada = recode(stada, FROM_SEA = "Sjógenginn", LOCAL="Staðbundinn", NOT_KNOWN = "Óþekkt"),
         sleppt = recode(sleppt_kodi, `0` = "Nei", `1` = "Já"),
         veidarfaeri = recode(veidarfaeri, FLUGA = "Fluga"),
         veidarfaeri = ifelse(heiti_beitu=="Maðkur", "Maðkur", veidarfaeri),
         ar=year(dags),
         man=month(dags),
         vika = week(dags)) %>%
  filter(ar > 2022) %>%
  bind_rows(veidi_allt,.) ->
  veidi_allt

veidi_allt %>% 
  mutate(veidistadur_numer = if_else(ar>=2020, case_when(veidistadur_numer == 82 ~ 83,
                                                         veidistadur_numer == 183 ~ 182,
                                                         veidistadur_numer == 8201 ~82,
                                                         veidistadur_numer == 18301 ~183,
                                                         TRUE ~ veidistadur_numer),
                                     veidistadur_numer)) %>%
  mutate(svaedi=cut_interval(veidistadur_numer,n=3,breaks=c(-Inf,0,99,Inf)
                             ,labels=c("Óþekkt","Neðan Glanna","Ofan Glanna"), right=TRUE)) %>%
  mutate(svaedi = ifelse(veidisvaedi %in% c("2. Svæði 2", "Norðurá II") & yday(dags) > 186 & svaedi =="Óþekkt", "Ofan Glanna", as.character(svaedi))) %>%
  mutate(svaedi = ifelse(veidisvaedi %in% c("2. Svæði 2", "Norðurá II") & yday(dags) <= 186 & svaedi =="Óþekkt", "Neðan Glanna", as.character(svaedi))) %>%
  mutate(svaedi = ifelse(veidisvaedi %in% c("4. Stekkur") & svaedi =="Óþekkt", "Neðan Glanna", as.character(svaedi))) %>%
  mutate(Skrad_len = !is.na(lengd_cm)) %>%
  mutate(Skrad_wei = !is.na(thyngd_kg)) %>%
  mutate(Skrad_kyn = kyn != "Óþekkt") %>%
  mutate(Len = ifelse( Skrad_len, lengd_cm, weight2len(thyngd_kg))) %>%
  mutate(Wei = ifelse( Skrad_wei, thyngd_kg, len2weight(lengd_cm))) %>%
  mutate(Sjor = ifelse(Len >= 70, "Stórlax", "Smálax")) %>%
  mutate(Sjor = replace_na(Sjor,"Smálax")) %>% # Gerum ráð fyrir að óskráð sé smálax
  mutate(Sjor = ifelse(fisktegund == "Lax", Sjor, NA)) ->
  veidi

##Skráningar árin 2002, 2005 og 2010 í veiðibók í ólagi. Fiskum bætt við eftir skýrslu veiðivarðar.
nadd <- tibble(ar=c(2002,2005,2010), kyn="Óþekkt", Sjor="Smálax", sleppt="Afli", nadd=c(125,139,846))
veidi |> 
  filter(fisktegund=="Lax", svaedi=="Ofan Glanna", ar > 2001) |> 
  mutate(sleppt=recode(sleppt, Óþekkt="Nei")) |> 
  mutate(sleppt = recode(sleppt, Já="Sleppt", Nei="Afli")) |>
  count(ar,Sjor, sleppt) |> 
  left_join(select(nadd,-kyn)) |> 
  mutate(n=ifelse(is.na(nadd),n,n+nadd)) |> 
  select(-nadd) |>
  pivot_wider(names_from = c(sleppt, Sjor), values_from = n, values_fill = 0) |> 
  mutate(Afli_heild = Afli_Smálax + Afli_Stórlax,
         Sleppt_heild = Sleppt_Smálax + Sleppt_Stórlax) |>
  left_join(select(nadd,ar,nadd)) |>
  write_excel_csv(file = "Norðurá_veiði_ofan_Glanna.csv")
