library(mar)
mar <- connect_mar()

##Sækjum veiðigögn
ain_nr <- 277

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
  select(-veidimadur,-heiti_beitu,-staerd_beitu,-athugasemd) %>%
  mutate(veidistadur_numer = if_else(ar < 2006, case_match(veidistadur_numer,3~4, 4~5,5~6,6~7,
                                                           7~10, 8~11,9~12, 10~15,11~16, 12~18,
                                                           13~20,14~21,15~22, 16~26,17~27,
                                                           18~30,19~31,20~32,21~33,22~34,23~35,24~36,
                                                           25~37,26~38,27~39,28~40,
                                                           .default = veidistadur_numer), veidistadur_numer)) |>
  collect(n=Inf) |>
  mutate(fisktegund = recode(fisktegund,Bleiklax="Hnúðlax", .default = fisktegund))->
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
         ar=year(dags),
         man=month(dags),
         vika = week(dags)) %>%
  filter(ar > 2022) %>%
  bind_rows(veidi_allt,.) ->
  veidi_allt

veidi_allt %>% 
  filter(is.na(athugasemd) | !(str_like(athugasemd, '%eldi%') | str_like(athugasemd, '%strokulax%'))) |> #Losna við eldislaxa
  mutate(veidarfaeri = case_when(ar < 1999 & veidarfaeri=="Annað" ~ "Maðkur",
                                 ar < 1999 & veidarfaeri=="Maðkur" ~ "Annað",
                                 .default = veidarfaeri)) %>%
  mutate(Skrad_len = !is.na(lengd_cm)) %>%
  mutate(Skrad_wei = !is.na(thyngd_kg)) %>%
  mutate(Skrad_kyn = kyn != "Óþekkt") %>%
  mutate(Skrad_veidist = !(veidistadur_numer %in% 
                             c(0,100))) |>
  mutate(svaedi = case_when(veidistadur_numer < 12 & Skrad_veidist ~ "Neðan Sundafoss",
                            veidistadur_numer > 11 & Skrad_veidist ~"Ofan Sundafoss",
                            !Skrad_veidist ~ "Óvíst")) |>
  mutate(Len = ifelse( Skrad_len, lengd_cm, weight2len(thyngd_kg))) %>%
  mutate(Wei = ifelse( Skrad_wei, thyngd_kg, len2weight(lengd_cm))) %>%
  mutate(Sjor = ifelse(Len >= 70, "Stórlax", "Smálax")) %>%
  mutate(Sjor = replace_na(Sjor,"Smálax")) %>%
  mutate(Sjor = ifelse(fisktegund == "Lax", Sjor, NA)) ->
  veidi

##Skráningar árin 2002, 2005 og 2010 í veiðibók í ólagi. Fiskum bætt við eftir skýrslu veiðivarðar.
veidi |> 
  filter(fisktegund=="Lax", svaedi=="Ofan Sundafoss", ar > 2015) |>
  mutate(sleppt=recode(sleppt, Óþekkt="Nei")) |> 
  mutate(sleppt = recode(sleppt, Já="Sleppt", Nei="Afli")) |>
  count(ar,Sjor, sleppt) |> 
  pivot_wider(names_from = c(sleppt,Sjor), values_from = n, values_fill = 0) |> 
  mutate(Afli_heild = Afli_Smálax + Afli_Stórlax,
         Sleppt_heild = Sleppt_Smálax + Sleppt_Stórlax) |>
  write_excel_csv(file = "Búðardalsá_veiði_ofan_Sundafoss.csv")
