library(mar)

mar <- connect_mar()
default_theme <- theme_set(theme_bw())

veidihlutfall <- read_csv("veidihlutfall.csv") |>
  select(-n,-svaedi) |>
  mutate(p_endur=0.25) |>
  bind_rows(tibble(ar = rep(1974:2013,9), landsvaedi_id=rep(c(1:8,NA),each=40), 
                        mp=0.5, p_endur = rep(c(rep(0.25,2), rep(0.25,4), rep(0.25,3)),each=40))) |>
  left_join({tbl_mar(mar,"veidibok.landsvaedi") |> 
      select(id,"landsvaedi"=heiti) |>
      collect(n=Inf)}, by=c("landsvaedi_id"="id"))
  
  
veidibok_veidibok(mar) |> 
  filter(vatnsfall_id!=479) |> #Losna við Galtalæk örfáir fiskar 97-2001
  left_join({veidibok_vatnsfall(mar) |> 
      left_join({tbl_mar(mar,"veidibok.landsvaedi") |> 
          select(id,"landsvaedi"=heiti)}, 
          by=c("landsvaedi_id"="id"))}, by=c("vatnsfall_id"="id")) |>
  left_join(tbl_mar(mar,"ops$johannes.VEIDIBOK_TO_WATER"), by=c("vatnsfall_id"="id")) |> 
  left_join({fv_vatn(mar) |>
      select(vatn_nr, adalvatnsfall_nr)}, by=c("water_no"="vatn_nr")) |>
  mutate(adal_nr = case_match(water_no, 530~1189, 819~1189, 1969~1189, 716~716, 2645~2645,
                              1599~1599, 1385~1189, 1629~1189, 2870~1189, 749~1189, 2553~1189,
                              12~1189, 709~1189, 1861~1189, 2591~1568, 2670~2670,
                              .default=adalvatnsfall_nr)) |>
  filter(!is.na(adal_nr)) |>
  filter(water_no != 1859) |> #Losna við Norðlingafljót
  left_join({fv_vatn(mar) |> 
      select(vatn_nafn, vatn_nr) |> 
      rename(adalvatnsfall = vatn_nafn)}, by= c("adal_nr"="vatn_nr")) |>
  filter(fisktegund_id == 91, ar > 1973, ar <2023) |> #select(adalvatnsfall,adal_nr, landsvaedi, landsvaedi_id) |> distinct() |> group_by(adal_nr) |> mutate(k=n())|> collect(n=Inf) |> View()
  replace_na(list(sleppt_kodi=0)) |> 
  group_by(adal_nr) |>
  collect(n=Inf) |>
  mutate(vatnsfall= str_flatten(sort(unique(vatnsfall)), collapse = " "),
         vatnsfall_id= str_flatten(sort(unique(vatnsfall_id)), collapse = " ")) |> 
  ungroup() |>
  count(adal_nr, adalvatnsfall,vatnsfall, vatnsfall_id, landsvaedi, landsvaedi_id, ar, sleppt_kodi) ->
  gamli_grunnur

tbl(mar, sql("select id as fishing_area_id, 
             area_name as veidisvaedi,
             water_name as vatnsfall,
             water_id as water_no
             from freshwater_fishing.fishing_area")) |>
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
  left_join({fv_vatn(mar) |>
      select(vatn_nr, adalvatnsfall_nr)}, by=c("water_no"="vatn_nr")) |>
  mutate(adal_nr = case_match(water_no, 530~1189, 819~1189, 1969~1189, 716~716, 2645~2645,
                              1599~1599, 1385~1189, 1629~1189, 2870~1189, 749~1189, 2553~1189,
                              12~1189, 709~1189, 1861~1189, 2591~1568, 2670~2670, 3093~1599,
                              .default=adalvatnsfall_nr)) |>
  mutate(adal_nr = case_match(vatnsfall, 'Litla Þverá Borgarfirði'~1189,
                              'Selós Svínadal' ~ 1589, 'Þverá Svínadal'~1589,
                              'Stóra Langadalsá Skógarströnd' ~ 1898,
                              'Þverá Þjórsárdal' ~ 2791,
                              .default=adal_nr)) |>
  filter(!is.na(adal_nr)) |>
  filter(water_no != 1859) |> #Losna við Norðlingafljót
  left_join({fv_vatn(mar) |> 
      select(vatn_nafn, vatn_nr) |> 
      rename(adalvatnsfall = vatn_nafn)}, by= c("adal_nr"="vatn_nr")) |>
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
         vika = week(dags)) |>
  filter(fisktegund_id == 210, ar > 2022, ar <2024) |> 
  filter(is.na(athugasemd) | !(str_like(athugasemd, '%eldi%') | str_like(athugasemd, '%strokulax%'))) |> #Losna við eldislaxa
  filter(is.na(athugasemd) | !(str_like(athugasemd, '%hnúð%') | str_like(athugasemd,'%hnuð%'))) |> #Losna við ranga skráningu á hnúðlöxum
  replace_na(list(sleppt_kodi=0)) |> 
  group_by(adal_nr) |> 
  mutate(vatnsfall= str_flatten(sort(unique(paste(vatnsfall,veidisvaedi))), collapse = " "),
         vatnsfall_id= str_flatten(sort(unique(water_no)), collapse = " ")) |> 
  ungroup() |>
  count(adal_nr, adalvatnsfall, ar, vatnsfall,sleppt_kodi) |> 
  left_join({gamli_grunnur|> 
      select(adal_nr, landsvaedi, landsvaedi_id) |> 
      distinct()}, by="adal_nr") |> 
  bind_rows(gamli_grunnur) |>
  left_join(veidihlutfall, by=c("landsvaedi_id","ar","landsvaedi")) |>
  mutate(fjoldi = ifelse(sleppt_kodi==1,n*(1-p_endur),n)) |> 
  mutate(stofn_tmp= fjoldi/mp) |> 
  group_by(adal_nr,ar) |>
  mutate(stofn = sum(stofn_tmp), logN = log(stofn)) |> 
  ungroup(ar) |>
  pivot_wider(names_from = sleppt_kodi, values_from = c(n,fjoldi,stofn_tmp)) |>
  mutate(last10 = ar > 2013) |>
  group_by(adal_nr,last10) |>
  mutate(M10=max(stofn), hraMedal10=mean(stofn), logMedal10=mean(logN), expMedal10=exp(logMedal10)) |> 
  ungroup(last10) |>
  mutate(M=max(stofn), hraMedal=mean(stofn), logMedal=mean(logN), expMedal=exp(logMedal), N_ar=n()) -> #filter(ar>2013) |> filter(!(expMedal10>40 | expMedal>40)) |> View()
  stofnmat

stofnmat |> #151 vatnsföll
  filter(ar>2013) |> #125 vatnsföll. Þarf að vera skráð veiði minnst einu sinni frá 2014
  filter(expMedal10>40 | expMedal>40) |> #90 vatnsföll. 10ára meðaltal eða langtímameðaltal yfir 40
  group_by(adal_nr) |> 
  mutate(vatnsfall= str_flatten(sort(unique(vatnsfall)), collapse = " "),
         vatnsfall_id= str_flatten(sort(unique(vatnsfall_id)), collapse = " ")) |> 
  ungroup() |>
  select(adal_nr, adalvatnsfall,vatnsfall,landsvaedi,landsvaedi_id,vatnsfall_id,M,hraMedal,logMedal,expMedal,M10,hraMedal10,logMedal10,expMedal10,N_ar) |>
  distinct() |> 
  full_join(read_csv("ar.csv"), by = "adal_nr") ->
  ahaetta#filter(expMedal10< 100) |> ggplot(aes(expMedal10, expMedal, label=adalvatnsfall)) + geom_point() + geom_label_repel() + geom_hline(yintercept = 50) + geom_vline(xintercept = 50) + geom_abline(slope=1, intercept=0)

ahaetta |>  
  write_excel_csv("ar2025.csv",na="")

ahaetta |>
  mutate(ath = ifelse(is.na(nafn),"ny","")) |>
  mutate(ath = ifelse(is.na(logMedal10),"ut",ath)) |>
  mutate(nafn = ifelse(is.na(nafn),ATH, nafn)) |>
  select(adal_nr, nafn,V,N,fjarlægð,logMedal10,expMedal10,fjoldi.flokk, ath) |>
  arrange(nafn) |>
  write_excel_csv("ahaetta.csv",na="")
