library(mar)
library(ggrepel)

default_theme <- theme_set(theme_bw())

svaedi <- tibble(vatnsfall=c("Elliðaár", "LangáSveðja", "Krossá", "Búðardalsá", 
                             "NorðuráGlanni", "Blanda", "Vesturdalsá", "Selá", "Laugardalsá"),
                 svaedi = c("SW","SW","SW","SW","SW","NW","NE","NE","NW"))
landsvaedi <- tibble(landsvaedi_id=c(1:8,NA), svaedi=c("SW","SW","NW","NW","NE","NE","SW","RA","RA"))

gogn <- read_delim("teljaragogn.csv") |>
  left_join(svaedi)

gogn |>
  mutate(fjoldi = ifelse(vatnsfall=="Blanda",fjoldi/.8,fjoldi)) |>
  mutate(sleppt_tvi = sleppt*.75) |>
  mutate(veidi = sleppt_tvi + afli) |>
  mutate(ln=log(fjoldi), lv=log(veidi)) |>
  mutate(p=veidi/fjoldi) |>
  filter(between(ar,2014,2023),!(vatnsfall%in%c("Vesturdalsá","LangáSveðja","NorðuráGlanni"))) |> 
  group_by(ar,svaedi) |>
  summarise(mp = mean(p,na.rm = T), n=n()) |> 
  mutate(mp = ifelse(is.na(mp) | (svaedi=="NE" & ar<2021), 0.5, mp)) |> 
  bind_rows(tibble(svaedi="NE",ar=2023, mp=0.5)) |> 
  bind_rows(tibble(svaedi="RA",ar=2014:2023, mp=0.7)) |> 
  left_join(landsvaedi) |> 
  write_excel_csv("veidihlutfall.csv")

#######################################################################
## Figures
#######################################################################
  
gogn |>
    mutate(fjoldi = ifelse(vatnsfall=="Blanda",fjoldi/.8,fjoldi)) |>
    mutate(sleppt_tvi = sleppt*.75) |>
    mutate(veidi = sleppt_tvi + afli) |>
    mutate(ln=log(fjoldi), lv=log(veidi)) |>
    mutate(p=veidi/fjoldi) |>
    #na.omit() |>
  filter(between(ar,2014,2023),!(vatnsfall%in%c("Vesturdalsá","LangáSveðja","NorðuráGlanni"))) |> 
  ggplot(aes(ar,p, colour = vatnsfall)) +
  stat_summary(fun="mean",aes(ar,p,group = svaedi),inherit.aes = FALSE, geom = "line", linetype=2) +
  stat_summary(fun="mean",aes(ar,p,group = svaedi),inherit.aes = FALSE, geom = "point",shape=4) +
  geom_point() +
  geom_path(linetype = 1) +
  #geom_smooth(se=FALSE) +
  xlim(2014,2023) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  facet_wrap(~svaedi, scales = "free_x", ncol=2) +
  geom_hline(yintercept = 0.5, linetype=3) +
  xlab("") +
  labs(color="")
  

gogn |>
  mutate(fjoldi = ifelse(vatnsfall=="Blanda",fjoldi/.8,fjoldi)) |>
  mutate(sleppt_tvi = sleppt*.75) |>
  mutate(veidi = sleppt_tvi + afli) |>
  mutate(ln=log(fjoldi), lv=log(veidi)) |>
  mutate(p=veidi/fjoldi) |>
  filter(between(ar,2014,2023),vatnsfall!="Vesturdalsá") |> 
  group_by(ar,svaedi) |>
  mutate(mp = mean(p,na.rm = T)) |> 
  mutate(mp = ifelse(is.na(mp) | (svaedi=="NE" & ar<2021), 0.5, mp)) |>
  select(ar, vatnsfall, svaedi,p, mp) |>
  pivot_wider(names_from = vatnsfall, values_from = p) |> View()
  
gogn |>
  mutate(fjoldi = ifelse(vatnsfall=="Blanda",fjoldi/.8,fjoldi)) |>
  mutate(sleppt_tvi = sleppt*.75) |>
  mutate(veidi = sleppt_tvi + afli) |>
  mutate(ln=log(fjoldi), lv=log(veidi)) |>
  mutate(p=veidi/fjoldi) |> 
  na.omit() |>
  filter(!(vatnsfall=="Selá" & ar < 2021)) |>
  ggplot(aes(ln,lv, colour = vatnsfall, label=ar)) +
  geom_point() +
  geom_label_repel() +
  #geom_path(linetype = 2) +
  geom_smooth(se=FALSE, method="lm") +
  facet_wrap(~vatnsfall, scales = "free")
  NULL

gogn |>
  mutate(fjoldi = ifelse(vatnsfall=="Blanda",fjoldi/.8,fjoldi)) |>
  filter(ar > 2013, ar <2024)|>
  mutate(sleppt_tvi = sleppt*.75) |>
  mutate(veidi = sleppt_tvi + afli) |>
  mutate(ln=log(fjoldi), lv=log(veidi)) |>
  mutate(p=veidi/fjoldi) |> 
  na.omit() |>
  filter(!(vatnsfall=="Selá" & ar < 2021)) ->
  data

m1 <- lm(lv ~ ln*factor(ar)+ln*vatnsfall+vatnsfall, data=data)
drop1(m1,test="F")

mx <- lm(lv ~ ln*svaedi+svaedi*factor(ar)+svaedi*vatnsfall, data=data)
drop1(mx,test="F")

m2 <- lm(lv ~ ln+factor(ar)+vatnsfall, data=data)
drop1(m2,test="F")

data$pred <- predict(m2)

data |>
  ggplot(aes(p, exp(pred)/fjoldi, color=vatnsfall, label=ar)) +
  geom_point() +
  geom_label_repel()+
  facet_wrap(~vatnsfall) +
  geom_abline(slope = 1, intercept = 0)
#  geom_path()
  NULL
