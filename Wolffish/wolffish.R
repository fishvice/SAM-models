devtools::install_github("fishfollower/SAM/stockassessment")
devtools::install_github("fishvice/husky", dependencies = FALSE) # A temporary measure
devtools::install_github("fishvice/pax", dependencies = FALSE)
library(tidyverse)
library(fjolst)    # for the time being
library(pax)
attach("/net/hafkaldi/export/u2/reikn/R/SurveyWork/SMB/Stations.rdata")


#EXAMPLE 

Station <- 
  STODVAR.all %>% 
  filter(tognumer %in% 1:39) %>% 
  select(id = synis.id, year = ar, towlength = toglengd, strata = oldstrata) %>% 
  mutate(towlength = pax:::trim_towlength(towlength),
         mult = 1) %>% 
  arrange(id)

Stratas <-
  husky::stratas_df %>% 
  select(strata, area = rall.area)

res <- calc_length_indices(Station, Stratas, SPECIES = 1)

tidy_fixed <-
  res$aggr %>% 
  filter(length == 5) %>% 
  mutate(source = "tidy")
p <- 
  tidy_fixed %>% 
  select(year, cb, cb.cv) %>% 
  ggplot(aes(year, cb)) +
  geom_pointrange(aes(ymin = cb * (1 - cb.cv),
                      ymax = cb * (1 + cb.cv)),
                  colour = "red", lwd = 1) +
  geom_line(colour = "red", lwd = 1) +
  scale_colour_brewer(palette = "Set1")

attach("/net/hafkaldi/export/u2/reikn/Splus5/SMB/Allindices.RData")
mri <- 
  All.indices %>% 
  filter(species == 1,
         svaedi == "Heild",
         lengd == 5,
         type == "all") %>% 
  select(year = ar, cb = bio.staerri, cb.cv = cv.bio.staerri, type) %>% 
  mutate(source = "mri")

p +
  geom_pointrange(data = mri, 
                  aes(ymin = cb * (1 - cb.cv),
                      ymax = cb * (1 + cb.cv)),
                  colour = "yellow") +
  geom_line(data = mri, colour = "yellow") +
  labs(x = NULL, y = NULL,
       title = "Biomass indices",
       subtitle = "Comparison of the tidyverse (red) and the Bible (grey)")

tidy_fixed %>% 
  select(year, cb) %>% 
  left_join(mri %>% select(year, cb2 = cb)) %>% 
  mutate(diff = (cb - cb2)/cb2 * 100) %>% 
  summary() #shows 0.2% increase in tidyverse over mri versions

res <- calc_length_indices(Station, Stratas, SPECIES = 48, Sex = 2)
# and a quick plot
res$aggr %>% 
  filter(length == 5) %>% 
  select(year, cb, cb.cv) %>% 
  ggplot(aes(year, cb)) +
  geom_pointrange(aes(ymin = cb * (1 - cb.cv),
                      ymax = cb * (1 + cb.cv)),
                  colour = "red", lwd = 1) +
  geom_line(colour = "red", lwd = 1) +
  scale_colour_brewer(palette = "Set1") +
  labs(x = NULL, y = NULL,
       title = "Grásleppa: Spring survey biomass index",
       subtitle = "All stations")

spe88 <-  calc_length_indices(Station, Stratas,  88, lwcoeff = c(0.01, 3))
spe562 <- calc_length_indices(Station, Stratas, 562, lwcoeff = c(0.01, 3))
spe150 <- calc_length_indices(Station, Stratas, 150, lwcoeff = c(0.01, 3))
spe71 <-  calc_length_indices(Station, Stratas,  71, lwcoeff = c(0.01, 3))
spe88$aggr %>% 
  mutate(Species = "Rauða sævesla") %>% 
  bind_rows(spe562$aggr %>% mutate(Species = "Ljóskjafta")) %>% 
  bind_rows(spe150$aggr %>% mutate(Species = "Svartgóma")) %>% 
  bind_rows(spe71$aggr %>%  mutate(Species = "Ískóð")) %>% 
  filter(length == 140) %>% 
  select(year, Species, cn, cn.cv) %>% 
  ggplot(aes(year, cn)) +
  geom_pointrange(aes(ymin = cn * (1 - cn.cv),
                      ymax = cn * (1 + cn.cv)),
                  colour = "red", lwd = 1) +
  geom_line(colour = "red", lwd = 1) +
  scale_colour_brewer(palette = "Set1") +
  facet_wrap(~ Species, scale = "free_y") +
  labs(x = NULL, y = NULL,
       title = "Spring survey abundance index",
       subtitle = "All stations")

#FALL SURVEY
Stations <- 
  smh_strata_setup() %>% 
  select(id = synis.id, year = ar, towlength = toglengd, strata = newstrata) %>% 
  mutate(towlength = pax:::trim_towlength(towlength),
         mult = 1) %>% 
  arrange(id)

Stratas <-
  husky::newstratas_df %>% 
  select(strata, area = rall.area)
res <- calc_length_indices(Stations, Stratas, SPECIES = 1)
res$aggr %>% 
  filter(length == 5) %>% 
  select(year, cb, cb.cv) %>% 
  ggplot(aes(year, cb)) +
  geom_pointrange(aes(ymin = cb * (1 - cb.cv),
                      ymax = cb * (1 + cb.cv)),
                  colour = "red", lwd = 1) +
  geom_line(colour = "red", lwd = 1) +
  scale_colour_brewer(palette = "Set1") +
  labs(x = NULL, y = NULL,
       title = "Cod: Fall survey biomass index",
       subtitle = "All stations")

#AGE BASED INDICES
# Lest start from scratch
stratas <-
  husky::stratas_df %>% 
  select(strata, area = rall.area)
SPECIES <- 1
lengthclass <- c(seq(4.5, 109.5, by = 5), 119.5, 139.5)
ind <- c(31931, 31932, 32131, 36731, 37031, 37131, 37132, 37231, 41431, 41531, 42231, 42232, 47431, 52331)
st1 <-
  bind_rows(husky::STODVAR) %>%
  filter(tognumer < 20 | index %in% ind) %>%
  mutate(region = ifelse(area %in% c(1, 9, 10), "south",
                         ifelse(area %in% c(2:8), "north", NA))) %>%
  filter(!is.na(region)) %>%
  select(synis.id, ar, towlength = toglengd, region, strata = newstrata)

st2 <-
  bind_rows(husky::STODVAR) %>% 
  filter(area %in% 1:10) %>% 
  bind_rows(lesa.stodvar(leidangur="A4-2001")) %>% 
  mutate(region = ifelse(area %in% c(1, 9, 10), "south",
                         ifelse(area %in% c(2:8), "north", NA))) %>%
  filter(!is.na(region)) %>%
  select(synis.id, ar, towlength = toglengd, region, strata = newstrata)

x <- calc_age_indices(st_length = st1, st_ototliths = st2, species = 1)
x$aggr %>% 
  mutate(n = round(n/1e6, 2)) %>% 
  select(-n.cv) %>% 
  filter(aldur %in% 1:11) %>% # just to fit things on the screen
  spread(aldur, n) %>% 
  as.data.frame()


#BEGIN WOLFFISH