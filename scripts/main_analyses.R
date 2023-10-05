
# - Family Formation and Abortion Attitudes --------------------------------------------------- #
# --------------------------------------------------------------------------------------------- #

# - part 01: load packages and the data ------------------------------------------------------- #

rm(list = ls())
pacman::p_load(haven,
               labelled,
               janitor,
               gssr,
               psych,
               panelr,
               modelsummary,
               tidyverse)

data("gss_panel06_long")
data("gss_panel08_long")
data("gss_panel10_long")

gss_panel06_long <- gss_panel06_long |>
  labelled::remove_labels() |> haven::zap_labels()
gss_panel08_long <- gss_panel08_long |>
  labelled::remove_labels() |> haven::zap_labels()
gss_panel10_long <- gss_panel10_long |>
  labelled::remove_labels() |> haven::zap_labels()

# - part 02: abortion attitudes --------------------------------------------------------------- #

# 06 panel
gss_panel06_long <-
  gss_panel06_long |> mutate(
    abany = recode(abany, "1" = 1, "2" = 0),
    abdefect = recode(abdefect, "1" = 1, "2" = 0),
    abhlth = recode(abhlth, "1" = 1, "2" = 0),
    abnomore = recode(abnomore, "1" = 1, "2" = 0),
    abpoor = recode(abpoor, "1" = 1, "2" = 0),
    abrape = recode(abrape, "1" = 1, "2" = 0),
    absingle = recode(absingle, "1" = 1, "2" = 0)
  )

gss_panel06_long$abscale6 <-
  psych::alpha(gss_panel06_long[, c("abdefect",
                                    "abhlth",
                                    "abnomore",
                                    "abpoor",
                                    "abrape",
                                    "absingle")])$scores |>
  as_vector()
gss_panel06_long$abscale7 <-
  psych::alpha(gss_panel06_long[, c("abdefect",
                                    "abhlth",
                                    "abnomore",
                                    "abpoor",
                                    "abrape",
                                    "absingle",
                                    "abany")])$scores |>
  as_vector()

# 08 panel
gss_panel08_long <-
  gss_panel08_long |> mutate(
    abany = recode(abany, "1" = 1, "2" = 0),
    abdefect = recode(abdefect, "1" = 1, "2" = 0),
    abhlth = recode(abhlth, "1" = 1, "2" = 0),
    abnomore = recode(abnomore, "1" = 1, "2" = 0),
    abpoor = recode(abpoor, "1" = 1, "2" = 0),
    abrape = recode(abrape, "1" = 1, "2" = 0),
    absingle = recode(absingle, "1" = 1, "2" = 0)
  )

gss_panel08_long$abscale6 <-
  psych::alpha(gss_panel08_long[, c("abdefect",
                                    "abhlth",
                                    "abnomore",
                                    "abpoor",
                                    "abrape",
                                    "absingle")])$scores |>
  as_vector()
gss_panel08_long$abscale7 <-
  psych::alpha(gss_panel08_long[, c("abdefect",
                                    "abhlth",
                                    "abnomore",
                                    "abpoor",
                                    "abrape",
                                    "absingle",
                                    "abany")])$scores |>
  as_vector()

# 10 panel
gss_panel10_long <-
  gss_panel10_long |> mutate(
    abany = recode(abany, "1" = 1, "2" = 0),
    abdefect = recode(abdefect, "1" = 1, "2" = 0),
    abhlth = recode(abhlth, "1" = 1, "2" = 0),
    abnomore = recode(abnomore, "1" = 1, "2" = 0),
    abpoor = recode(abpoor, "1" = 1, "2" = 0),
    abrape = recode(abrape, "1" = 1, "2" = 0),
    absingle = recode(absingle, "1" = 1, "2" = 0)
  )

gss_panel10_long$abscale6 <-
  psych::alpha(gss_panel10_long[, c("abdefect",
                                    "abhlth",
                                    "abnomore",
                                    "abpoor",
                                    "abrape",
                                    "absingle")])$scores |>
  as_vector()
gss_panel10_long$abscale7 <-
  psych::alpha(gss_panel10_long[, c("abdefect",
                                    "abhlth",
                                    "abnomore",
                                    "abpoor",
                                    "abrape",
                                    "absingle",
                                    "abany")])$scores |>
  as_vector()

# - part 03: family transitions --------------------------------------------------------------- #

# 06 panel
gss_panel06_long <-
  gss_panel06_long |>
  mutate(everchild = ifelse(childs == 0, 0, 1),
         evermarry = ifelse(marital == 5, 0, 1))

# 08 panel
gss_panel08_long <-
  gss_panel08_long |>
  mutate(everchild = ifelse(childs == 0, 0, 1),
         evermarry = ifelse(marital == 5, 0, 1))

# 10 panel
gss_panel10_long <-
  gss_panel10_long |>
  mutate(everchild = ifelse(childs == 0, 0, 1),
         evermarry = ifelse(marital == 5, 0, 1))

# - part 04: cleaning stuff ------------------------------------------------------------------- #

# clean names
gss_panel06_long <- gss_panel06_long |>
  mutate(firstid = paste0("gss2006", "_", firstid))
gss_panel08_long <- gss_panel08_long |>
  mutate(firstid = paste0("gss2008", "_", firstid))
gss_panel10_long <- gss_panel10_long |>
  mutate(firstid = paste0("gss2010", "_", firstid))

# select for variables
gss_panel06_long <- gss_panel06_long |>
  dplyr::select(
    firstid,
    wave,
    abscale6,
    abscale7,
    everchild,
    evermarry,
    sex,
    abany,
    abdefect,
    abhlth,
    abnomore,
    abpoor,
    abrape,
    absingle
  ) |>
  drop_na() |> group_by(firstid) |> mutate(n = n()) |> filter(n >= 2) |> ungroup()

gss_panel08_long <- gss_panel08_long |>
  dplyr::select(
    firstid,
    wave,
    abscale6,
    abscale7,
    everchild,
    evermarry,
    sex,
    abany,
    abdefect,
    abhlth,
    abnomore,
    abpoor,
    abrape,
    absingle
  ) |>
  drop_na() |> group_by(firstid) |> mutate(n = n()) |> filter(n >= 2) |> ungroup()

gss_panel10_long <- gss_panel10_long |>
  dplyr::select(
    firstid,
    wave,
    abscale6,
    abscale7,
    everchild,
    evermarry,
    sex,
    abany,
    abdefect,
    abhlth,
    abnomore,
    abpoor,
    abrape,
    absingle
  ) |>
  drop_na() |> group_by(firstid) |> mutate(n = n()) |> filter(n >= 2) |> ungroup()

# bind rows together
gss <-
  bind_rows(gss_panel06_long, gss_panel08_long, gss_panel10_long)

# turn the dataframe into a panel data
gss <- panelr::panel_data(gss, id = firstid, wave = wave)

# - part 05: cumulative treatment ------------------------------------------------------------- #

# revise such that there is no dropouts
gss <- gss |>
  mutate(wave = as.integer(wave)) |>
  widen_panel(separator = "_") |>
  mutate(
    # child in wave 2, according to wave 1
    everchild_2 = ifelse(everchild_1 == 1 &
                           everchild_2 == 0, 1, everchild_2),
    # child in wave 3, according to wave 1 and 2
    everchild_3 = ifelse(
      (everchild_1 == 1 |
         everchild_2 == 1) & everchild_3 == 0,
      1,
      everchild_3
    ),
    # marry in wave 2, according to wave 1
    evermarry_2 = ifelse(evermarry_1 == 1 &
                           evermarry_2 == 0, 1, evermarry_2),
    # marry in wave 3, according to wave 1 and 2
    evermarry_3 = ifelse(
      (evermarry_1 == 1 |
         evermarry_2 == 1) & evermarry_3 == 0,
      1,
      evermarry_3
    )
  ) |>
  long_panel(
    prefix = "_",
    begin = 1,
    end = 3,
    id = "firstid",
    wave = "wave"
  )

# - part 06: model analysis ------------------------------------------------------------------- #

# female
m1_f <-
  panelr::wbm(
    abscale7 ~ everchild,
    id = firstid,
    wave = wave,
    use.wave = TRUE,
    wave.factor = TRUE,
    data = gss |> filter(sex == 2),
    model = "within"
  )
m2_f <-
  panelr::wbm(
    abscale7 ~ evermarry,
    id = firstid,
    wave = wave,
    use.wave = TRUE,
    wave.factor = TRUE,
    data = gss |> filter(sex == 2),
    model = "within"
  )
m3_f <-
  panelr::wbm(
    abscale7 ~ everchild + evermarry,
    id = firstid,
    wave = wave,
    use.wave = TRUE,
    wave.factor = TRUE,
    data = gss |> filter(sex == 2),
    model = "within"
  )

# male
m1_m <-
  panelr::wbm(
    abscale7 ~ everchild,
    id = firstid,
    wave = wave,
    use.wave = TRUE,
    wave.factor = TRUE,
    data = gss |> filter(sex == 1),
    model = "within"
  )
m2_m <-
  panelr::wbm(
    abscale7 ~ evermarry,
    id = firstid,
    wave = wave,
    use.wave = TRUE,
    wave.factor = TRUE,
    data = gss |> filter(sex == 1),
    model = "within"
  )
m3_m <-
  panelr::wbm(
    abscale7 ~ everchild + evermarry,
    id = firstid,
    wave = wave,
    use.wave = TRUE,
    wave.factor = TRUE,
    data = gss |> filter(sex == 1),
    model = "within"
  )

saveRDS(m1_f, file = "./models/m1.RDS")
saveRDS(m2_f, file = "./models/m2.RDS")
saveRDS(m3_f, file = "./models/m3.RDS")
saveRDS(m1_m, file = "./models/m4.RDS")
saveRDS(m2_m, file = "./models/m5.RDS")
saveRDS(m3_m, file = "./models/m6.RDS")

# - part 07: alternative specification -------------------------------------------------------- #

m4 <-
  panelr::wbm(
    abscale7 ~ everchild,
    id = firstid,
    wave = wave,
    use.wave = TRUE,
    wave.factor = TRUE,
    data = gss,
    model = "w-b"
  )
m4.int <-
  panelr::wbm(
    abscale7 ~ everchild | sex | everchild * sex,
    id = firstid,
    wave = wave,
    use.wave = TRUE,
    wave.factor = TRUE,
    data = gss,
    model = "w-b"
  )

m5 <-
  panelr::wbm(
    abscale7 ~ evermarry,
    id = firstid,
    wave = wave,
    use.wave = TRUE,
    wave.factor = TRUE,
    data = gss,
    model = "w-b"
  )
m5.int <-
  panelr::wbm(
    abscale7 ~ evermarry | sex | evermarry * sex,
    id = firstid,
    wave = wave,
    use.wave = TRUE,
    wave.factor = TRUE,
    data = gss,
    model = "w-b"
  )

BIC(m4)
BIC(m4.int)
BIC(m5)
BIC(m5.int)

# - part 08: model assumptions ---------------------------------------------------------------- #

gss.wide <- gss |>
  widen_panel(separator = "_") |>
  mutate(abscale7_12_sum = abscale7_1 + abscale7_2)

# selection assumption
assump.1 <- lm(everchild_3 ~ abscale7_2 + abscale7_12_sum,
               data = gss.wide)
assump.2 <- lm(evermarry_3 ~ abscale7_2 + abscale7_12_sum,
               data = gss.wide)

# --------------------------------------------------------------------------------------------- #
