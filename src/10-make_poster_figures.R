# make_poster_figures

library(tidyverse)
library(stringr)
library(ReporteRs)

dirr::get_rds("data/final")

scores <- ls(pattern = "score_apache2")

apache2_scores <- map(scores, ~mutate(get(.x), score = .x)) %>%
    map(~select(.x, pie.id, score, apache2)) %>%
    map_df(~semi_join(.x, manual_patients, by = "pie.id")) %>%
    arrange(pie.id, score) %>%
    dmap_at("score", str_replace_all, pattern = "score_apache2_", replacement = "") %>%
    mutate(type = "apache2") %>%
    rename(comorbidity = score, risk_score = apache2)

scores <- ls(pattern = "score_apache3")

apache3_scores <- map(scores, ~mutate(get(.x), score = .x)) %>%
    map(~select(.x, pie.id, score, apache3)) %>%
    map_df(~semi_join(.x, manual_patients, by = "pie.id")) %>%
    arrange(pie.id, score) %>%
    dmap_at("score", str_replace_all, pattern = "score_apache3_", replacement = "") %>%
    mutate(type = "apache3") %>%
    rename(comorbidity = score, risk_score = apache3)

scores <- ls(pattern = "score_saps2")

saps2_scores <- map(scores, ~mutate(get(.x), score = .x)) %>%
    map(~select(.x, pie.id, score, saps2)) %>%
    map_df(~semi_join(.x, manual_patients, by = "pie.id")) %>%
    arrange(pie.id, score) %>%
    dmap_at("score", str_replace_all, pattern = "score_saps2_", replacement = "") %>%
    mutate(type = "saps2") %>%
    rename(comorbidity = score, risk_score = saps2)

risk_scores <- bind_rows(apache2_scores, apache3_scores, saps2_scores)

graph <- ggplot(risk_scores, aes(x = comorbidity, y = risk_score)) +
    geom_boxplot() +
    facet_grid(type ~ ., scales = "free_y")

mydoc <- pptx() %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = graph,
            offx = 1,
            offy = 1,
            width = 5.5,
            height = 5*5.5/7,
            vector.graphic = TRUE,
            fontname_sans = "Calibri")

# mydoc <- addSlide(mydoc, slide.layout = "Blank")

writeDoc(mydoc, file = "report/ashp_poster_figures.pptx")
