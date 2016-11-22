# make_poster_figures

library(tidyverse)
library(stringr)
library(forcats)
library(broom)
library(ReporteRs)

dirr::get_rds("data/final")

score_type <- list("ahrq" = "AHRQ",
                   "ek" = "Authors",
                   "elixhauser" = "Elixhauser",
                   "manual" = "Manual",
                   "quan" = "Quan",
                   "_drg" = " with DRG")

scores <- ls(pattern = "score_apache2")

apache2_scores <- map(scores, ~mutate(get(.x), score = .x)) %>%
    map(~select(.x, pie.id, score, apache2)) %>%
    map_df(~semi_join(.x, manual_patients, by = "pie.id")) %>%
    arrange(pie.id, score) %>%
    dmap_at("score", str_replace_all, pattern = "score_apache2_", replacement = "") %>%
    dmap_at("score", str_replace_all, pattern = score_type) %>%
    dmap_at("score", factor) %>%
    dmap_at("score", ~ fct_relevel(.x, c("Manual", "Authors", "Authors with DRG"))) %>%
    mutate(type = "apache2") %>%
    rename(comorbidity = score, risk_score = apache2)

g1 <- ggplot(apache2_scores, aes(x = comorbidity, y = risk_score)) +
    geom_boxplot() +
    xlab("Comorbidity Set") +
    ylab("APACHE II Score") +
    ggtitle("Range of APACHE II Scores by Comorbidity Set") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

scatter1 <- apache2_scores %>%
    spread(comorbidity, risk_score) %>%
    rename(authors = `Authors with DRG`, manual = Manual) %>%
    select(authors, manual) %>%
    ggplot(aes(x = authors, y = manual)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm") +
    ggtitle("Comparison of APACHE II Scores") +
    xlab("Score from Author-Defined Set with DRG") +
    ylab("Score from Manual Review of EMR") +
    # labs(caption = "*Author-Defined Comorbidity Set Using DRG Codes") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

apache2_mod <- map(scores, ~mutate(get(.x), score = .x)) %>%
    map(~select(.x, pie.id, score, apache2)) %>%
    map_df(~semi_join(.x, manual_patients, by = "pie.id")) %>%
    arrange(pie.id, score) %>%
    dmap_at("score", str_replace_all, pattern = "score_apache2_", replacement = "") %>%
    spread(score, apache2)

mod_drg1 <- lm(manual ~ ek_drg, apache2_mod)

mod1 <- augment(mod_drg1) %>%
    ggplot(aes(x = .fitted, y = .resid)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle("APACHE II Linear Regression Model") +
    xlab("Fitted values") +
    ylab("Residuals") +
    # labs(caption = "Author-Defined Comorbidity Set Using DRG Codes") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

scores <- ls(pattern = "score_apache3")

apache3_scores <- map(scores, ~mutate(get(.x), score = .x)) %>%
    map(~select(.x, pie.id, score, apache3)) %>%
    map_df(~semi_join(.x, manual_patients, by = "pie.id")) %>%
    arrange(pie.id, score) %>%
    dmap_at("score", str_replace_all, pattern = "score_apache3_", replacement = "") %>%
    dmap_at("score", str_replace_all, pattern = score_type) %>%
    dmap_at("score", factor) %>%
    dmap_at("score", ~ fct_relevel(.x, "Manual")) %>%
    mutate(type = "apache3") %>%
    rename(comorbidity = score, risk_score = apache3)

g2 <- ggplot(apache3_scores, aes(x = comorbidity, y = risk_score)) +
    geom_boxplot() +
    xlab("Comorbidity Set") +
    ylab("APACHE III Score") +
    ggtitle("Range of APACHE III Scores by Comorbidity Set") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

alt_apache3 <- apache3_scores %>%
    filter(comorbidity %in% c("AHRQ", "AHRQ with DRG", "Manual")) %>%
    dmap_at("comorbidity", str_replace_all, pattern = "AHRQ with DRG", replacement = "Sets with DRG") %>%
    dmap_at("comorbidity", str_replace_all, pattern = "AHRQ", replacement = "Sets without DRG")

alt_g2 <- ggplot(alt_apache3, aes(x = comorbidity, y = risk_score)) +
    geom_boxplot() +
    xlab("Comorbidity Set") +
    ylab("APACHE III Score") +
    ggtitle("Range of APACHE III Scores by Comorbidity Set") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

scatter2 <- apache3_scores %>%
    spread(comorbidity, risk_score) %>%
    rename(authors = `Authors with DRG`, manual = Manual) %>%
    select(authors, manual) %>%
    ggplot(aes(x = authors, y = manual)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm") +
    ggtitle("Comparison of APACHE III Scores") +
    xlab("Score from Author-Defined Set with DRG") +
    ylab("Score from Manual Review of EMR") +
    # labs(caption = "*Author-Defined Comorbidity Set Using DRG Codes") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

apache3_mod <- map(scores, ~mutate(get(.x), score = .x)) %>%
    map(~select(.x, pie.id, score, apache3)) %>%
    map_df(~semi_join(.x, manual_patients, by = "pie.id")) %>%
    arrange(pie.id, score) %>%
    dmap_at("score", str_replace_all, pattern = "score_apache3_", replacement = "") %>%
    spread(score, apache3)

mod_drg2 <- lm(manual ~ ek_drg, apache3_mod)

mod2 <- augment(mod_drg2) %>%
    ggplot(aes(x = .fitted, y = .resid)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle("APACHE III Linear Regression Model") +
    xlab("Fitted values") +
    ylab("Residuals") +
    # labs(caption = "Author-Defined Comorbidity Set Using DRG Codes") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

scores <- ls(pattern = "score_saps2")

saps2_scores <- map(scores, ~mutate(get(.x), score = .x)) %>%
    map(~select(.x, pie.id, score, saps2)) %>%
    map_df(~semi_join(.x, manual_patients, by = "pie.id")) %>%
    arrange(pie.id, score) %>%
    dmap_at("score", str_replace_all, pattern = "score_saps2_", replacement = "") %>%
    dmap_at("score", str_replace_all, pattern = score_type) %>%
    mutate(type = "saps2") %>%
    rename(comorbidity = score, risk_score = saps2)

g3 <- ggplot(saps2_scores, aes(x = comorbidity, y = risk_score)) +
    geom_boxplot() +
    xlab("Comorbidity Set") +
    ylab("SAPS II Score") +
    ggtitle("Range of SAPS II Scores by Comorbidity Set") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

alt_saps2 <- saps2_scores %>%
    filter(comorbidity %in% c("AHRQ", "AHRQ with DRG", "Manual")) %>%
    dmap_at("comorbidity", str_replace_all, pattern = "AHRQ with DRG", replacement = "Sets with DRG") %>%
    dmap_at("comorbidity", str_replace_all, pattern = "AHRQ", replacement = "Sets without DRG")

alt_g3 <- ggplot(alt_apache3, aes(x = comorbidity, y = risk_score)) +
    geom_boxplot() +
    xlab("Comorbidity Set") +
    ylab("SAPS II Score") +
    ggtitle("Range of SAPS II Scores by Comorbidity Set") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

scatter3 <- saps2_scores %>%
    spread(comorbidity, risk_score) %>%
    rename(authors = `Authors with DRG`, manual = Manual) %>%
    select(authors, manual) %>%
    ggplot(aes(x = authors, y = manual)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm") +
    ggtitle("Comparison of SAPS II Scores") +
    xlab("Score from Author-Defined Set with DRG") +
    ylab("Score from Manual Review of EMR") +
    # labs(caption = "*Author-Defined Comorbidity Set Using DRG Codes") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

saps2_mod <- map(scores, ~mutate(get(.x), score = .x)) %>%
    map(~select(.x, pie.id, score, saps2)) %>%
    map_df(~semi_join(.x, manual_patients, by = "pie.id")) %>%
    arrange(pie.id, score) %>%
    dmap_at("score", str_replace_all, pattern = "score_saps2_", replacement = "") %>%
    spread(score, saps2)

mod_drg3 <- lm(manual ~ ek_drg, saps2_mod)

mod3 <- augment(mod_drg3) %>%
    ggplot(aes(x = .fitted, y = .resid)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle("SAPS II Linear Regression Model") +
    xlab("Fitted values") +
    ylab("Residuals") +
    # labs(caption = "Author-Defined Comorbidity Set Using DRG Codes") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

mydoc <- pptx() %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = g1,
            offx = 1,
            offy = 1,
            width = 5.5,
            height = 5*5.5/7,
            vector.graphic = TRUE,
            fontname_sans = "Calibri") %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = scatter1,
            offx = 1,
            offy = 1,
            width = 2.75,
            height = 2.75,
            vector.graphic = TRUE,
            fontname_sans = "Calibri") %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = mod1,
            offx = 1,
            offy = 1,
            width = 2.75,
            height = 2.75,
            vector.graphic = TRUE,
            fontname_sans = "Calibri") %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = g2,
            offx = 1,
            offy = 1,
            width = 5.5,
            height = 5*5.5/7,
            vector.graphic = TRUE,
            fontname_sans = "Calibri") %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = alt_g2,
            offx = 1,
            offy = 1,
            width = 2.75,
            height = 5*5.5/7,
            vector.graphic = TRUE,
            fontname_sans = "Calibri") %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = scatter2,
            offx = 1,
            offy = 1,
            width = 2.75,
            height = 2.75,
            vector.graphic = TRUE,
            fontname_sans = "Calibri") %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = mod2,
            offx = 1,
            offy = 1,
            width = 2.75,
            height = 2.75,
            vector.graphic = TRUE,
            fontname_sans = "Calibri") %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = g3,
            offx = 1,
            offy = 1,
            width = 5.5,
            height = 5*5.5/7,
            vector.graphic = TRUE,
            fontname_sans = "Calibri") %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = alt_g3,
            offx = 1,
            offy = 1,
            width = 2.75,
            height = 5*5.5/7,
            vector.graphic = TRUE,
            fontname_sans = "Calibri") %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = scatter3,
            offx = 1,
            offy = 1,
            width = 2.75,
            height = 2.75,
            vector.graphic = TRUE,
            fontname_sans = "Calibri") %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = mod3,
            offx = 1,
            offy = 1,
            width = 2.75,
            height = 2.75,
            vector.graphic = TRUE,
            fontname_sans = "Calibri")

# mydoc <- addSlide(mydoc, slide.layout = "Blank")

writeDoc(mydoc, file = "report/ashp_poster_figures.pptx")
