## principal components of subject areas
areas = abstracts_df %>%
    filter(year < 2008) %>%
    select(scopus_id, authors, subjects) %>%
    unnest(authors, .drop = FALSE) %>%
    unnest(subjects) %>%
    filter(!duplicated(.)) %>%
    count(auid, subject_area) %>%
    spread(key = subject_area, value = n, fill = 0) %>%
    as.data.frame() %>%
    column_to_rownames(var = 'auid')

fit = prcomp(areas, scale. = TRUE)
plot(fit)

biplot(fit, choices = 1:2)
biplot(fit, choices = 3:4)
biplot(fit, choices = 5:6)

pcs = fit$x %>% 
    as.data.frame() %>%
    rownames_to_column(var = 'auid') %>% 
    as_tibble()

pcs %>%
    left_join(self_classify) %>%
    GGally::ggpairs(aes(color = category), columns = c(1:4+1, 32:34))

## Major areas: BIOC, IMMU, AGRI, MEDI, ENVI
areas %>%
    rownames_to_column(var = 'auid') %>%
    gather(key = category, value = count, AGRI:VETE) %>% 
    group_by(category) %>%
    summarize(tot = sum(count)) %>%
    arrange(desc(tot))
