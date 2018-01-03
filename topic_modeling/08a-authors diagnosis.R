## Why do means do a poor job of separating the authors?  
## In part because of high skew:  
## Authors can have a median close to the extremes, but a mean somewhere in the middle. 

author_diag = docs_df %>%
    select(-keywords) %>%
    unnest() %>%
    group_by(auids) %>%
    summarize(n = n(),
              topic_1.mean = mean(topic_1),
              topic_1.median = median(topic_1),
              topic_1.max = max(topic_1),
              topic_1.min = min(topic_1)) %>%
    ungroup() %>%
    arrange(desc(topic_1.mean)) %>%
    mutate(auids = as.factor(auids)) %>%
    mutate(auids = forcats::fct_inorder(auids))

## Just taking means does a poor job of separating authors
ggplot(author_diag, aes(topic_1.mean)) + geom_density() + geom_rug()

## Because many authors have a wide range of topic_1 values
ggplot(author_diag, aes(auids)) +
    geom_pointrange(aes(y = topic_1.mean, 
                        ymin = topic_1.min, ymax = topic_1.max))

## And so skew:  means > medians when topic_1 close to 0; means < medians when topic_1 close to 1
ggplot(author_diag, aes(topic_1.mean, topic_1.median)) + 
    geom_point() + 
    stat_function(fun = function (x) x)
