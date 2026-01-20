library(tidyverse)

data <- read_csv("data/gapminder_americas.csv")

pais_analisis <- "Chile"

exp_promedio <- data %>% 
    group_by(country) %>% 
    summarise(exp = mean(lifeExp)) %>% 
    arrange(desc(exp))

top5_y_pais <- exp_promedio %>% 
    slice_max(order_by = exp, n = 5) %>% 
    pull(country) %>% 
    c(pais_analisis) %>% 
    unique()

grafico_5paises <- data %>% 
    filter(country %in% top5_y_pais) %>% 
    ggplot(aes(x = year, y = lifeExp, color = country))+
    geom_line()+
    geom_point()+
    labs(x = "Año", y = "Expectativa Vida", color = "País", 
    title = paste("Comparación de", pais_analisis, "con los 5 países \ncon mayor expectativa de vida en América"))


