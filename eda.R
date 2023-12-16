library(readxl)
library(esquisse)
library(dplyr)

df <- read.csv("~/Downloads/quantified data.csv", header=T)

df <- df %>%
     mutate(`domestic.chicken` = case_when(
          `domestic.chicken` == 0 ~ 'No',
          `domestic.chicken` == 1 ~ 'Yes',
          `domestic.chicken` == 2 ~ 'With Conditions',
          TRUE ~ as.character(`domestic.chicken`)
     ))

# Calculate percentage for 'domestic.chicken'
counts <- df %>% 
     group_by(`domestic.chicken`) %>% 
     summarise(n = n()) %>%
     mutate(Percentage = n / sum(n) * 100)

# Plot bar chart
ggplot(counts, aes(x = `domestic.chicken`, y = Percentage, fill = `domestic.chicken`)) +
     geom_bar(stat = 'identity') +
     geom_text(aes(label = sprintf("%.2f%%", Percentage)), vjust = -0.5,size = 5) +
     labs(title = 'Sell domestic chicken or not',
          x = '',
          y = 'Percentage') +
     theme_classic(base_size = 18) +
     theme(
          title = element_text(size = 20),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)
     )






library(dplyr)
library(ggplot2)

df %>%
 filter(expense >= 100L & expense <= 1070L) %>%
 ggplot() +
 aes(x = frequency, y = expense, colour = rent) +
 geom_jitter(size = 3) +
 scale_color_hue(direction = 1) +
 theme_gray()



library(dplyr)
library(ggplot2)

df %>%
 filter(`monthly expense` >= 48L & `monthly expense` <= 6126L) %>%
 ggplot() +
 aes(x = people, y = `monthly expense`, colour = frequency) +
 geom_jitter(size = 3) +
 scale_color_distiller(palette = "Greens", 
 direction = 1) +
 labs(x = "Household size") +
 theme_bw()

