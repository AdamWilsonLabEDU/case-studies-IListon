#install.packages("gapminder")

#load packages
library(ggplot2)
library(gapminder)
library(dplyr)


#remove Kuwait from data set
gapminder_filtered <- gapminder %>% 
                        filter(country != "Kuwait")
#plot 1

plot_1 <- ggplot(gapminder_filtered, 
            aes(x = lifeExp, y = gdpPercap, 
            color = continent, size = pop/100000))+
            geom_point()+
            scale_y_continuous(trans = "sqrt")+
            facet_wrap(~year, nrow = 1)+
            theme_bw()+
            labs(title = "Wealth and life expectancy through time",
              x = "Life Expectancy", y = "GDP per capita", 
              size = "Population (100k)",
              color = "Continent")

plot_1

#prepare data for plot 2

gapminder_continent <- gapminder_filtered %>%
                          group_by(year, continent) %>%
                          summarize(gdpPercapweighted = weighted.mean(x = gdpPercap, w = pop),
                                    pop = sum(as.numeric(pop)))

#plot 2

plot_2 <- ggplot()+
            geom_line(data = gapminder_filtered,
                      aes(x = year, y = gdpPercap, 
                          color = continent, group = country))+
            geom_point(data = gapminder_filtered, 
                       aes(x = year, y = gdpPercap, 
                           color = continent))+
            geom_line(data = gapminder_continent, 
                      aes(x = year, y = gdpPercapweighted), color = "black")+
            geom_point(data = gapminder_continent,
                        aes(x = year, y = gdpPercapweighted, size = pop/100000),
                            color = "black")+
            facet_wrap(~continent, nrow = 1)+
            theme_bw()+
            labs(title = "Wealth through time, shown by year and continent",
                       x = "Year", y = "GDP per capita",
                       size = "Population (100k)",
                       color = "Continent")

plot_2  

#save plots

ggsave("Plot_1.png", plot = plot_1, width = 15, height = 5, units = "in")

ggsave("Plot_2.png", plot = plot_2, width = 15, height = 5, units = "in")
