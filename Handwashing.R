#Load in  packages
library(tidyverse)
library(magrittr)
library(rvest)
library(lubridate)
library(ggpomological)
library(extrafont)
library(magick)

#install.packages("vctrs", type = "source")
#URL
webpage <- xml2::read_html("https://en.wikipedia.org/wiki/Historical_mortality_rates_of_puerperal_fever#Monthly_mortality_rates_for_birthgiving_women_1841%E2%80%931849")

#get all the classes with tables
tables <- rvest::html_nodes(webpage, "table")

#get the first table of Vienna Hospital where Handwashing was started
vienna_hospital <- rvest::html_table(tables[grep("first clinic at the Vienna General Hospital 1841–1849",
                          tables,ignore.case = T)],fill = T)[[1]]

#clean the data
vienna_hospital %<>% dplyr::select(-c(1,6)) %>% filter(!grepl('na', Births))

#change datastypes of everything but the "months"
vienna_hospital %<>% dplyr::mutate_at(vars(Births, Deaths,`Rate (%)`), dplyr::funs(as.numeric))

#changing to date is pretty tricky because we have both %B and %b
a <- readr::parse_date(vienna_hospital$Month, "%b %Y") # Produces NA when format is not "%B %Y"
b <- readr::parse_date(vienna_hospital$Month, "%B %Y") # Produces NA when format is not "%d.%m.%Y"

a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks, ignore the warning

vienna_hospital$Month <- lubridate::ymd(a) # Put it back in your dataframe

colnames(vienna_hospital) <- c("Date", "Births", "Deaths", "Rate(%)")

#Apparently, from this date handwashing was made mandatory according to the Wikipedia Page
mandatory_handwashing_date = as.Date('1847-06-01')

#Added a TRUE/FALSE column to monthly called handwashing_started
vienna_hospital <- vienna_hospital%>% mutate(handwashing_started = (Date >= mandatory_handwashing_date))

#plot
line_plot <- ggplot2::ggplot(vienna_hospital, aes(x= Date, y = `Rate(%)`/100, color = handwashing_started))+
            geom_line(show.legend = F, size= 1) +
            labs(x = 'Year', y = '% monthly deaths', title = "Monthly deaths following the mandatory handwashing",
                 subtitle = "at the Vienna Hospital for the years 1841 to 1849",
                 caption = "*arrowhead & annotation were added with paint") +
            scale_y_continuous(label = scales::percent) +
            scale_color_pomological()+ theme(plot.caption = element_text(size=8, face="italic", color="lightpink2"))


#Apply the theme & save
ggpomological::paint_pomological(
                  line_plot + theme_pomological("Harrington", 16),
                  res = 110)%>% 
magick::image_write("handwashing_graph.png")

