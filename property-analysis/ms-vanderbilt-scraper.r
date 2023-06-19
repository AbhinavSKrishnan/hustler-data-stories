library("rvest")
library("readr")

firstpage <- read_html(read_file("firstpage.htm"))

pagenav <- firstpage %>% html_elements("a.ng-binding")

lastpage <- strtoi(html_text(pagenav[length(pagenav)]))
lastpage

resultsraw <- firstpage %>% html_elements("div#mySearchResults")

resultsbetter <- resultsraw %>% html_elements("div.row")

linksraw = resultsbetter %>% html_elements("a")

for (linkraw in linksraw) {
  print(html_attr(linkraw, "href"))
}