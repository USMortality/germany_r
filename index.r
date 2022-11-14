url <- paste0(
  "https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/",
  "Sterbefaelle-Lebenserwartung/Tabellen/sonderauswertung-sterbefaelle",
  ".xlsx?__blob=publicationFile"
)
download.file(url, "data/sonderauswertung-sterbefaelle.xlsx")

source("mortality.r")
source("mortality_yearly_cumulative.r")
source("mortality_yearly_pi.r")
source("mortality_yearly_cumulative_bar.r")
source("mortality_yearly_cumulative_bar_ytd.r")
