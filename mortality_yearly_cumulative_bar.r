for (ag in unique(mortality$age_group)) {
  print(ag)
  ts <- mortality %>%
    filter(age_group == ag) %>%
    tsibble(index = date)

  # Mortality
  data <- ts %>%
    filter(year <= 2021) %>%
    group_by_key() %>%
    index_by(year) %>%
    summarise(mortality = sum(mortality))

  png(
    paste0("./out/Germany-cum-bar-", ag, ".png"),
    width = 1200, height = 670, res = 144
  )
  print(
    ggplot(data, aes(x = year, y = mortality)) +
      ggtitle(
        paste0(
          "Yearly Mortality (",
          sapply(ag, URLdecode, USE.NAMES = FALSE),
          ") [Germany]"
        ),
        paste0(
          "Made by @USMortality; ",
          "Datasource: Destatis"
        )
      ) +
      geom_col(fill = "#5383EC") +
      geom_text(
        aes(label = round(mortality)),
        vjust = 2.5, colour = "white", size = 3
      ) +
      xlab("Year") +
      ylab("Deaths/100k")
  )
  dev.off()
}
