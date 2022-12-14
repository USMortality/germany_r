for (ag in unique(mortality$age_group)) {
  print(ag)
  ts <- mortality %>%
    filter(age_group == ag) %>%
    tsibble(index = date) %>%
    filter(year <= 2021) %>%
    group_by_key() %>%
    index_by(year) %>%
    summarise(mortality = sum(mortality))

  current_year <- 2019

  ts_last10 <- ts %>%
    filter(year >= current_year - 10) %>%
    filter(year <= current_year)

  png(
    paste0("./out/Germany-cum-pi-", ag, ".png"),
    width = 1200, height = 670, res = 144
  )
  print(
    ts_last10 %>%
      model(
        ets = ETS(box_cox(mortality, 0.3), ),
        arima = ARIMA(log(mortality))
      ) %>%
      forecast(h = 3) %>%
      autoplot(.vars = mortality, level = 99)
      + autolayer(ts, .vars = mortality)
      + ggtitle(
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
      xlab("Year") +
      ylab("Deaths/100k")
  )
  dev.off()
}
