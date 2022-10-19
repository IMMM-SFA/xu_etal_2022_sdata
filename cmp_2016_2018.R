library("dplyr")

grid.level = "coarse"
if (grid.level == "coarse") {
    grid.suf = ""
} else if (grid.level == "finer"){
    grid.suf = "_finer"
} else if (grid.level == "tract") {
    grid.suf = "_tract"
}

if (grid.level != "finer") {
    data.2016 <- readr::read_csv(sprintf("intermediate_data/hourly_heat_energy/annual_2016%s.csv",
                                         grid.suf))
    data.2018 <- readr::read_csv(sprintf("output_data/hourly_heat_energy/annual_2018%s.csv",
                                         grid.suf))
} else {
    data.2016 <- readr::read_csv(sprintf("intermediate_data/hourly_heat_energy/annual_2016_07%s.csv",
                                         grid.suf))
    data.2018 <- readr::read_csv(sprintf("output_data/hourly_heat_energy/annual_2018%s_07.csv",
                                         grid.suf))
}

annual.total.2016 <- data.2016 %>%
    dplyr::group_by(geoid) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    dplyr::ungroup()

annual.total.2018 <- data.2018 %>%
    dplyr::group_by(geoid) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    dplyr::ungroup()

annual.total.2016 %>%
    dplyr::mutate(year = 2016) %>%
    dplyr::bind_rows(annual.total.2018 %>%
                     dplyr::mutate(year = 2018)) %>%
    dplyr::select(year, geoid, starts_with("energy")) %>%
    tidyr::gather(variable, value, energy.elec:energy.overall) %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = value, group = year)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(.~variable) +
    ggplot2::theme()
ggplot2::ggsave(sprintf("figures/cmp_2016_2018_energy_%s.png", grid.level), width=8, height=6)

annual.total.2016 %>%
    dplyr::mutate(year = 2016) %>%
    dplyr::bind_rows(annual.total.2018 %>%
                     dplyr::mutate(year = 2018)) %>%
    dplyr::select(year, geoid, starts_with("energy")) %>%
    tidyr::gather(variable, value, energy.elec:energy.overall) %>%
    ggplot2::ggplot(ggplot2::aes(x = value, group = year, color = year)) +
    ggplot2::geom_density() +
    ggplot2::facet_wrap(.~variable, ncol = 1) +
    ggplot2::theme()

monthly.total <- data.2016 %>%
    dplyr::mutate(year = 2016) %>%
    dplyr::bind_rows(data.2018 %>%
                     dplyr::mutate(year = 2018)) %>%
    tidyr::separate(timestamp, into = c("month", "suffix"), sep = "/") %>%
    dplyr::group_by(geoid, year, month) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    dplyr::ungroup() %>%
    {.}

weather <- readr::read_csv("intermediate_data/weather_2018.csv") %>%
    dplyr::mutate(year = 2018) %>%
    dplyr::bind_rows(readr::read_csv("intermediate_data/weather_2016.csv") %>%
                     dplyr::mutate(year = 2016)) %>%
    {.}

weather.summary <- weather %>%
    dplyr::group_by(year, month) %>%
    dplyr::summarise_at(vars(DryBulb.C, RelHum.percent, `WindSpd.m/s`), mean) %>%
    dplyr::ungroup()

weather.summary %>%
    tidyr::gather(variable, value, DryBulb.C:`WindSpd.m/s`) %>%
    dplyr::mutate(year = factor(year),
                  month = factor(month)) %>%
    ggplot2::ggplot(ggplot2::aes(x = month, y = value, color = year, group = interaction(variable, year), label = round(value, 1))) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggrepel::geom_text_repel() +
    ggplot2::facet_wrap(.~variable, scales = "free_y", ncol = 1) +
    ggplot2::theme_bw() +
    ggplot2::theme()
ggplot2::ggsave("figures/cmp_2016_2018_weather.png", width = 9, height = 6)

to.plot <- monthly.total %>%
    dplyr::mutate(year = factor(year)) %>%
    dplyr::select(year, month, geoid, energy.overall, emission.overall) %>%
    tidyr::gather(variable, value, energy.overall:emission.overall) %>%
    {.}

to.plot %>%
    ggplot2::ggplot(ggplot2::aes(x = month, y = value, fill = year)) +
    ggplot2::geom_boxplot(outlier.size = 0.5) +
    ggplot2::facet_wrap(.~variable, ncol = 1, scales = "free_y") +
    ggplot2::theme_bw() +
    ggplot2::theme()
ggplot2::ggsave("figures/cmp_2016_2018_energy_heat.png", width = 9, height = 6)


