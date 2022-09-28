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
    data.2016 <- readr::read_csv(sprintf("output_data/hourly_heat_energy/annual_2016%s.csv",
                                         grid.suf))
    data.2018 <- readr::read_csv(sprintf("output_data/hourly_heat_energy/annual_2018%s.csv",
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
