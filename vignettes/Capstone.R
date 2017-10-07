## ------------------------------------------------------------------------
eq_clean_data('../R/signif.txt')

## ------------------------------------------------------------------------
eq_loc_helper("co:lon")

## ------------------------------------------------------------------------
eq_location_clean(eq_clean_data('../R/signif.txt'))

## ------------------------------------------------------------------------
ggplot(data = filter(eq_location_clean(eq_clean_data('../R/signif.txt')), COUNTRY == "GERMANY"), aes(x = date, y = COUNTRY, color = DEATHS, size = EQ_PRIMARY)) + geom_timeline(alpha = 0.2)

