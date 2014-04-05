library(rgdal)
writeOGR(pm10sel, "PG:dbname=user", "pm10", driver = "PostgreSQL")
writeOGR(co2sp, "PG:dbname=user", "co2", driver = "PostgreSQL")
