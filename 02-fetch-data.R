library(data.table)
library(RCurl)
library(lubridate)

datasource = 'jhu'
if (datasource == 'datahub') {
    # https://datahub.io/core/covid-19, it's eventually CSSE data...
    data <- fread("https://pkgstore.datahub.io/core/covid-19/time-series-19-covid-combined_csv/data/0e19819c40014068ad1a1de38e3e6573/time-series-19-covid-combined_csv.csv")
    setnames(data, 
             c('Country/Region', 'Province/State', 'Lat', 'Long'), 
             c('CountryName', 'RegionName', 'Latitude', 'Longitude'))
} else if (datasource == 'open-covid-19'){
    data <- fread('https://open-covid-19.github.io/data/data.csv')
    # select regional data
    data <- data[RegionName!='', ]
} else if (datasource == 'jhu') {
    site_link <- paste0("https://raw.githubusercontent.com/",
                        "CSSEGISandData/COVID-19/",
                        "master/csse_covid_19_data/",
                        "csse_covid_19_time_series/"
    )
    confirmed_data <- fread(getURL(paste0(site_link, "time_series_covid19_confirmed_global.csv")))
    recovered_data <- fread(getURL(paste0(site_link, "time_series_covid19_recovered_global.csv")))
    death_data <- fread(getURL(paste0(site_link, "time_series_covid19_deaths_global.csv")))
    
    # remove columns with NA's
    confirmed_data <- confirmed_data[
        , colSums(!is.na(confirmed_data)) == nrow(confirmed_data), with=FALSE]
    recovered_data <- recovered_data[
        , colSums(!is.na(confirmed_data)) == nrow(confirmed_data), with=FALSE]
    death_data <- death_data[
        , colSums(!is.na(confirmed_data)) == nrow(confirmed_data), with=FALSE]
    
    cols <- names(recovered_data)[5:dim(recovered_data)[2]]
    recovered_data[, (cols) := lapply(.SD, as.integer), .SDcols = cols]
    
    confirmed <- melt(
        confirmed_data,
        id=1:4,
        measure=colnames(confirmed_data)[5:dim(confirmed_data)[2]],
        value.factor=TRUE,
        variable.name = "Date",
        value.name = "Num"
    )
    recovered <- melt(
        recovered_data,
        id=1:4,
        measure=colnames(recovered_data)[5:dim(recovered_data)[2]],
        value.factor=TRUE,
        variable.name = "Date",
        value.name = "Num"
    )
    death <- melt(
        death_data,
        id=1:4,
        measure=colnames(death_data)[5:dim(death_data)[2]],
        value.factor=TRUE,
        variable.name = "Date",
        value.name = "Num"
    )
    
    confirmed[, Type:='Confirmed']
    recovered[, Type:='Recovered']
    death[, Type:='Deaths']
    
    data <- rbindlist(list(confirmed, recovered, death), fill = TRUE)
    data[is.na(Num), Num:=0]
    data <- data[`Province/State`!='Recovered']
    data[, Date:=mdy(Date)]
    
    # data[`Country/Region`=='Taiwan*', `Province/State`:='Taiwan']
    # data[`Country/Region` %in% c('Mainland China', 'Taiwan*'), `Country/Region`:='China']
    # data[`Province/State` %in% c('Hong Kong', 'Macau'), `Country/Region`:='China']
    
    # calculate current case: confirmed - death - recovered
    current <- dcast(data, ...~Type, value.var = 'Num')
    current[, `:=`(
        Current=Confirmed - Deaths - Recovered,
        Confirmed=NULL,
        Deaths=NULL,
        Recovered=NULL,
        Type='current'
    )]
    setnames(current, 'Current', 'Num')
    
    data <- rbindlist(list(data, current), fill = TRUE)
    data <- dcast(data, ...~Type, value.var = 'Num')
    
    setnames(data, 
             c('Country/Region', 'Province/State', 'Lat', 'Long'), 
             c('CountryName', 'RegionName', 'Latitude', 'Longitude'))
}

saveRDS(data, here::here('covid-19-data.RDS'))