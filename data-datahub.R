library("jsonlite")

json_file <- 'https://datahub.io/core/covid-19/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# get list of all resources:
print(json_data$resources$name)

# print all tabular data(if exists any)
for(i in 1:length(json_data$resources$datahub$type)){
    if(json_data$resources$datahub$type[i]=='derived/csv'){
        if (json_data$resources$name[i] == 'time-series-19-covid-combined_csv') {
            path_to_file = json_data$resources$path[i]
            data <- fread(path_to_file)
            break
        }
    }
}