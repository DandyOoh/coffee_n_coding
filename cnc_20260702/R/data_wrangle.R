#' create some metrics from grouped data
#' @param data the target object with relevant data
#' @param group the variable in the data to group by
#' @returns an aggregate dataframe ready for plotting

wrangle_data <- function(data,group){
  
  wrangled <- data |>
    group_by({{ group }}) |>
    summarise(spells = sum(spells),
              los = sum(los),
              patients = sum(patients)) |>
    ungroup() |>
    rename(group = {{ group }}) |>
    mutate(spells_per_pat = round(spells/patients,2),
           avg_los = round(los/spells,2))
  
  return(wrangled)
}