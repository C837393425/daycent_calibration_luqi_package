## functions
### temperature effect on plant growth function
create_temp_curve <- function(temperature, ppdf1, ppdf2, ppdf3, ppdf4) {
    frac <- (ppdf2 - temperature) / (ppdf2 - ppdf1)
    gpdf <- exp((ppdf3 / ppdf4) * (1 - frac^ppdf4)) * (frac^ppdf3)
    return(gpdf)
}

### temperature unit conversion function
convert_f_to_c <- function(temp_f) {
    temp_c <- (temp_f - 32) * 5 / 9
    return(temp_c)
}