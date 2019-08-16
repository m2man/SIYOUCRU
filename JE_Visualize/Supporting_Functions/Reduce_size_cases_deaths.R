# ----- NOTE -----
# Regenerate cases and deaths vaccine and unvaccine scenerio to reduce size of original data
# The original cases and deaths data is too intensive (since it is the burden for each age)
# This script will use the original data and combine into only 2 group: children (age 0 - 14) and adult (15 - 99)
# Since the original data is too intensive --> cannot upload to github --> This script is just for reference
# -------------- #

DataPath <- '/home/duynguyen/DuyNguyen/RProjects/OUCRU JE/Testing Plot/DataBurden/'

Group_Data <- function(x){
    matrix_data <- matrix(0, nrow = nrow(x), ncol = 2 * 66) # 66 years from 1950 to 2015
    for (i in 1 : 66){
        matrix_data[ , (i-1)*2 + 1] <- rowSums(x[, (i-1) * 100 + c(1 : 15)]) # age band of children: 0 --> 14 (col 1 --> 15)
        matrix_data[ , (i-1)*2 + 2] <- rowSums(x[, (i-1) * 100 + c(16 : 100)])
    }
    # matrix_data <- data.frame(matrix_data)
    return(matrix_data)
}

# case_no_vac <- readRDS(paste0(DataPath, 'no_vac_cases_gen.rds'))
# case_no_vac_group <- lapply(case_no_vac, Group_Data)
# saveRDS(case_no_vac_group, 'no_vac_cases_agegroup.rds')

# case_vac <- readRDS(paste0(DataPath, 'vac_cases_gen.rds'))
# case_vac_group <- lapply(case_vac, Group_Data)
# saveRDS(case_vac_group, 'vac_cases_agegroup.rds')

# death_no_vac <- readRDS(paste0(DataPath, 'no_vac_deaths_gen.rds'))
# death_no_vac_group <- lapply(death_no_vac, Group_Data)
# saveRDS(death_no_vac_group, 'no_vac_deaths_agegroup.rds')

death_vac <- readRDS(paste0(DataPath, 'vac_deaths_gen.rds'))
death_vac_group <- lapply(death_vac, Group_Data)
saveRDS(death_vac_group, 'vac_deaths_agegroup.rds')
