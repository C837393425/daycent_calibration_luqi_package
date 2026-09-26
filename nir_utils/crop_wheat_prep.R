# states where most MLRAs grow hard red for winter wheat and where no
# LAI winter wheat should be grown
# in all other states, all MLRAs grow soft red winter wheat
hrw_states = ['TX', 'OK', 'KS', 'NE', 'SD', 'WY', 'CO', 'NM']
hrw_st_str = "'" + "', '".join(hrw_states) + "'"
no_lai_states = ['WA', 'OR', 'ID', 'CA', 'NV', 'AZ', 'UT', 'ND', 'MT']
no_lai_st_str = "'" + "', '".join(no_lai_states) + "'"
# exception MLRAs
nolai_mlra = {'SD': "'53B', '54' , '55B', '56', '58D', '102A'"}
srw_mlra = {'KS': "'112'",
            'OK': "'112'",
            'TX': "'86', '133B'"}
sql = (f'UPDATE {crop_table} '
       f'SET lai_crop = "W3SR" '
       f'WHERE daycent_crop = "W3" '
       f'   AND state_abbr NOT IN({hrw_st_str}) '
       f'   AND state_abbr NOT IN({no_lai_st_str});')
cursor.execute(sql)
for state, mlra in srw_mlra.items():
    sql = (f'UPDATE {crop_table} '
           f'SET lai_crop = "W3SR" '
           f'WHERE daycent_crop = "W3" '
           f'   AND state_abbr = "{state}" ')
    if state in srw_mlra:
           sql += f'AND MLRA1997_NREL IN ({srw_mlra[state]})'
    sql += ';'
    cursor.execute(sql)
for state in hrw_states:
    sql = (f'UPDATE {crop_table} '
           f'SET lai_crop = "W3HR" '
           f'WHERE daycent_crop = "W3" '
           f'   AND state_abbr = "{state}" ')
    if state in nolai_mlra:
        sql += f'AND MLRA1997_NREL NOT IN ({nolai_mlra[state]}) '
    if state in srw_mlra:
        sql += f'AND MLRA1997_NREL NOT IN ({srw_mlra[state]})'
    sql += ';'
    cursor.execute(sql)


## R version of the wheat LAI crop updates above
hrw_states <- c("TX", "OK", "KS", "NE", "SD", "WY", "CO", "NM")
hrw_st_str <- paste(sprintf("'%s'", hrw_states), collapse = ", ")

no_lai_states <- c("WA", "OR", "ID", "CA", "NV", "AZ", "UT", "ND", "MT")
no_lai_st_str <- paste(sprintf("'%s'", no_lai_states), collapse = ", ")

# exception MLRAs
nolai_mlra <- list(
  SD = "'53B', '54', '55B', '56', '58D', '102A'"
)

srw_mlra <- list(
  KS = "'112'",
  OK = "'112'",
  TX = "'86', '133B'"
)

sql <- paste0(
  "UPDATE ", crop_table, " ",
  "SET lai_crop = 'W3SR' ",
  "WHERE daycent_crop = 'W3' ",
  "AND state_abbr NOT IN(", hrw_st_str, ") ",
  "AND state_abbr NOT IN(", no_lai_st_str, ");"
)
DBI::dbExecute(con, sql)

for (state in names(srw_mlra)) {
  sql <- paste0(
    "UPDATE ", crop_table, " ",
    "SET lai_crop = 'W3SR' ",
    "WHERE daycent_crop = 'W3' ",
    "AND state_abbr = '", state, "' "
  )
  if (!is.null(srw_mlra[[state]])) {
    sql <- paste0(sql, "AND MLRA1997_NREL IN (", srw_mlra[[state]], ")")
  }
  sql <- paste0(sql, ";")
  DBI::dbExecute(con, sql)
}

for (state in hrw_states) {
  sql <- paste0(
    "UPDATE ", crop_table, " ",
    "SET lai_crop = 'W3HR' ",
    "WHERE daycent_crop = 'W3' ",
    "AND state_abbr = '", state, "' "
  )
  if (!is.null(nolai_mlra[[state]])) {
    sql <- paste0(sql, "AND MLRA1997_NREL NOT IN (", nolai_mlra[[state]], ") ")
  }
  if (!is.null(srw_mlra[[state]])) {
    sql <- paste0(sql, "AND MLRA1997_NREL NOT IN (", srw_mlra[[state]], ")")
  }
  sql <- paste0(sql, ";")
  DBI::dbExecute(con, sql)
}

# Short summary of assignment logic:
# - W3SR (soft red winter wheat) is assigned to all W3 records in states that are
#   NOT in HRW states and NOT in no-LAI states.
# - W3SR is also explicitly assigned in MLRA exceptions for:
#   KS (MLRA 112), OK (MLRA 112), and TX (MLRA 86, 133B).
# - W3HR (hard red winter wheat) is assigned for HRW states:
#   TX, OK, KS, NE, SD, WY, CO, NM.
# - Within HRW states, these are excluded from W3HR assignment:
#   SD MLRAs 53B, 54, 55B, 56, 58D, 102A (no-LAI exceptions),
#   and SRW exception MLRAs in KS (112), OK (112), TX (86, 133B).

# State-by-state assignment details:
# - W3HR states (default hard red assignment): TX, OK, KS, NE, SD, WY, CO, NM
# - W3SR states from the broad rule: all states except
#   (a) HRW states TX, OK, KS, NE, SD, WY, CO, NM and
#   (b) no-LAI states WA, OR, ID, CA, NV, AZ, UT, ND, MT
# - W3SR MLRA exceptions inside HRW states:
#   KS -> MLRA 112
#   OK -> MLRA 112
#   TX -> MLRAs 86, 133B
# - SD no-LAI exception MLRAs (53B, 54, 55B, 56, 58D, 102A) are excluded from W3HR
#   by this code and are not explicitly reassigned here.
    