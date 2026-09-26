.libPaths('/data/rubelscratch/rubelogle/daycent_calibration/rlib')
files <- list.files('tests', pattern='^test_.*\\.R$', full.names=TRUE)
if (length(files) == 0) {
  cat('NO_TESTS\n')
  quit(status = 0)
}
for (f in files) {
  cat('=== RUN:', f, '\n')
  tryCatch({
    source(f)
    cat('=== PASSED:', f, '\n')
  }, error = function(e) {
    cat('=== FAILED:', f, '\n', conditionMessage(e), '\n')
    quit(status = 2)
  })
}
cat('ALL_TESTS_DONE\n')
