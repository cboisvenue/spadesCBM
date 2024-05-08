make_gc_df <- function(gc_hash) {
gc_df <- NULL
for (gcid in ls(gc_hash)){
  gc <- gc_hash[[gcid]]
  for (age in ls(gc)){
    row <- gc[[age]]
    row <- c(as.integer(gcid), as.integer(age), row)
    gc_df <- rbind(gc_df, row)
  }
}
gc_df <- data.table(gc_df)
rownames(gc_df) <- NULL
colnames(gc_df) <- c(
  "gcid",
  "age",
  "sw_merch_inc",
  "sw_foliage_inc",
  "sw_other_inc",
  "hw_merch_inc",
  "hw_foliage_inc",
  "hw_other_inc"
)

# map the gcids to the correct values corresponding to the inventory
gc_df$gcid <- structure(
  gc_df$gcid,
  levels = c("49", "50", "52", "58", "61"),
  class = "factor"
)
gc_df$gcid <- as.integer(as.character(gc_df$gcid))
return(gc_df)
}
