# Unnest nested tibble
# 
# From fabletools written by Mitchell O'Hara-Wild

# @param .data A dataset containing a listed column of tsibbles
# @param tbl_col The column containing the tibble to be unnested

unnest_tbl <- function(.data, tbl_col, .sep = NULL){
  row_indices <- rep.int(seq_len(NROW(.data)), purrr::map_int(.data[[tbl_col[[1]]]], NROW))
  
  nested_cols <- purrr::map(tbl_col, function(x){
    lst_col <- .data[[x]]
    if(is.data.frame(lst_col[[1]])){
      dplyr::bind_rows(!!!set_names(lst_col, rep(x, length(lst_col))))
    }
    else{
      rlang::list2(!!x := unlist(lst_col))
    }
  })
  
  if(!is.null(.sep)){
    nested_cols <- purrr::map2(
      nested_cols, tbl_col,
      function(x, nm) set_names(x, paste(nm, colnames(x), sep = .sep))
    )
  }
  
  dplyr::bind_cols(
    .data[row_indices, setdiff(names(.data), tbl_col), drop = FALSE], # Parent cols
    !!!nested_cols # Nested cols
  )
}