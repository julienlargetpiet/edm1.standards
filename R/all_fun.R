#' better_unique
#'
#' Returns the element that are not unique from the input vector
#'
#' @param inpt_v  is the input vector containing the elements
#' @param occu is a parameter that specifies the occurence of the elements that must be returned, defaults to ">-1-" it means that the function will return all the elements that are present more than one time in inpt_v. The synthax is the following "comparaison_type-actual_value-". The comparaison type may be "==" or ">" or "<". Occu can also be a vector containing all the occurence that must have the elements to be returned.
#' @examples
#'
#' print(better_unique(inpt_v=c("oui", "oui", "non", "non", "peut", "peut1", "non")))
#'
#' #[1] "oui" "non"
#'
#' print(better_unique(inpt_v=c("oui", "oui", "non", "non", "peut", "peut1", "non"), occu="==-2-"))
#'
#' #[1] "oui"
#'
#' print(better_unique(inpt_v=c("oui", "oui", "non", "non", "peut", "peut1", "non"), occu=">-2-"))
#'
#' #[1] "non"
#' 
#' print(better_unique(inpt_v=c("oui", "oui", "non", "non", "peut", "peut1", "non"), occu=c(1, 3)))
#' 
#' #[1] "non"   "peut"  "peut1"
#'
#' print(better_unique(inpt_v = c("a", "b", "c", "c"), occu = "==-1-"))
#' 
#' [1] "a" "b"
#'
#' print(better_unique(inpt_v = c("a", "b", "c", "c"), occu = "<-2-"))
#'
#' [1] "a" "b"
#'
#' @export

better_unique <- function(inpt_v, occu=">-1-"){

   rtn_v <- c()

   if (typeof(occu) == "character"){

           pre_vec <- str_locate(occu, "-(.*?)-")

           occu_v <- unlist(strsplit(occu, split=""))

           max_val <- as.numeric(occu_v[(pre_vec[1]+1):(pre_vec[length(pre_vec)]-1)])

           comp_ <- paste(occu_v[1:(pre_vec[1] - 1)], collapse="")

           if (comp_ == "=="){

                for (el in unique(inpt_v)){ if (sum(inpt_v == el) == max_val) { rtn_v <- c(rtn_v, el) } }

           }

           if (comp_ == ">"){

                for (el in unique(inpt_v)){ if (sum(inpt_v == el) > max_val) { rtn_v <- c(rtn_v, el) } }

           }

           if (comp_ == "<"){

                for (el in unique(inpt_v)){ if (sum(inpt_v == el) < max_val) { rtn_v <- c(rtn_v, el) } }

           }

   }else{

          for (el in unique(inpt_v)){ if (sum(inpt_v == el) %in% occu) { rtn_v <- c(rtn_v, el) } }

   }

   return(rtn_v)

}

#' better_match
#' 
#' Allow to get the nth element matched in a vector
#' 
#' @param inpt_v is the input vector 
#' @param ptrn is the pattern to be matched
#' @param untl is the maximum number of matched pattern outputed
#' @param nvr_here is a value you are sure is not present in inpt_v
#' @examples
#'
#' print(better_match(inpt_v=c(1:12, 3, 4, 33, 3), ptrn=3, untl=1))
#'
#' #[1] 3
#'
#' print(better_match(inpt_v=c(1:12, 3, 4, 33, 3), ptrn=3, untl=5))
#'  
#' #[1]  3 13 16
#'
#' print(better_match(inpt_v=c(1:12, 3, 4, 33, 3), ptrn=c(3, 4), untl=5))
#'  
#' [1]  3 13 16  4 14
#' 
#' print(better_match(inpt_v=c(1:12, 3, 4, 33, 3), ptrn=c(3, 4), untl=c(1, 5)))
#'
#' [1]  3  4 14
#' 
#' @export

better_match <- function(inpt_v=c(), ptrn, untl=1, nvr_here=NA){
  Rtn_v <- c()
  if (length(untl) < length(ptrn)){
    val_add <- untl[length(untl)]
    while (length(untl) < length(ptrn)){
      untl <- c(untl, val_add)
    }
  }
  if (!(is.na(match(x = "max", table = untl)))){
    untl[untl == "max"] <- length(inpt_v)
    untl <- as.numeric(untl)
  }
  for (cur_ptrn in 1:length(ptrn)){
    rtn_v <- c()
    cnt = 1
    stop <- FALSE
    while (length(rtn_v) < untl[cur_ptrn] & cnt < (length(inpt_v) + 1) & !(stop)){
            pre_match <- match(x=ptrn[cur_ptrn], table=inpt_v)
            if (!(is.na(pre_match))){
              inpt_v[pre_match] <- nvr_here
              rtn_v <- c(rtn_v, pre_match)
            }else{
              stop <- TRUE
            }
            cnt = cnt + 1
    }
    Rtn_v <- c(Rtn_v, rtn_v)
  }
  return(Rtn_v)
}

#' better_split
#'
#' Allows to split a string by multiple split, returns a vector and not a list.
#' @param inpt is the input character
#' @param split_v is the vector containing the splits
#' @examples
#'
#' print(better_split(inpt = "o-u_i", split_v = c("-")))
#' 
#' [1] "o"   "u_i"
#'
#' print(better_split(inpt = "o-u_i", split_v = c("-", "_")))
#'
#' [1] "o" "u" "i"
#'
#' @export

better_split <- function(inpt, split_v = c()){
  for (split in split_v){
    pre_inpt <- inpt
    inpt <- c()
    for (el in pre_inpt){
      inpt <- c(inpt, unlist(strsplit(x = el, split = split)))
    }
  }
  return(inpt)
}

#' match_by
#'
#' Allow to match elements by ids, see examples.  
#'
#' @param to_match_v is the vector containing all the elements to match
#' @param inpt_v is the input vector containong all the elements that could contains the elements to match. Each elements is linked to an element from inpt_ids at any given index, see examples. So inpt_v and inpt_ids must be the same size
#' @param inpt_ids is the vector containing all the ids for the elements in inpt_v. An element is linked to the id x is both are at the same index. So inpt_v and inpt_ids must be the same size 
#' @examples
#'
#' print(match_by(to_match_v = c("a"), inpt_v = c("a", "z", "a", "p", "p", "e", "e", "a"), 
#'                inpt_ids = c(1, 1, 1, 2, 2, 3, 3, 3)))
#' 
#' [1] 1 8
#'
#' print(match_by(to_match_v = c("a"), inpt_v = c("a", "z", "a", "a", "p", "e", "e", "a"), 
#'                inpt_ids = c(1, 1, 1, 2, 2, 3, 3, 3)))
#'
#' [1] 1 4 8
#'
#' print(match_by(to_match_v = c("a", "e"), inpt_v = c("a", "z", "a", "a", "p", "e", "e", "a"), 
#'                inpt_ids = c(1, 1, 1, 2, 2, 3, 3, 3)))
#' 
#' [1] 1 4 8 6
#'
#' @export

match_by <- function(to_match_v = c(), inpt_v = c(), inpt_ids = c()){
  rtn_v <- c()
  for (el in to_match_v){
    for (id in unique(inpt_ids)){
      if (!(is.na(match(x = el, table = inpt_v[grep(pattern = id, x = inpt_ids)])))){
        rtn_v <- c(rtn_v, (match(x = id, table = inpt_ids) +
                  match(x = el, table = inpt_v[grep(pattern = id, x = inpt_ids)]) - 1))
      }
    }
  }
  return(rtn_v)
}

#' grep_all 
#'
#' Allow to perform a grep function on multiple input elements
#'
#' @param inpt_v is the input vectors to grep elements from
#' @param pattern_v is a vector contaning the patterns to grep
#' @examples
#'
#' print(grep_all(inpt_v = c(1:14, "z", 1:7, "z", "a", "z"), 
#'                pattern_v = c("z", "4")))
#'
#' [1] 15 23 25  4 14 19
#'
#' print(grep_all(inpt_v = c(1:14, "z", 1:7, "z", "a", "z"), 
#'                pattern_v = c("z", "^4$")))
#'
#' [1] 15 23 25  4 19
#'
#' print(grep_all(inpt_v = c(1:14, "z", 1:7, "z", "a", "z"), 
#'                pattern_v = c("z")))
#' 
#' [1] 15 23 25
#'
#' @export

grep_all <- function(inpt_v, pattern_v){
  rtn_v <- c(grep(pattern = pattern_v[1], x = inpt_v))
  if (length(pattern_v) > 1){
    pattern_v <- pattern_v[2:length(pattern_v)]
    for (ptrn in pattern_v){
      rtn_v <- c(rtn_v, grep(pattern = ptrn, x = inpt_v))
    }
  }
  return(rtn_v)
}

#' sub_mult
#'
#' Performs a sub operation with n patterns and replacements.
#'
#' @param inpt_v is a vector containing all the elements that contains expressions to be substituted
#' @param pattern_v is a vector containing all the patterns to be substituted in any elements of inpt_v
#' @param replacement_v is a vector containing the expression that are going to substituate those provided by pattern_v
#' @examples
#'
#' print(sub_mult(inpt_v = c("X and Y programming languages are great", "More X, more X!"), 
#'                pattern_v = c("X", "Y", "Z"), 
#'                replacement_v = c("C", "R", "GO")))
#'
#' [1] "C and R programming languages are great"
#' [2] "More C, more X!"
#'
#' @export

sub_mult <- function(inpt_v, pattern_v = c(), replacement_v = c()){
  for (i in 1:length(inpt_v)){
    cur_char <- inpt_v[i]
    for (i2 in 1:length(pattern_v)){
      cur_char <- sub(pattern = pattern_v[i2], replacement = replacement_v[i2], x = cur_char)
    }
    inpt_v[i] <- cur_char
  }
  return(inpt_v)
}

#' gsub_mult
#'
#' Performs a gsub operation with n patterns and replacements.
#'
#' @param inpt_v is a vector containing all the elements that contains expressions to be substituted
#' @param pattern_v is a vector containing all the patterns to be substituted in any elements of inpt_v
#' @param replacement_v is a vector containing the expression that are going to substituate those provided by pattern_v
#' @examples
#'
#' print(gsub_mult(inpt_v = c("X and Y programming languages are great", "More X, more X!"), 
#'                pattern_v = c("X", "Y", "Z"), 
#'                replacement_v = c("C", "R", "GO")))
#' [1] "C and R programming languages are great"
#' [2] "More C, more C!"                        
#'
#' @export

gsub_mult <- function(inpt_v, pattern_v = c(), replacement_v = c()){
  for (i in 1:length(inpt_v)){
    cur_char <- inpt_v[i]
    for (i2 in 1:length(pattern_v)){
      cur_char <- gsub(pattern = pattern_v[i2], replacement = replacement_v[i2], x = cur_char)
    }
    inpt_v[i] <- cur_char
  }
  return(inpt_v)
}

#' better_sub
#'
#' Allow to perform a sub operation to a given number of matched patterns, see examples
#'
#'
#' @param inpt_v is a vector containing all the elements that contains expressions to be substituted
#' @param pattern is the expression that will be substituted
#' @param replacement is the expression that will substituate pattern
#' @param untl_v is a vector containing, for each element of inpt_v, the number of pattern that will be substituted
#' @examples
#'
#' print(better_sub(inpt_v = c("yes NAME, i will call NAME and NAME", 
#'                             "yes NAME, i will call NAME and NAME"),
#'                  pattern = "NAME",
#'                  replacement = "Kevin",
#'                  untl = c(2)))
#' 
#' [1] "yes Kevin, i will call Kevin and NAME"
#' [2] "yes Kevin, i will call Kevin and NAME"
#'
#' print(better_sub(inpt_v = c("yes NAME, i will call NAME and NAME", 
#'                             "yes NAME, i will call NAME and NAME"),
#'                  pattern = "NAME",
#'                  replacement = "Kevin",
#'                  untl = c(2, 3)))
#' 
#' [1] "yes Kevin, i will call Kevin and NAME" 
#' [2] "yes Kevin, i will call Kevin and Kevin"
#'
#' print(better_sub(inpt_v = c("yes NAME, i will call NAME and NAME", 
#'                              "yes NAME, i will call NAME and NAME"),
#'                   pattern = "NAME",
#'                   replacement = "Kevin",
#'                   untl = c("max", 3)))
#' 
#' [1] "yes Kevin, i will call Kevin and Kevin"
#' [2] "yes Kevin, i will call Kevin and Kevin"
#'
#' @export

better_sub <- function(inpt_v = c(), pattern, replacement, untl_v = c()){
  if (length(untl_v) < length(inpt_v)){
    val_add <- untl_v[length(untl_v)]
    while (length(untl_v) < length(inpt_v)){
      untl_v <- c(untl_v, val_add)
    }
  }
  for (el in 1:length(inpt_v)){
    cur_char <- inpt_v[el]
    if (untl_v[el] == "max"){
        cur_char <- gsub(x = cur_char, pattern = pattern, replacement = replacement)
    }else{
      for (i in 1:untl_v[el]){
        cur_char <- sub(x = cur_char, pattern = pattern, replacement = replacement)
      }
    }
    inpt_v[el] <- cur_char
  }
  return(inpt_v)
}

#' better_sub_mult
#'
#' Allow to perform a sub_mult operation to a given number of matched patterns, see examples
#'
#'
#' @param inpt_v is a vector containing all the elements that contains expressions to be substituted
#' @param pattern_v is a vector containing all the patterns to be substituted in any elements of inpt_v
#' @param replacement_v is a vector containing the expression that are going to substituate those provided by pattern_v
#' @param untl_v is a vector containing, for each element of inpt_v, the number of pattern that will be substituted
#' @examples
#'
#' print(better_sub_mult(inpt_v = c("yes NAME, i will call NAME and NAME2", 
#'                              "yes NAME, i will call NAME and NAME2, especially NAME2"),
#'                   pattern_v = c("NAME", "NAME2"),
#'                   replacement_v = c("Kevin", "Paul"),
#'                   untl = c(1, 3)))
#'
#' [1] "yes Kevin, i will call NAME and Paul"                 
#' [2] "yes Kevin, i will call NAME and Paul, especially Paul"
#'
#' print(better_sub_mult(inpt_v = c("yes NAME, i will call NAME and NAME2", 
#'                               "yes NAME, i will call NAME and NAME2, especially NAME2"),
#'                    pattern_v = c("NAME", "NAME2"),
#'                    replacement_v = c("Kevin", "Paul"),
#'                    untl = c("max", 3)))
#' 
#' [1] "yes Kevin, i will call Kevin and Kevin2"                   
#' [2] "yes Kevin, i will call Kevin and Kevin2, especially Kevin2"
#'
#' @export

better_sub_mult <- function(inpt_v = c(), pattern_v = c(), 
                            replacement_v = c(), untl_v = c()){
  if (length(untl_v) < length(inpt_v)){
    val_add <- untl_v[length(untl_v)]
    while (length(untl_v) < length(inpt_v)){
      untl_v <- c(untl_v, val_add)
    }
  }
  for (el in 1:length(inpt_v)){
    cur_char <- inpt_v[el]
    for (i2 in 1:length(pattern_v)){
      if (untl_v[i2] == "max"){
          cur_char <- gsub(x = cur_char, pattern = pattern_v[i2], replacement = replacement_v[i2])
      }else{
        for (i in 1:untl_v[i2]){
          cur_char <- sub(x = cur_char, pattern = pattern_v[i2], replacement = replacement_v[i2])
        }
      }
    }
    inpt_v[el] <- cur_char
  }
  return(inpt_v)
}

#' grep_all2
#'
#' Performs the grep_all function with another algorythm, potentially faster
#'
#' @param inpt_v is the input vectors to grep elements from
#' @param pattern_v is a vector contaning the patterns to grep
#' @examples
#'
#' print(grep_all2(inpt_v = c(1:14, "z", 1:7, "z", "a", "z"), 
#'                pattern_v = c("z", "4")))
#'
#' [1] 15 23 25  4 14 19
#'
#' print(grep_all2(inpt_v = c(1:14, "z", 1:7, "z", "a", "z"), 
#'                pattern_v = c("z", "^4$")))
#'
#' [1] 15 23 25  4 19
#'
#' print(grep_all2(inpt_v = c(1:14, "z", 1:7, "z", "a", "z"), 
#'                pattern_v = c("z")))
#' 
#' [1] 15 23 25
#'
#' @export

grep_all2 <- function(inpt_v, pattern_v){
  rtn_v <- c(grep(pattern = pattern_v[1], x = inpt_v))
  if (length(pattern_v) > 1){
    pattern_v <- pattern_v[2:length(pattern_v)]
    for (ptrn in pattern_v){
      cur_grp <- grep(pattern = ptrn, x = inpt_v)
      rtn_v <- c(rtn_v, cur_grp)
      inpt_v <- inpt_v[-cur_grp]
    }
  }
  return(rtn_v)
}

#' better_split_any
#'
#' Allows to split a string by multiple split regardless of their length, returns a vector and not a list. Contrary to better_split, this functions keep the delimiters in the output.
#' @param inpt is the input character
#' @param split_v is the vector containing the splits
#' @examples
#'
#' print(better_split_any(inpt = "o-u_i", split_v = c("-")))
#' 
#' [1] "o"  "-" "u_i"
#'
#' print(better_split_any(inpt = "o-u_i", split_v = c("-", "_")))
#'
#' [1] "o" "-" "u"  "_" "i"
#'
#' print(better_split_any(inpt = "--o--_/m/m/__-opo-/m/-u_i-_--", split_v = c("--", "_", "/")))
#'
#'  [1] "--"    "o"     "--"    "_"     "/"     "m"     "/"     "m"     "/"    
#' [10] "_"     "_"     "-opo-" "/"     "m"     "/"     "-u"    "_"     "i-"   
#' [19] "_"     "--"   
#'
#'
#' print(better_split_any(inpt = "(ok(ee:56))(ok2(oui)(ee:4))", split_v = c("(", ")", ":")))
#'
#'  [1] "("   "ok"  "("   "ee"  ":"   "56"  ")"   ")"   "("   "ok2" "("   "oui"
#'  [13] ")"   "("   "ee"  ":"   "4"   ")"   ")"  
#'
#' @export

better_split_any <- function(inpt, split_v = c()){
  glue_groupr_v <- function(inpt_v, group_v = c(), untl){
    rtn_v <- c()
    cur_v <- c()
    grp_status <- FALSE
    cnt_untl = 0
    for (el in inpt_v) {
      if (el %in% group_v & cnt_untl < untl){
        grp_status <- TRUE
        cur_v <- c(cur_v, el)
        cnt_untl = cnt_untl + 1
      }else if (grp_status){
        grp_status <- FALSE
        rtn_v <- c(rtn_v, paste(cur_v, collapse = ""))
        cur_v <- c()
        if (el %in% group_v){
          cnt_untl = 1
          cur_v <- c(el)
          grp_status <- TRUE
        }else{
          cnt_untl = 0
          rtn_v <- c(rtn_v, el)
        }
      }else{
        rtn_v <- c(rtn_v, el)
      }
    }
    if (length(cur_v) > 0){
      rtn_v <- c(rtn_v, paste(cur_v, collapse = ""))
    }
    return(rtn_v)
  }
  regex_spe_detect <- function(inpt){
        fillr <- function(inpt_v, ptrn_fill="\\.\\.\\.\\d"){
          ptrn <- grep(ptrn_fill, inpt_v)
          while (length(ptrn) > 0){
            ptrn <- grep(ptrn_fill, inpt_v)
            idx <- ptrn[1] 
            untl <- as.numeric(c(unlist(strsplit(inpt_v[idx], split="\\.")))[4]) - 1
            pre_val <- inpt_v[(idx - 1)]
            inpt_v[idx] <- pre_val
            if (untl > 0){
              for (i in 1:untl){
                inpt_v <- append(inpt_v, pre_val, idx)
              }
            }
          ptrn <- grep(ptrn_fill, inpt_v)
          }
          return(inpt_v)
        }
        inpt <- unlist(strsplit(x=inpt, split=""))
        may_be_v <- c("[", "]", "{", "}", "-", "_", ".", "(", ")", "/", "%", "*", "^", "?", "$")
        pre_idx <- unique(match(x=inpt, table=may_be_v))
        pre_idx <- pre_idx[!(is.na(pre_idx))]
        for (el in may_be_v[pre_idx]){
                cnt = 0
                for (i in grep(pattern=paste("\\", el, sep=""), x=inpt)){
                        inpt <- append(x=inpt, values="\\", after=(i - 1 + cnt))
                        cnt = cnt + 1
                }
        }
          return(paste(inpt, collapse=""))
  }
  for (split in split_v){
    pre_inpt <- inpt
    inpt <- c()
    lst_splt <- FALSE
    for (el in pre_inpt){
      cur_splt <- unlist(strsplit(x = el, split = regex_spe_detect(split)))
      cur_splt[cur_splt == ""] <- split
      cnt = 1
      while (cnt < length(cur_splt)){
        if (cur_splt[cnt] %in% split_v){
          if (lst_splt & cur_splt[cnt] != split){
            cur_splt <- append(x = cur_splt, values = split, after = cnt)
            cnt = cnt + 2 
          }
          lst_splt <- TRUE
          cnt = cnt + 1
        }else{
          cur_splt <- append(x = cur_splt, values = split, after = cnt)
          lst_splt <- FALSE
          cnt = cnt + 2
        }
      }
      cur_grp <- c()
      split_decomp <- unlist(strsplit(x = split, split = ""))
      for (el2 in split_decomp){ cur_grp <- c(cur_grp, el2) }
      last_verif <- glue_groupr_v(unlist(strsplit(x = el, split = "")), group_v = cur_grp, untl = nchar(split))
      if (sum(grepl(x = last_verif, pattern = regex_spe_detect(split))) == (sum(grepl(x = cur_splt, pattern = regex_spe_detect(split))) + 1)){
        cur_splt <- c(cur_splt, split)
      }
      inpt <- c(inpt, cur_splt)
    }
  }
  return(inpt)
}


