#' Most Probable Path
#'
#' Returns the most probable path through the character \eqn{\rightarrow}{->} character state space for
#' a given year for a given sex.
#'
#' @inheritParams generate_states
#' @details
#' The most probable path for most years and sexes is occasionally an infinite
#' loop. When a path is about to loop infinitely, the path stops, and \code{:}
#' is appended to the end. If it reaches the end state, \code{.} is appended.
#'
#' The first time using \code{most_prob_name()}, \code{generate_name()} or
#' \code{generate_n_names()} with a unique Year and Sex combination may take
#' a few seconds to run, but should be faster on any subsequent call to the same
#' function or any of the other two using the same Year and Sex combination.
#'
#' @examples
#' most_prob_name(Year = 1985, Sex = "M")
#'
#' @export

most_prob_name <- function(Year = 1950, Sex = "F"){
  states <- memStates(Year = Year, Sex = Sex)
  length_dist <- memLength(Year = Year, Sex = Sex)

  from <- states$from
  to <- states$to
  probs <- states$prob

  current_state <- "#"
  end <- "_"
  out = ""

  iter <- 0
  while(current_state != end & iter < 20){
    max_prob <- max(probs[from == current_state])
    new_state <- to[probs == max_prob][1]
    if(grepl(new_state, out)){
      current_state <- end
      out <- paste0(out, new_state, ":")
    }else{
      if(current_state == "#"){
        out <- paste0(out, toupper(new_state))
      }else if(new_state == end){
        out <- paste0(out, ".")
      }else{
        out <- paste0(out, new_state)
      }
      current_state = new_state
      iter = iter + 1
    }
  }
  return(out)

}


#' Most Probable Path2
#'
#' Returns the most probable path through the bigram \eqn{\rightarrow}{->} character state space for
#' a given year for a given sex.
#' @inheritParams generate_states
#' @details
#' The most probable path for most years and sexes is occasionally an infinite
#' loop. When a path is about to loop infinitely, the path stops, and \code{:}
#' is appended to the end. If it reaches the end state, \code{.} is appended.
#'
#' The first time using \code{most_prob_name2()}, \code{generate_name2()} or
#' \code{generate_n_names2()} with a unique Year and Sex combination may take
#' a few seconds to run, but should be faster on any subsequent call to the same
#' function or any of the other two using the same Year and Sex combination.
#'
#' @examples
#' most_prob_name(Year = 1985, Sex = "M")
#'
#' @importFrom stringr str_count
#' @importFrom stringr str_extract
#'
#' @export

most_prob_name2 <- function(Year = 1950, Sex = "F"){
  bi_states <- memStates(Year = Year, Sex = Sex)
  tri_states <- memStates2(Year = Year, Sex = Sex)

  length_dist <- memLength(Year = Year, Sex = Sex)

  bi_from <- bi_states$from
  bi_to <- bi_states$to
  bi_probs <- bi_states$prob

  tri_from <- tri_states$from
  tri_to <- tri_states$to
  tri_probs <- tri_states$prob



  end_pattern <- "[_\\.:]"
  end <- "_"
  out = "#"
  iter = 0

  while(!grepl(end_pattern, out) & iter < 20){

    if(str_extract(out, "..$") %in% tri_from){
      to_space <- tri_to[tri_from == str_extract(out, "..$")]
      prob_space <- tri_probs[tri_from == str_extract(out, "..$")]
      new_state <- to_space[prob_space == max(prob_space)][1]
    }else{
      to_space <- bi_to[bi_from == str_extract(out, ".$")]
      prob_space <- bi_probs[bi_from == str_extract(out, ".$")]
      new_state <- to_space[prob_space == max(prob_space)][1]
    }
    out <- paste0(out, new_state)
    test_state <- str_extract(out, "..$")

    if(str_count(out, test_state) > 1){
      out <- paste0(out, new_state, ":")
    }else{
      if(grepl(end_pattern, out)){
        out <- paste0(out, ".")
      }
    }
    iter = iter + 1
  }
  out <- gsub("[#_]", "", out)
  out_vec <- unlist(strsplit(out, split = ""))
  out_vec[1] <- toupper(out_vec[1])
  out <- paste(out_vec, collapse = "")

  return(out)


}
