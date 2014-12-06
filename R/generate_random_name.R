#' Generate One Name
#'
#' Generates one name on the basis of bigram transition probabilities from a given year
#' for a given sex.
#'
#' @inheritParams generate_states
#' @details
#' Using data from the \code{babynames} package, this function will generate
#' novel "names" using a Markov Chain based on transition probabilities for
#' a given year and sex.
#'
#' This function
#' also probabilistically ends the chain on the basis of how long the name
#' currently is, based on the distribution of name lengths for the
#' given year and sex.
#'
#' The first time using \code{most_prob_name()}, \code{generate_name()} or
#' \code{generate_n_names()} with a unique Year and Sex combination may take
#' a few seconds to run, but should be faster on any subsequent call to the same
#' function or any of the other two using the same Year and Sex combination.
#' @export

generate_name <- function(Year = 1950, Sex = "F"){
  states <- memStates(Year = Year, Sex = Sex)
  length_dist <- memLength(Year = Year, Sex = Sex)

  from <- states$from
  to <- states$to
  probs <- states$prob

  current_state <- "#"
  end <- "_"
  out = ""
  iter = 0
  while(current_state != end & iter < 15){

    to_space <- to[from==current_state]
    to_probs <- probs[from==current_state]
    new_state <- sample(to_space, size = 1, replace = F, prob = to_probs)
    if(current_state == "#"){
      out <- paste0(out, toupper(new_state))
    }else if(new_state == end){
      out <- paste0(out, ".")
    }else{
      out <- paste0(out, new_state)
    }
    current_state = new_state
    iter = iter + 1

    if(iter >= min(length_dist$nchar)){
      if(iter %in% length_dist$nchar){
        stop_prob <- (length_dist %>% filter(nchar == iter))$cum_prob
        current_state <- sample(c(new_state, end), size = 1, replace = F, prob = c(1-stop_prob, stop_prob))
      }else{
        current_state <- end
      }
    }else{
      current_state = new_state
    }
  }
  return(out)

}


#' Generate N names
#'
#' Calls \code{generate_name()} \code{n} times.
#' @inheritParams generate_states
#' @param n the number of random names to generate
#' @details
#'
#' See \code{generate_name()}
#'
#' The first time using \code{most_prob_name()}, \code{generate_name()} or
#' \code{generate_n_names()} with a unique Year and Sex combination may take
#' a few seconds to run, but should be faster on any subsequent call to the same
#' function or any of the other two using the same Year and Sex combination.
#'
#' @export

generate_n_names <- function(n = 1, Year = 1950, Sex = "F"){
  out <- rep("", n)
  for(i in seq(along =out)){
    out[i] <- generate_name(Year = Year, Sex = Sex)
  }
  return(out)
}



#' Generate One Name2
#'
#' Generates one name on the basis of transition probabilities from bigrams to the next character
#' from a given year for a given sex.
#'
#'
#' @inheritParams generate_states
#' @details
#' Using data from the \code{babynames} package, this function will generate
#' novel "names" using a Markov Chain based on transition probabilities from bigrams to the next character
#' for a given year and sex.
#' When the current bigram wasn't present in the training data, it falls back to bigram transition probabilities
#' (i.e. character -> character probabilities).
#'
#' This function has a maximum name length of 20 characters.
#'
#' The first time using \code{most_prob_name2()}, \code{generate_name2()} or
#' \code{generate_n_names2()} with a unique Year and Sex combination may take
#' a few seconds to run, but should be faster on any subsequent call to the same
#' function or any of the other two using the same Year and Sex combination.
#'
#' @importFrom stringr str_extract
#' @export

generate_name2 <- function(Year = 1950, Sex = "F"){
  bi_states <- memStates(Year = Year, Sex = Sex)
  tri_states <- memStates2(Year = Year, Sex = Sex)

  length_dist <- memLength(Year = Year, Sex = Sex)

  bi_from <- bi_states$from
  bi_to <- bi_states$to
  bi_probs <- bi_states$prob

  tri_from <- tri_states$from
  tri_to <- tri_states$to
  tri_probs <- tri_states$prob



  end_pattern <- "[_\\.]"
  end <- "_"
  out = "#"
  iter = 0
  while(!grepl(end_pattern, out) & iter < 20){

    if(str_extract(out, "..$") %in% tri_from){
      to_space <- tri_to[tri_from == str_extract(out, "..$")]
      to_probs <- tri_probs[tri_from == str_extract(out, "..$")]
    }else{
      to_space <- bi_to[bi_from == str_extract(out, ".$")]
      to_probs <- bi_probs[bi_from == str_extract(out, ".$")]
    }

    new_state <- sample(to_space, size = 1, replace = F, prob = to_probs)
    if(new_state == end){
      out <- paste0(out, ".")
    }else{
      out <- paste0(out, new_state)
    }

    iter = iter + 1


  }

  out <- gsub("[#_]", "", out)
  out_vec <- unlist(strsplit(out, split = ""))
  out_vec[1] <- toupper(out_vec[1])
  out <- paste(out_vec, collapse = "")

  return(out)

}


#' Generate N names
#'
#' Calls \code{generate_name2()} \code{n} times.
#' @inheritParams generate_n_names
#' @details
#'
#' See \code{generate_name2()}
#'
#' The first time using \code{most_prob_name2()}, \code{generate_name2()} or
#' \code{generate_n_names2()} with a unique Year and Sex combination may take
#' a few seconds to run, but should be faster on any subsequent call to the same
#' function or any of the other two using the same Year and Sex combination.
#'
#' @export

generate_n_names2 <- function(n = 1, Year = 1950, Sex = "F"){
  out <- rep("", n)
  for(i in seq(along =out)){
    out[i] <- generate_name2(Year = Year, Sex = Sex)
  }
  return(out)
}



