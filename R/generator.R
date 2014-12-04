#' Generate States
#'
#' Generates the transition matrix from one character
#' to the next for a given year and sex.
#'
#' @import babynames
#' @importFrom tau textcnt
#' @import dplyr
#' @import reshape2
#' @export

generate_states <- function(Year = 1950, Sex = "F"){
  babynames%>%
    filter(year == Year, sex == Sex)%>%
    group_by(year, sex, name, n)%>%
    do(melt(as.table(textcnt(.$name, 2, tolower = T, method = "string", split = ""))))%>%
    mutate(value = n*value)%>%
    group_by(year, sex, Var1) %>%
    summarise(value = sum(value))->bigram_n

  babynames%>%
    filter(year == Year, sex == Sex)%>%
    mutate(name = tolower(name),
           first = gsub("(^[a-z]).*", "start \\1", name),
           last = gsub(".*([a-z]$)", "\\1 end", name))->firstlast

  first <- firstlast %>%
    mutate(Var1 = first)%>%
    group_by(year, sex, Var1)%>%
    summarise(value = n())

  last <- firstlast %>%
    mutate(Var1 = last)%>%
    group_by(year, sex, Var1)%>%
    summarise(value = n())



  states <- rbind_list(bigram_n, first, last)
  states <-  cbind(states, colsplit(states$Var1, " ", names = c("from", "to")))

  states%>%
    group_by(year, sex, from)%>%
    mutate(prob = value/sum(value))%>%
    ungroup()-> states


  return(states)

}


#' Memoised States
#' @importFrom memoise memoise
#' @export

memStates <- memoise(generate_states)


#' Generate Length
#'
#' @import babynames
#' @import dplyr
#' @export
generate_length <- function(Year = 1950, Sex = "F"){
  babynames%>%
    filter(year == Year, sex == Sex)%>%
    mutate(nchar = nchar(name))%>%
    group_by(year, sex, nchar)%>%
    summarise(n = n())%>%
    mutate(prob = n/sum(n),
           cum_prob = cumsum(prob))->length_prob

  return(length_prob)
}

#' Memoised Length
#'
#' @importFrom memoise memoise
#' @export

memLength <- memoise(generate_length)



#' Generate One Name
#'
#' Generates one name on the basis of transition probabilities from a given year
#' for a given sex.
#'
#' @details
#' Using data from the \code{babynames} package, this function will generate
#' novel "names" using a Markov Chain based on transition probabilities for
#' a given year and sex.
#'
#' Since the Markov Chains rarely land in the \code{end} state, this function
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

  current_state <- "start"
  end <- "end"
  out = ""
  iter = 0
  while(current_state != end & iter < 15){

    to_space <- to[from==current_state]
    to_probs <- probs[from==current_state]
    new_state <- sample(to_space, size = 1, replace = F, prob = to_probs)
    if(current_state == "start"){
      out <- paste0(out, toupper(new_state))
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
#'
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

generate_n_names <- function(n = n, Year = 1950, Sex = "F"){
  out <- rep("", n)
  for(i in seq(along =out)){
    out[i] <- generate_name(Year = Year, Sex = Sex)
  }
  return(out)
}

#' Most Probable Path
#'
#' Returns the most probable path through the character state space for
#' a given year for a given sex.
#'
#' @details
#' The most probable path for most years and sexes is actually an infinite
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

  current_state <- "start"
  end <- "end"
  out = ""

  iter <- 0
  while(current_state != end & iter < 20){
    max_prob <- max(probs[from == current_state])
    new_state <- to[probs == max_prob][1]
    if(grepl(new_state, out)){
      current_state <- end
      out <- paste0(out, new_state, ":")
    }else{
      if(current_state == "start"){
        out <- paste0(out, toupper(new_state))
      }else if(new_state == "end"){
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
