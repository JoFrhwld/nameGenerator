#' Generate States
#'
#' Generates the transition matrix from one character
#' to the next for a given year and sex.
#'
#' @param Year the year from which training data is drawn
#' @param Sex the sex from which training data is drawn
#' @import babynames
#' @importFrom tau textcnt
#' @import dplyr
#' @import reshape2
#' @export

generate_states <- function(Year = 1950, Sex = "F"){
  babynames%>%
    filter(year == Year, sex == Sex)%>%
    mutate(name = paste0("#", name, "_"))%>%
    group_by(year, sex, name, n)%>%
    do(melt(as.table(textcnt(.$name, 2, tolower = T, method = "string", split = ""))))%>%
    mutate(value = n*value)%>%
    group_by(year, sex, Var1) %>%
    summarise(value = sum(value))->bigram_n

  states <-  cbind(bigram_n, colsplit(bigram_n$Var1, " ", names = c("from", "to")))

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

#' Generate States2
#'
#' Generates the transition matrix from a bigram
#' to the next character for a given year and sex.
#'
#' @inheritParams generate_states
#' @import babynames
#' @importFrom tau textcnt
#' @import dplyr
#' @import reshape2
#' @export

generate_states2 <- function(Year = 1950, Sex = "F"){
  babynames%>%
    filter(year == Year, sex == Sex)%>%
    mutate(name = paste0("#", name, "_"))%>%
    group_by(year, sex, name, n)%>%
    do(melt(as.table(textcnt(.$name, 3, tolower = T, method = "string", split = ""))))%>%
    mutate(value = n*value)%>%
    group_by(year, sex, Var1) %>%
    summarise(value = sum(value))->trigram_n

  trigram_n <- cbind(trigram_n, colsplit(trigram_n$Var1,
                                         pattern = " ",
                                         names = c("from1","from2", "to")))

  trigram_n <- trigram_n %>% mutate(from = paste0(from1, from2))%>%
    group_by(year, sex, from)%>%
    mutate(prob = value/sum(value))%>%
    ungroup()-> trigram_n


  return(trigram_n)

}

#' Memoised States2
#'
#' @inheritParams generate_states
#' @importFrom memoise memoise
#' @export

memStates2 <- memoise(generate_states2)



#' Generate Length
#' @inheritParams generate_states
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
#' @inheritParams generate_states
#' @importFrom memoise memoise
#' @export

memLength <- memoise(generate_length)




