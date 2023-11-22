{
  library(tidyverse)
  
}

{
  cards <- rep(
    seq(0,13,1),
    c(2, rep(4, 12), 2)
  )
  
  actions <- c(
    rep(NA, 7),
    rep("peek", 2),
    rep("spy", 2),
    rep("swap", 2),
    rep(NA, 2)
  )
}

{
  
  
  fn_deal <- function(deck, num_cards = 1){
    (deal <- sample(deck, num_cards, replace = FALSE))
    
    (cards_remove <- sapply(deal, \(x){which(deck == x)[1]}))
    
    for(i in seq_along(cards_remove)){
      x <- cards_remove[i] == cards_remove
      cards_remove <- cards_remove + (cumsum(x)-1)*x
    }
    
    deck <<- deck[-cards_remove]
    
    return(deal)
  }
  
  
  fn_takeTurn(hand, deck){
    
    
    
    
  }
  
  fn_discard <- function(){}
  
  fn_kamikazee <- function(){}
  
  fn_peek <- function(){}
  
  fn_spy <- function(){}
  
  fn_swap <- function(){}
}

{
  deck <- cards
  players = 2
  
  p1_hand <- fn_deal(deck, 4)
  
  setdiff(deck, c(6,1,12,7))
  
  which(deck == c(1, 2, 3))
  
}