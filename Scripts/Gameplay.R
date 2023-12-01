# Gameplay ----
{
  ## Setup ----
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
  
  
  # Start & Setup ----
  {
    deck <- cards
    players = 2
    
    hands <- lapply(1:players, \(p){
      fn_deal(deck, 4)
    })
    discard <- fn_deal(deck, 1)
    
    scores <- lapply(hands, sum)
    
    
    turn_log <- tibble(
      turn = 0,
      player = 1:players,
      hand = sapply(hands, \(x){paste(x, collapse = ",")}),
      discard = -1,
      action = "begin"
    )
    
    gameCont = TRUE
    lastTurn <- lapply(c(1:players), \(x){1})
    caboCalled = FALSE
    t = 0
  }
  
  # Turn 1 ----
  {
    
    while (gameCont) {
      t = t + 1
      
      hands
      for(p in 1:players){
        if(lastTurn[[p]] == 1){
          
          drawAction = sample(
            c("fromDeck", "fromDiscard", "callCabo"), 1, 
            prob = c(.7, .2, .1)
          )
          card_num = ifelse(drawAction == "callCabo", NA, sample(c(1:4, NA), 1))
          
          
          
          if(drawAction == "callCabo"){
            fn_callCabo()
            
            turn_log <- rbind(turn_log, c(
              t,
              p,
              hands[[p]] %>% paste(collapse = ","),
              NA,
              action = drawAction
            ))
            
          } else {
            p.turn <- fn_turn(
              hand = hands[[p]],
              drawAction = drawAction,
              playAction = NA,
              card_num = card_num,
              deck = deck,
              discard = discard
            )
            
            turn_log <- rbind(turn_log, c(
              t,
              p,
              p.turn$hand %>% paste(collapse = ","),
              p.turn$discard,
              action = drawAction
            ))
          }
          
          if(caboCalled) lastTurn[[p]] <- 0
        }
        
      }
      # hands
      # turn_log
      cat(paste0("\nTurn:\t", t))
      
      
      if(sum(do.call(c, lastTurn)) == 0) gameCont = FALSE
      if(length(deck) == 0) gameCont = FALSE
      if(gameCont == FALSE) break
    }
    
    
    turn_log <- turn_log %>% 
      mutate(
        score = sapply(hand, \(x){
          str_split_1(x, pattern = ",") %>% 
            as.numeric() %>% 
            sum()
        })
      )
    
    
  }
  
  
}



# OOP
{
  install.packages("R7")
  library(R7) # Load the R7 package
  
  # Define Classes
  {
    # Define the Player class
    Player <- R7::setClass(
      "Player",
      slots = c(
        hand = "numeric", 
        discard = "numeric", 
        drawAction = "character", 
        playAction = "character", 
        card_num = "character"
      )
    )
    
    # Define the Turn class
    Turn <- R7::setClass(
      "Turn",
      slots = c(playerData = "list")
    )
    
    # Define the Game class
    Game <- R7::setClass(
      "Game",
      slots = c(turns = "list")
    )
  }
  
  
  # Constructor functions
  {
    # Constructor for Player
    create_player <- function(hand, discard, drawAction, playAction, card_num) {
      Player$new(
        hand = hand, 
        discard = discard, 
        drawAction = drawAction,
        playAction = playAction,
        card_num = card_num
      )
    }
    
    # Constructor for Turn
    create_turn <- function(playerData) {
      Turn$new(playerData = playerData)
    }
    
    # Constructor for Game
    create_game <- function(turns) {
      Game$new(turns = turns)
    }
    
  }
}