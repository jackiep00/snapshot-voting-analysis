library(tidyverse)
library(ghql)
library(jsonlite)

# Use GraphQL to pull data from Snapshot

# set the space name we're working with here:
# Some potential space names:
# Radicle - gov.radicle.eth
# ENS - ens.eth
# Gitcoin - gitcoindao.eth
# Aave - aave.eth

create_proposal_query <- function(space_name = 'index-coop.eth') {
  proposal_query <- sprintf(
    '
    query Proposals {
      proposals (
        first: 10000000
        where: {
          space_in: ["%s"],
          state: "closed",
          scores_state: "final"
        },
        orderBy: "created",
        orderDirection: desc
      ) {
        id
        title
        body
        choices
        start
        end
        snapshot
        state
        author
        scores
        scores_by_strategy
        scores_state
        scores_total
        scores_updated
        created
        votes
        space {
          id
          name
        }
      }
    }'
    , space_name
  )
  
  return(proposal_query)
}
create_vote_query <- function(proposal_id) {
  vote_query <- sprintf(
    '
    query Votes {
      votes (
        first: 9999999
        skip: 0
        where: {
          proposal: "%s"
        }
        orderBy: "created",
        orderDirection: desc
      ) {
        id
        ipfs
        voter
        created
        space {
          id
        }
        proposal {
          id
        }
        choice
        metadata
        vp
        vp_by_strategy
        vp_state
      }
    }'
    , proposal_id
  )
  return(vote_query)
}

create_proposal_df <- function(space_id = 'index-coop.eth') {
  url <- "https://hub.snapshot.org/graphql/"
  conn <- GraphqlClient$new(url = url)
  
  proposal_query_obj <- Query$new()$query('link',create_proposal_query(space_id))
  
  proposal_query_result <- conn$exec(proposal_query_obj$link) %>%
    fromJSON(flatten = F)
  
  proposal_query_df_raw <-proposal_query_result$data$proposals %>% as_tibble
  
  return (proposal_query_df_raw)
  # Clean the proposal query DF
}

create_vote_df_from_proposal_id <- function(proposal_id) {
  url <- "https://hub.snapshot.org/graphql/"
  conn <- GraphqlClient$new(url = url)
  proposal_id <- '0xc40a68e94ed07db068563f8c991730a8ec409c54ab34695782cbe8f0bc6bfac7'
  
  vote_query_obj <- Query$new()$query('link',create_vote_query(proposal_id))
  
  vote_query_result <- conn$exec(vote_query_obj$link) %>%
    fromJSON(flatten = F)
  
  vote_query_df <-vote_query_result$data$votes %>% as_tibble() %>%
    rename(space = space$id
           , proposal = proposal$id
           )
}

make_proposal_category_from_title <- function(space_id, title) {
  # For Index Coop, I categorized it each proposal by doing some basic string matching on the title
  if (space_id == 'index-coop.eth'){
    if (grepl('DG1', title, fixed = TRUE) | grepl('Decision Gate 1', title, fixed = TRUE)) {
      category <- 'DG1'
    }
    else if (grepl('DG2', title, fixed = TRUE) | grepl('Decision Gate 2', title, fixed = TRUE)){
      category <- 'DG2'
    }
    else if (grepl('IIP', title, fixed = TRUE)){
      category <- 'IIP'
    }
    else {
      category <- 'Metagovernance'
    }
  }
  else {
    warning('No proposal categorization scheme found, returning default')
    category <- 'default'
  }
  
  return(category)
}

make_proposal_result <- function(category, percentage_for) {
  
}

# iterates over a list of choices and returns the index of the choice that matches for
find_yes_choice <- function(choices) {
  for (i in seq_along(choices))
    if(str_detect(str_to_lower(choices[[i]]), 'for|yes|approve|aye|yae|yay|adopt'))
      return(as.numeric(i))
  return(NULL)
}

find_no_choice <- function(choices) {
  for (i in seq_along(choices))
    if(str_detect(str_to_lower(choices[[i]]), 'against|reject|no|deny|nay|nae'))
      return(as.numeric(i))
  return(NULL)
}

find_abstain_choice <- function(choices) {
  for (i in seq_along(choices))
    if(str_detect(str_to_lower(choices[[i]]), 'abstain'))
      return(as.numeric(i))
  return(NULL)
}

clean_proposal_query <- function(proposal_query_df_raw){
  proposal_query_df <- proposal_query_df_raw %>%
    # filter(scores_state == 'final') %>%       # Only look at votes that are finalized
    filter(sapply(choices,length) <= 3) %>%     # Only look at votes that are maximum "yes/no/abstain"
    mutate(yes_choice_idx = vapply(choices, find_yes_choice, c(1))
          , no_choice_idx = vapply(choices, find_no_choice, c(1))
          , abst_choice_idx = vapply(choices, find_abstain_choice), c(1)) %>%
    mutate(yes_votes = )
    
  # we need to parse through the choices and figure out which choice is a "yes" vote
  # and which choice is a "no" vote
  # Iterate over the choices and look for the indices that we care about, and save them on a per-proposal level
  # Is there a way to vectorize this? It sure doesn't seem like it
  
    
    
    
    # Clean Proposal Dataframe
    
}





