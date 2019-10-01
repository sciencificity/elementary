
#' Generate a rumour
#'
#' @param suspect string value of the suspect
#' @param weapon string value of the weapon
#' @param room string value of the room
#'
#' @return a rumour based on who did the deed, in which room and with what weapon.
#' @importFrom glue glue
#' @examples
rumour <- function(suspect, weapon, room) {
  return(glue::glue("I think {suspect} did it in the {room} with the {weapon}"))
}

#' Whodunnit
#'
#' @param x an integer value of how many rumours you want to generate
#'
#' @return x number of rumours returned
#' @export
#'
#' @importFrom purrr pmap_chr
#'
#' @examples
#' whodunnit()
#' whodunnit(5)
whodunnit <- function(x = 1) {
  if (is.character(x)) {
    stop("whodunnit needs an integer input")
  }

  if (x %% 1 != 0) {
    stop("whodunnit needs an integer input")
  }

  suspects <- c(
    "Mrs. White", "Reverend. Green", "Mrs. Peacock",
    "Professor Plum", "Miss Scarlet", "Colonel Mustard"
  )
  weapons <- c(
    "Ax", "Bat", "Candlestick", "Dumbbell", "Pistol",
    "Poison", "Trophy", "Knife", "Rope"
  )
  rooms <- c(
    "Hall", "Guest House", "Dining Room", "Kitchen",
    "Patio", "Spa", "Theatre", "Living Room", "Observatory"
  )

  suspect = sample(suspects, x, replace = T)
  weapon = sample(weapons, x, replace = T)
  room = sample(rooms, x, replace = T)

  rumours = data.frame(suspect, weapon, room)

  return(purrr::pmap_chr(rumours, rumour))
}
