# List of potential computer names
computer_names <- c("Alexa", "Charlie", "Jordan", "Morgan", "Riley", "Skyler", "Taylor", "Jamie", "Casey", "Drew")

# Randomly select a computer name
computer_name <- sample(computer_names, 1)

# Initialize scores
player_score <- 0
computer_score <- 0

# Function to convert choice letters to words
convert_choice <- function(choice) {
    if (choice == "r") {
        return("Rock")
    } else if (choice == "p") {
        return("Paper")
    } else {
        return("Scissors")
    }
}

# Function to update and display scores
update_scores <- function(player_wins, is_tie = FALSE) {
    if (!is_tie) {
        if (player_wins) {
            player_score <<- player_score + 1
        } else {
            computer_score <<- computer_score + 1
        }
    }
    cat("Your score: ", player_score, "\n")
    cat(paste0(computer_name, "'s score: "), computer_score, "\n")
    flush.console() # Ensure immediate display of scores
}

# Function to display the welcome message
welcome_message <- function() {
    cat(paste("This is the Rock-Paper-Scissors game.", computer_name, "will be your opponent this round.\nChoices are 'r' (Rock), 'p' (Paper), 's' (Scissors)\n"))
    flush.console()
}

# Define the Rock-Paper-Scissors engine function
engine <- function() {
    choices <- c("r", "p", "s") # Define the possible choices

    # User choice input
    user_choice <- readline("Please type your choice (r/p/s) or press enter to stop playing: ")

    # Allow user to press enter to quit the engine
    if (user_choice == "") {
        cat("\nGame over. Final Scores - Your score: ", player_score, ", ", paste0(computer_name, "'s score: "), computer_score, ". Thanks for playing! 🌟\n")
        flush.console()
        return(invisible()) # Prevent NULL showing at the end
    }

    # Keep asking for input until a valid choice is made
    while (!(user_choice %in% choices)) {
        user_choice <- readline("Invalid choice. Please choose only 'r', 'p', or 's': ")
        if (user_choice == "") {
            cat("\nGame over. Final Scores - Your score: ", player_score, ", ", paste0(computer_name, "'s score: "), computer_score, ". Thanks for playing! 🌟\n")
            flush.console()
            return(invisible()) # Prevent NULL showing at the end
        }
    }

    # Generate a random choice for the computer and convert both choices to words
    computer_choice <- sample(choices, 1)
    user_choice_word <- convert_choice(user_choice)
    computer_choice_word <- convert_choice(computer_choice)

    # Display choices
    cat("Your choice:", user_choice_word, "\n")
    cat(paste0(computer_name, "'s choice: "), computer_choice_word, "\n")

    # Determine the winner or if it's a tie and update scores
    if (user_choice == computer_choice) {
        cat("It's a tie!\n")
        update_scores(FALSE, is_tie = TRUE)
    } else {
        if ((user_choice == "r" && computer_choice == "s") ||
            (user_choice == "p" && computer_choice == "r") ||
            (user_choice == "s" && computer_choice == "p")) {
            cat("You win!! 💪\n")
            update_scores(TRUE)
        } else {
            cat(computer_name, " wins. 🤖\n")
            update_scores(FALSE)
        }
    }

    # Call engine again for a new round
    engine()
}

# Function to start the game
game <- function() {
    welcome_message()
    engine()
}

# Execute the game function
game()
