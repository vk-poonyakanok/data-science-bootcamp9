# Define the pizza chatbot function
pizza_chatbot <- function() {

  # Initial welcome message
  cat("Welcome to Blue's Pizza! 🍕 Let's customize your pizza.\n")
  flush.console()

  # Ask for pizza size
  size <- readline(prompt = "What size would you like? (Small/Medium/Large): ")
  while(!(size %in% c("Small", "Medium", "Large"))) {
    size <- readline(prompt = "Please choose a valid size (Small/Medium/Large): ")
  }

  # Ask for crust type
  crust <- readline("What type of crust? (Thin/Cheesy/Sausage/New York): ")
  while(!(crust %in% c("Thin", "Cheesy", "Sausage", "New York"))) {
    crust <- readline("Please choose a valid crust type (Thin/Cheesy/Sausage/New York): ")
  }

  # Define valid toppings
  valid_toppings <- c("Pepperoni", "Tom Yum Kung", "Seafood", "Hawaiian", "Ham&Crab Sticks")

  # Function to get valid topping
  get_valid_topping <- function(prompt_message) {
    topping <- readline(prompt_message)
    while(!(topping %in% valid_toppings)) {
      topping <- readline("Please choose a valid topping: ")
    }
    return(topping)
  }

  # Check for Half'n'Half option based on size
  half_half <- "No"
  if (size != "Small") {
    half_half <- readline("Do you want Half'n'Half toppings? (Yes/No): ")
    while(!(half_half %in% c("Yes", "No"))) {
      half_half <- readline("Please answer Yes or No: ")
    }
  }

  # Ask for toppings
  toppings_options <- "Choose your toppings (Pepperoni, Tom Yum Kung, Seafood, Hawaiian, Ham&Crab Sticks): "
  if (half_half == "Yes") {
    toppings_first_half <- get_valid_topping(paste("For the first half, ", toppings_options))
    toppings_second_half <- get_valid_topping(paste("For the second half, ", toppings_options))
    toppings <- paste("Half", toppings_first_half, "and half", toppings_second_half)
    # Check for seafood or tom yum kung in either half
    if ("Seafood" %in% c(toppings_first_half, toppings_second_half) || "Tom Yum Kung" %in% c(toppings_first_half, toppings_second_half)) {
      cat("\nPlease note: The Seafood/Tom Yum Kung topping contains shellfish and may cause allergies in some individuals.\n")
    }
  } else {
    toppings <- get_valid_topping(toppings_options)
    # Check for seafood or tom yum kung in single topping
    if (toppings == "Seafood" || toppings == "Tom Yum Kung") {
      cat("\nPlease note: The Seafood/Tom Yum Kung topping contains shellfish and may cause allergies in some individuals.\n")
    }
  }

  # Confirm the order
  cat("\nYou have ordered a", size, "pizza with a", crust, "crust and", toppings, "toppings. Yummy! 😋\n")
  cat("Thank you for ordering at Blue's Pizza! Your delicious pizza will be ready soon.\n")
}

# Run the pizza chatbot
pizza_chatbot()
