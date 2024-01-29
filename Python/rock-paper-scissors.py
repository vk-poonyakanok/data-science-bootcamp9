# Homework 15.1
# Rock-Paper-Scissors game
import random

def game():
    print("🎲 Welcome to the Rock-Paper-Scissors game 🎲")
    player = input("👤 Please enter your name: ")
    print("")

    print(f"🌟 Welcome, {player}!")

    while True:
        try:
            target_score = int(input("🏆 Enter the score needed to win the game: "))
            if target_score > 0:
                break
            else:
                print("❌ Please enter a positive number.")
        except ValueError:
            print("🚫 Please enter a valid integer.")

    print("")
    print(f"🔮 The first player to reach {target_score} points wins the game.")
    print("👉 Choose 1 for Rock 🪨, 2 for Paper 📄, 3 for Scissors ✂️, 4 to stop the game.")
    print("")

    choices = {1: 'Rock 🪨', 2: 'Paper 📄', 3: 'Scissors ✂️'}

    def ordinal(number):
        if 10 <= number % 100 <= 20:
            suffix = 'th'
        else:
            suffixes = {1: 'st', 2: 'nd', 3: 'rd'}
            suffix = suffixes.get(number % 10, 'th')
        return f"{number}{suffix}"

    round_number = 1
    player_score = 0
    computer_score = 0

    while player_score < target_score and computer_score < target_score:
        print(f"⭐ This is the {ordinal(round_number)} round.")
        while True:
            try:
                player_choice = int(input("✨ Make your choice (1=🪨, 2=📄, 3=✂️, or 4=quit): "))
                if player_choice in choices or player_choice == 4:
                    break
                else:
                    print("❌ Invalid choice. Please choose 1, 2, 3, or 4.")
                    print("")
            except ValueError:
                print("🚫 Please enter a valid number (1, 2, 3, or 4).")
                print("")

        if player_choice == 4:
            print(f"🛑 Game stopped! Final score: {player}: {player_score}, Computer: {computer_score}. Thanks for playing!")
            break

        player_choice_name = choices.get(player_choice, "Invalid")
        print("")
        print(f"😎 You chose {player_choice_name}")

        computer_choice = random.choice(list(choices.keys()))
        computer_choice_name = choices[computer_choice]
        print(f"🤖 The computer chose {computer_choice_name}.")

        if player_choice == computer_choice:
            print("🤝 It's a tie! 🤝")
        elif (player_choice == 1 and computer_choice == 3) or \
             (player_choice == 2 and computer_choice == 1) or \
             (player_choice == 3 and computer_choice == 2):
            print(f"🌈 {player} wins this round! 🌈")
            player_score += 1
        elif player_choice in choices:
            print("😈 Computer wins this round! 😈")
            computer_score += 1

        round_number += 1
        print(f"📊 The current score is {player}: {player_score}, Computer: {computer_score}.")
        print("")

    if player_score >= target_score and player_choice != 4:
        print(f"🎉 Victory! {player}, you've won the game with {player_score} points! 🎉")
    elif computer_score >= target_score and player_choice != 4:
        print(f"💔 Oh no, the computer won with {computer_score} points. Better luck next time! 💔")

# Run the game
game()
