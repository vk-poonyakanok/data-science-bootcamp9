# Homework 15.2
# Create DataAanalyst class

class DataAnalyst:
  def __init__(self, name, age, company, salary):
      self.name = name
      self.age = age
      self.company = company
      self.salary = salary
      # Initialize skills with boolean values
      self.spreadsheet = False
      self.python = False
      self.sql = False
      self.r = False
      self.machine_learning = False
      self.dashboard = False

  def detail(self):
      print(f"{self.name} is {self.age} years old, works at {self.company}, and earns {self.salary} per year.")

  def add_skill(self, skill):
      if hasattr(self, skill):
          setattr(self, skill, True)
          print(f"{self.name} has now acquired the {skill} skill.")
      else:
          print(f"Skill {skill} is not recognized for {self.name}.")

  def list_skills(self):
      skills = [skill for skill in ['spreadsheet', 'python', 'sql', 'r', 'machine_learning', 'dashboard'] if getattr(self, skill)]
      if skills:
          print(f"{self.name}'s skills: {', '.join(skills)}")
      else:
          print(f"{self.name} has no skills listed yet.")

analyst01 = DataAnalyst("Vitchakorn", 29, "Department of Disease Control", 100000)

# Display basic details
analyst01.detail()

# Add and display skills
analyst01.add_skill("spreadsheet")
analyst01.add_skill("python")
analyst01.add_skill("sql")
analyst01.add_skill("r")
analyst01.add_skill("machine_learning")
analyst01.add_skill("dashboard")
analyst01.list_skills()