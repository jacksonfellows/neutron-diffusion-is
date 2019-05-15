from pyne.endf import Evaluation

h1 = Evaluation("./1-H.txt")
h1.read()

print(h1.reactions[1])
