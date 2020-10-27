
import os
import csv

for item in os.listdir():
    if os.path.isdir(item):
        with open(os.path.join(item, "./js/stimuli.js"), 'r') as f:
            with open(os.path.join(item, "./js/stimuli.js"), 'a') as a:
                content = f.read()
                lines = content.split('\n') #lines[1] is path
                for i in range(len(lines)):
                    if i != 1:
                        a.write(lines[i])
                    if i == 1:
                        old = lines[i].split('korpus3_') #old[1] is 0.txt'
                        number = item.split('_') #number[1] is number of experiment
                        new = number[1] + ".txt'"
                        new_line = old[0] + new
                        print(new_line)
                        a.write(new_line)
