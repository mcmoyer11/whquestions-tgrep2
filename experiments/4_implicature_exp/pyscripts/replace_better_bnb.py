# Compare 'hand_annotated_v1.csv' and files in "../korpus"
# for each item in a file in "../korpus"
# if it is "option_bnb==FALSE" in 'hand_annotated_v1.csv', replace the "best sentence" with "better_bnb"

import os
import csv

change_ids = []
change_bnbs = []
# remove_ids = []
with open('hand_annotated_v1.csv', encoding="ISO-8859-1") as csvfile:
    readCSV= csv.reader(csvfile, delimiter=',')
    for row in readCSV:
        tg = row[0]
        option_bnb = row[6] # TRUE or FALSE
        better_bnb = row[7] # sentence to replace
        # weird = row[8] # weird or not
        # if (weird == "TRUE"):
        #     remove_ids.append(tg)
        if (option_bnb == "FALSE"):
            change_ids.append(tg)
            change_bnbs.append(better_bnb)

# removed = 0
changed = []
for filename in os.listdir("../korpus"):
    if filename.endswith(".txt"):
        with open(os.path.join("../korpus", filename), 'r') as f:
            content = f.read()
            lines = content.split('\n') # line[0] is "TGrepID	EntireSentence	context	BestResponse"
            # missing = '5704:06'
            # for line in lines:
            #     if missing in line:
            #         print("FOUND IT!")
            for index, line in enumerate(lines[:-1]):
                line_content = line.split('\t')
                tgrep = line_content[0] 
                entire_sentence = line_content[1] 
                context = line_content[2] 
                best_response = line_content[3]
                line = line + '\n'
                # for x in range(len(remove_ids)-1):
                #     if tgrep == remove_ids[x]:
                #         line = ''
                #         removed += 1
                        #print("REMOVED: ",filename,index,tgrep)
                for x in range(len(change_ids)):
                    if tgrep == change_ids[x]:
                        best_response = change_bnbs[x]
                        line = tgrep + '\t' + entire_sentence + '\t' + context + '\t' + best_response + '\n'
                        changed.append(tgrep)
                        print("changing: ",filename,index,tgrep)
                newFile = open(filename, 'a')
                newFile.write(line)

# total_removed = removed - changed
# print("TOTAL REMOVED: ", total_removed)
print("SHOULD CHANGE: ", len(change_ids))
print("TOTAL CHANGED: ", len(changed))

list_difference = []
for item in change_ids:
  if item not in changed:
    list_difference.append(item)
print("Items that weren't changed: ", list_difference)
