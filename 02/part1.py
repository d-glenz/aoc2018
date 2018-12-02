from collections import Counter

with open('input02.txt') as input_file:
    day2_input = input_file.read().split('\n')

doubles = triples = 0
for inp in day2_input: 
    cd = dict(Counter(inp)) 
    if [k for k,v in cd.items() if v==2]: 
        doubles +=1 
    if [k for k,v in cd.items() if v==3]: 
        triples +=1                                                                                                                                       

print(f"Doubles: {doubles}")
print(f"Triples: {triples}")
print(f"Checksum: {doubles*triples}")

