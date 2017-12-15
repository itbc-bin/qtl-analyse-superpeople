import re

a = open("CvixLerC9.csv", "r")
b = open("CvixLerC9Rready3.csv", "w")

counter = 0
counter2 = 1
row = ''
for line in a:
    if counter is 5:
        counter = 0
        row = re.sub('\s+', '', row)
        for char in row:
            head += ','+char
        head+='\n'
        print(head)
        b.write(head)
        row = ''
    if counter > 0:
        #print(line.rstrip('\n'))
        row += line
    else:
        head = str(line.rstrip().split("(")[0])
        head = re.sub('\s+', '', head)
        head = re.sub(',', '', head)
        counter2+=1
    counter+=1
b.close()
a.close()
