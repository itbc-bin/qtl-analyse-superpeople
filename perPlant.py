bestand = open("tabsep.txt","r")
plantsBestand = open("CvixLerC9.qua~","r")
plants = {}
for i in plantsBestand:
    plants[i.split("\t")[0]] = []

regel = []
positivePlants = []
counter = 0
for i in bestand:
    if counter > 0:
        regel = i.split("\t")
        positivePlants = regel[8].split(",")
        for p in plants:
            if p in positivePlants:
                plants[p].append(regel[0])


    counter += 1

bestand.close()
plantsBestand.close()



bestand = open("MarkerPerPlant.txt","w")
bestand.write("Plants"+"\t"+"n.Markers")
for key in plants.keys():
    bestand.write("\t"+key)

bestand.write("\n")

for key in plants.keys():
    bestand.write(""+key+"\t"+str(len(plants[key]))+"\t")

    for key2 in plants.keys():
        set1 = set(plants[key])
        set2 = set(plants[key2])
        inter = set.intersection(set1,set2)

        if key == key2:
            bestand.write("X")
        else:
            counter = 0
            for i in inter:
                if counter == 0:
                    bestand.write(i)
                else:
                    bestand.write(","+i)
                counter += 1
        bestand.write("\t")
    bestand.write("\n")


bestand.close()

