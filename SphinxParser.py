import re

def parseSphinxDictionaryFile(filename):
    entries = {}
    with open(filename) as f:
        for line in f.readlines():
            parts = line.split()
            entries[parts[0]] = parts[1:]

    return entries

def writeSphinxDictionaryFile(dic, f):
    for entry in dic.items():
        f.write(entry[0] + "\t" + " ".join(entry[1]) + "\n")

def parseSphinxLanguageModelFile(filename):
    currentGram = None
    gramEntries = {}
    
    with open(filename) as f:
        for line in f.readlines():
            # check for lines like "\2-grams" which mark the beginning
            # of that data type
            gramMatch = re.match("^\\\([0-9])-grams:$", line)
            if gramMatch:
                currentGram = int(gramMatch.group(1))
                continue

            # everything before the first gram section is implicitly comments
            if not currentGram:
                continue

            if line.strip() == "":
                continue

            if line.strip() == "\\end\\":
                break

            data = line.strip().split()
            probability = float(data[0])
            backoffWeight = None
            try:
                backoffWeight = float(data[-1])
            except ValueError:
                # Field is optional, if last field isn't a float
                # it's part of the word list
                wordList = data[1:]
            else:
                wordList = data[1:-1]

            if not currentGram in gramEntries:
                gramEntries[currentGram] = []
            gramEntries[currentGram].append((probability, wordList, backoffWeight))

    return gramEntries

def writeSphinxLanguageModelFile(lm, f):
    f.write("\\data\\\n")
    for gram, entries in lm.items():
        f.write("ngram %s=%s\n" % (gram, len(entries)))
    f.write("\n")

    for gram, entries in lm.items():
        f.write("\\" + str(gram) + "-grams:\n")
        for entry in entries:
            f.write("%.04f %s" % (entry[0], " ".join(entry[1])))
            if not entry[2] is None:
                f.write(" %.04f" % (entry[2],))
            f.write("\n")
        f.write("\n")
    f.write("\\end\\\n")
