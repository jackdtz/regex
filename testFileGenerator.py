import rstr
import os

targetFolder = "./test/testfiles/"
targetNumber = 200

def genRandomStr(regex):
    return rstr.xeger(regex)

def genRandomStrLst(regex, length):
    res = []
    for i in range(length):
        s = genRandomStr(regex)
        res.append(s)
    return res

def readRegexsFromFile(filename):
    return [line.rstrip('\n') for line in open(filename)]

def alreadyExist(filename):
    return os.path.exists(filename)

def generateTestFiles(sourcefile):
    regexs = readRegexsFromFile(sourcefile)
    index = 1
    for re in regexs:
        filename = targetFolder + "data" + str(index) + ".txt"
        results = genRandomStrLst(re, targetNumber)

        if alreadyExist(filename):
            op = "w"
        else:
            op = "a+"
        with open(filename, op) as f:
            f.write("%s\n" % re)
            for item in results:
                f.write("%s\n" % item)
        index = index + 1

if __name__ == "__main__":
    generateTestFiles("regex-data.txt")


