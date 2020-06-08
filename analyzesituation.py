emotionharm = []
physharm = []

situationWords = []


with open("situation.txt", "r") as situation:
    for line in situation:
    situationWords.extend(line.split())
    
    
def list_check(situation, harmlist):
    for word in situation:
        if word in harmlist:
            return True
    return False
    
emotionCheck = list_check(situationWords, emotionharm)
physcialCheck = list_check(situationWords, phsyharm)

if emotionCheck == True or phsyicalCheck == True:
    """this is where an acute stress response reaction would be initiated"""