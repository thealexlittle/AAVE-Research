import csv
import re
import webvtt
from datetime import date



# Populate Occurence of morpheme

def captureSentence(i, captions):
    punc = ".!?"

    # Read line until punctuation found
    s = ""
    for j in range(i,len(captions)):
        rt = captions[j].text
        if rt.isnumeric():
            continue
        t = re.sub(r'\d+$', '', rt)
        s = s + " " +t
        for p in punc:
            if p in s:
                print(i,j)
                return (j,s)
    #Return sentence 


def findTimeStamps_DN(fp):
    negation_dict = ['No ', 'Not ', 'None', 'No one', 'Nobody', 'Nothing', 'Neither', 'Nowhere', 'Never', 'Hardly', 'Scarcely', 'Barely', 'Doesn`t', 'Isn`t', 'Wasn`t', 'Shouldn`t', 'Wouldn`t', 'Couldn`t', 'Won`t', 'Can`t', 'Don`t']

    lines = []
    captions = webvtt.read(fp)
    for i in range(len(captions)):
        #text = caption.text.splitlines()[0]
        ts = captions[i].start
        d = captureSentence(i, captions)
        i = d[0]+1
        text = d[1]
        
        # Skip music notes
        if "♪" in text:
            continue
        
        # Check if any negation words are in the line
        c = 0 
        for n in negation_dict:
        # Add Line if it occurs, (maybe code for occurences of multiple)
            if n.lower() in text.lower():
                c = c + 1
            
            if c >= 2:
                break
        
        # Store result in tuple, (ts,t,oc)
        if c == 1:
            lines.append((ts, text, 0))
        elif c >= 2:
            lines.append((ts, text, 1))
        else: 
            continue

    return lines    


def findTimeStamps_TH(fp):
    lines = []
    for caption in webvtt.read(fp):
        text = caption.text.splitlines()[0]
        #print(text)
        # Skip music notes
        if "♪" in text:
            continue

        #Get number of occurences from regex
        p = '^[Tt][Hh]| [Tt][Hh]'
        c = len(re.findall(p,text))
        for i in range(c):
            t = (caption.start, text, 0)
            lines.append(t)
    return lines

def csvData(lines, d, n):
    fp = f'data_template_{n}.csv'
    with open(fp, "w") as csvfile:
        fieldnames = ['RESPONSE', 'RACE_INT', 'SUBJECT', 'AGE', 'GENDER', 'DATE', 'TIMESTAMP', 'LINE']
        writer = csv.DictWriter(csvfile, fieldnames)

        writer.writeheader()
        for line in lines:
            ts = line[0]
            cap = line[1]
            occ = line[2]
            #print(cap)
            writer.writerow({
                fieldnames[0]: occ, 
                fieldnames[1]: 'black', # race of interviewee
                fieldnames[2]: 'marriage', # subject
                fieldnames[3]: 53, # age of interviewee
                fieldnames[4]: 'male', # gender of interviewee
                fieldnames[5]: d.isoformat(),    # date of airing
                fieldnames[6]: ts, # timestamp of occurence
                fieldnames[7]: cap
            })

fp = ["cleantext\WillSmith.vtt", "cleantext\Maya_Angelou_Interview_(2013-5-19).en.vtt"]
d = date(2021,10,13)

ts = findTimeStamps_DN(fp[1])
csvData(ts,d,"DN")

""