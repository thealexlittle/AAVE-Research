import csv
import re
import webvtt
from datetime import date



# Populate Occurence of morpheme

def findTimeStamps_TH():
    lines = []
    for caption in webvtt.read("cleantext/WillSmith.vtt"):
        text = caption.text.splitlines()[0]
        print(text)
        # Skip music notes
        if "♪" in text:
            continue

        #Get number of occurences from regex
        p = '^[Tt][Hh]| [Tt][Hh]'
        c = len(re.findall(p,text))
        for i in range(c):
            t = (caption.start, text)
            lines.append(t)
    print(lines)
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
            print(cap)
            writer.writerow({
                fieldnames[0]: 0, 
                fieldnames[1]: 'black', # race of interviewee
                fieldnames[2]: 'marriage', # subject
                fieldnames[3]: 53, # age of interviewee
                fieldnames[4]: 'male', # gender of interviewee
                fieldnames[5]: d.isoformat(),    # date of airing
                fieldnames[6]: ts, # timestamp of occurence
                fieldnames[7]: cap
            })

d = date(2021,10,13)
ts = findTimeStamps_TH()
csvData(ts,d,"TH")


