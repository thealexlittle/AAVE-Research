import webvtt

fp = 'rawtext/Steve.en.vtt'

vtt = webvtt.read(fp)
lines = vtt[0].text.split(" ")

chunk_size = 7
lc = [lines[i:i + chunk_size] for i in range(0, len(lines), chunk_size)]

res = 'cleantext/steve.txt'

with open(res, 'w') as f:
    for c in lc:
        l = ""
        for w in c:
            l = l + " " + w
        f.write(l + "\n")