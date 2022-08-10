from html import entities
import nltk

line = "We don't be going out like that."
line2 = "We don't go out like that."
line3 = "We not doing this."

lines = [
    "We don't be going out like that.", 
    "We don't go out like that.",
    "We not doing this.",
    "He not there.",
    "We will be here tomorrow."
    ]

def doStuff(line):
    print(line)
    tokens = nltk.word_tokenize(line)
    print(tokens)
    tagged = nltk.pos_tag(tokens)
    print(tagged[0:6])
    entities = nltk.chunk.ne_chunk(tagged)
    print(entities)

for l in lines:
    doStuff(l)
