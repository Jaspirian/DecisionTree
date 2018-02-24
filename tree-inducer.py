"""
This decision tree creator parses a file containing representatives, their party alignment, and their votes.
"""
__author__ = "Jasper Raynolds"
__license__ = "MIT"
__date__ = "March 2018"

import re

class Representative:
	def __init__(self, party, votes):
		"""
		Constructor. Takes a Char for party (D, R) and a dictionary of integer-Char pairs for votes.
		"""
		self.party = party
		self.votes = votes

def read_file(fileLoc):
	"""
	Reads a file into a list of strings.
	"""
	file = open(fileLoc, "r")
	lineList = []
	for line in file:
		lineList.append(line)
	file.close()

	return lineList

def line_to_representative(line):
	"""
	Converts a line string into a Representative object.
	"""
	matcher = re.match(r'Rep-.*([RD])', line)
	party = matcher.group(1)
	matcher = re.match(r'Rep-.*?([\+\-\.]+)', line)
	votes = list(matcher.group(1))
	voteDict = {}
	for index in range(len(votes)):
		voteDict[index] = votes[index]
	return Representative(party, voteDict)
