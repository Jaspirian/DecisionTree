"""
This decision tree creator parses a file containing representatives, their party alignment, and their votes.
"""
__author__ = "Jasper Raynolds"
__license__ = "MIT"
__date__ = "March 2018"

import re
import math

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

def entropy(representatives):
	"""
	Determines the entropy for a system of given representatives,
	based on their political affiliations.
	We use the formula entropy(S) = -p(A)log(p(A)) - p(B)log(p(B)).
	We use log base 2 in order to measure information in bits.
	"""
	affiliations = {D:0, R:0}
	for rep in representatives:
		affiliations[rep.party] += 1
	probD = affiliations["D"] / len(affiliations)
	probR = affiliations["R"] / len(affiliations)
	# We accept 0 * log(0) as 0, not undefined.
	# To escape domain issues, we set probabilities to 1 if they are 0. 
	if probD == 0:
		probD = 1
	if probR == 0:
		probR = 1

	entropy = -probD * math.log(probD, 2) - probR * math.log(probR, 2)

	return entropy

def information_gain(new_group, old_group):
	"""
	Calculates the infomation gain of a new split.
	We use the formula gain(S) = entropy(S) - (Sum(Si/S) * entropy(Si))
	"""
	new_proportion = len(new_group) / (len(new_group) + len(old_group))
	old_proportion = 1 - new_proportion 
	gain = entropy(old_group) - ((new_proportion * entropy(new_group)) + (old_proportion * entropy(old_group)))

	return gain

print(1 * math.log(1,2))
# print(1 * math.log(0, 2))
rep = line_to_representative(read_file("voting-data.tsv")[0])
print(rep.party)
print(rep.votes)
