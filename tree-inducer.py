"""
This decision tree creator parses a file containing representatives, their party alignment, and their votes.
"""
__author__ = "Jasper Raynolds"
__license__ = "MIT"
__date__ = "March 2018"

import math
import re
import string
from collections import OrderedDict
import sys

################ FILE and INPUT FUNCTIONS ################

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
	votes = tuple(matcher.group(1))
	return Representative(party, votes)

################ REPRESENTATIVE and REPRESENTATIVES FUNCTIONS ################

class Representative:
	"""
	A convenience object to hold a congressional representative's party and voting history.

	Attributes:
		party:	A char denoting the party affiliation of the rep. "D" or "R."
		votes:	A tuple of characters ("+","-",".") denoting this reps's votes on issues.
	"""
	def __init__(self, party, votes):
		"""
		Constructor. Takes a Char for party (D, R) and a tuple of characters (+, -, .) for votes.
		"""
		assert (party == "D" or party == "R"), "Sorry, we only accept a two-party system."
		self.party = party
		assert (type(votes) is tuple or type(votes) is str), "Votes must be a tuple."
		assert (all(vote == "+" or vote == "-" or vote == "." for vote in votes)), "Votes must only contain +, - and . characters"
		self.votes = votes

	def test_on_node(self, node):
		"""
		Tests this Representative against a Node. Does this rep's vote on the node's issue match? 
		If not, return False. If so, keep going. If we reach the end and it still matches, return True.
		"""
		if not node:
			return False
		if node.issue != None:
			vote = self.votes[node.issue]
			return self.test_on_node(node.children[vote])
		else :
			parties = split_around_parties(node.reps)
			if len(parties["D"]) > len(parties["R"]):
				return self.party == "D"
			elif len(parties["R"]) > len(parties["D"]): 
				return self.party == "R"
			else :
				return False

def split_around_parties(reps):
	"""
	Returns a dictionary of party, representative-list pairs, split
	around the parties of the given representatives.
	"""
	parties = {"D":[], "R":[]}

	for rep in reps:
		parties[rep.party].append(rep)

	return parties

def split_around_positions(reps, issue):
	"""
	Returns a dictionary of vote, representative-list pairs, split
	around the positions the representatives took on a given issue.
	"""
	positions = OrderedDict((("+",[]), ("-",[]), (".",[])))

	for rep in reps:
		if rep.votes[issue] in positions:
			positions[rep.votes[issue]].append(rep)

	return positions

def get_best_issue(reps, ignored=[]):
	"""
	Finds the best issue to split around for a given list of
	representatives. Ignores already visited issues.
	Returns None in several cases where a split isn't useful.
	"""
	parties = split_around_parties(reps)

	# Return None if either party is empty.
	if any(not voters for voters in parties.values()):
		return None

	# Return None if all voters are exact duplicates.
	indistinguishable = True
	for issue in range(len(reps[0].votes)):
		if issue in ignored:
			continue

		if not all_share_position(reps, issue):
			indistinguishable = False
			break
	if indistinguishable:
		return None

	# Find the best issue
	best_issue = (None, -float("inf"))
	num_issues = len(reps[0].votes)
	for issue in range(num_issues):
		# Skip the issues already split on.
		if issue in ignored:
			continue

		# Skip the issues all are agreed on.
		if all_share_position(reps, issue):
			continue

		positions = split_around_positions(reps, issue)

		for position,voters in positions.items():
			if voters:
				gain = information_gain(reps, voters)
				if gain > best_issue[1]:
					best_issue = (issue, gain)

	return best_issue[0]

def all_share_position(reps, issue):
	"""
	Returns true if all representatives passed voted the same
	way on a given issue.
	"""
	position = reps[0].votes[issue]

	votes = []
	for rep in reps:
		votes.append(rep.votes[issue])

	return all(vote == position for vote in votes)

################ MATH and EVALUATION FUNCTIONS ################

def entropy(reps):
	"""
	Determines the entropy for a system of given representatives,
	based on their political affiliations.
	We use the formula entropy(S) = -p(A)log(p(A)) - p(B)log(p(B)).
	We use log base 2 in order to measure information in bits.
	"""
	affiliations = {"D":0, "R":0}
	for rep in reps:
		affiliations[rep.party] += 1
	probD = affiliations["D"] / len(reps)
	probR = affiliations["R"] / len(reps)
	# We accept 0 * log(0) as 0, not undefined.
	# To prevent domain issues, we set probabilities to 1 if they are 0. 
	if probD == 0:
		probD = 1
	if probR == 0:
		probR = 1

	entropy = -probD * math.log(probD, 2) - probR * math.log(probR, 2)

	return entropy

def information_gain(reps, previous_reps):
	"""
	Calculates the infomation gain of a new split.
	We use the formula gain(S) = entropy(S) - (Sum(Si/S) * entropy(Si))
	"""
	new_proportion = len(reps) / (len(previous_reps) + len(previous_reps))
	old_proportion = 1 - new_proportion 
	gain = entropy(previous_reps) - ((new_proportion * entropy(reps)) + (old_proportion * entropy(previous_reps)))

	return gain

def get_matches(base, reps):
	"""
	Compares the given list of Representatives with the Node base (and all its children) given.
	Returns a dictionary of boolean-Representative list pairs.
	"""
	matches = {True:[], False:[]}

	for rep in reps:
		# print(rep.party,rep.votes)
		matches[rep.test_on_node(base)].append(rep)

	return matches

def get_percentage(num_correct, num_incorrect):
	"""
	Returns the percentage correct given a number of correct and incorrect values.
	"""
	return (num_correct / (num_correct + num_incorrect)) * 100

################ TREE FUNCTIONS ################

class Node:
	"""
	A node point connected to its parent and its children, part of a decision tree.

	Attributes:
		parent: 	The parent Node this node was grown from.
		reps:		The list of Representatives at this node.
		issue:		The best issue to split this Node upon--either an integer or None.
		children:	The three possible children from this issue: for (+), against (-), or neutral (.).
		depth:		The number of nodes deep. 1 means it has 1 parent.
	"""
	def __init__(self, parent, reps, depth):
		"""
		Constructor. Takes a node parent (may be None) and a list of Representatives, and the depth of the node.
		Then gets the best issue, grows children and prunes ties.
		"""
		self.parent = parent
		self.reps = reps
		self.depth = depth

		# Can we find the best issue for this node?
		self.issue = self.get_issue()

		# For each split along the issue that has representatives fitting it, grow a child.
		self.children = OrderedDict((("+",None), ("-",None), (".",None)))
		if self.issue != None:
			self.grow_children()

		# If a child is an unsplittable tie, travel back up the tree.
		self.trim_equal_splits()

	def grow_children(self):
		"""
		Splits this Node's Representatives around this Node's issue and, if a given
		position has voters, creates a new Node at the child.
		"""
		for pos,voters in split_around_positions(self.reps, self.issue).items():
			if voters:
				self.children[pos] = Node(self, voters, self.depth + 1)

	def get_issue(self):
		"""
		Finds and returns the best non-visited issue to split
		the node's representatives on. Returns None if only one party is represented.
		"""
		# Is this a terminal node?
		parties = split_around_parties(self.reps)
		if not parties["D"] or not parties["R"]:
			return None

		# Can we find a best issue to split on?
		visited = []
		if self.parent:
			visited = self.parent.get_visited_issues([])
		issue = get_best_issue(self.reps, visited)
		return issue

	def to_string(self, output="", tabs=0, position=None):
		"""
		Recursively adds to an output string this node and its children.
		If this node has an issue and some children, it should be "Issue _"
		If this node has no issue and no children, it should be a party, or majority.
		"""
		indent = "\t" * tabs
		output += indent
		if position:
			output += position + " "
		parties = split_around_parties(self.reps)
		if self.issue != None:
			output += "Issue {0}:\n".format(string.ascii_uppercase[self.issue])
		else :
			if not parties["D"]:
				output += "R"
			elif not parties["R"]:
				output += "D"
			elif len(parties["D"]) > len(parties["R"]):
				output += "D majority."
			elif len(parties["R"]) > len(parties["D"]):
				output += "R majority."
			elif len(parties["D"]) == len(parties["R"]):
				output += "Even split."
			return output + "\n"

		for pos,child in self.children.items():
			if not child:
				continue
			output += child.to_string("", tabs+1, pos)

		return output

	def get_visited_issues(self, visited):
		"""
		Recursively returns a list of issues already visited and split
		at this node and its parents.
		"""
		visited.append(self.issue)

		if self.parent:
			return self.parent.get_visited_issues(visited)

		return visited

	def trim_equal_splits(self):
		"""
		If any of this Node's children are terminal nodes with an even split,
		makes this Node a terminal node.
		"""
		for position,child in self.children.items():
			if child:
				parties = split_around_parties(child.reps)
				if child.issue == None:
					parties = split_around_parties(child.reps)
					if len(parties["D"]) == len(parties["R"]):
						# Make this Node a terminal one.
						self.children = OrderedDict((("+",None), ("-",None), (".",None)))
						self.issue = None
						return

def get_all_nodes(node, nodes):
	"""
	Recursively returns all child nodes from a given node.
	"""
	children = node.children.values()
	nodes.append(node)

	for child in children:
		if child:
			nodes.extend(get_all_nodes(child, []))

	return nodes

def create_tree(reps, interval):
	"""
	Creates and prunes a decision tree using the list of Representatives given.
	The tuning set is every Rep at the given interval. The training set is all reps besides those.
	"""
	training_set = []
	tuning_set = []

	for rep in reps:
		if reps.index(rep) % interval == 0:
			tuning_set.append(rep)
		else :
			training_set.append(rep)

	tree = Node(None, training_set, 0)
	prune_tree(tree, tuning_set)

	return tree

def prune_tree(base, tuning_set):
	"""
	Recursively prunes the tree at the Node base given.
	Finds the node where, if it was removed, the tree would have similar or greater accuracy,
	and removes the node. Then recurses. If no such node is found, exits.
	"""
	nodes = get_all_nodes(base, [])
	# print(nodes)

	BLANK_DICT = {"+":None, "-":None, ".":None}

	beginning_matches = get_matches(base, tuning_set)
	beginning_percentage = get_percentage(len(beginning_matches[True]), len(beginning_matches[False]))
	# Initialize the best prune we can make. A tuple containing the Node and its percentage.
	best_prune = (None, beginning_percentage)

	for node in nodes:
		if node.issue != None:
			# test removing this
			# Get the unaltered values
			unaltered_dict = node.children
			unaltered_issue = node.issue
			# Clear them
			node.children = BLANK_DICT
			node.issue = None
			# Test it
			test_matches = get_matches(base, tuning_set)
			test_percentage = get_percentage(len(test_matches[True]), len(test_matches[False]))
			# Compare it
			if test_percentage > best_prune[1]:
				best_prune = (node, test_percentage)
			# Only accept equal-valued prunes if they're closer to the base of the tree.
			if test_percentage == best_prune[1]:
				if best_prune[0] == None or best_prune[0].depth > node.depth:
					best_prune = (node, test_percentage)
			# Reset
			node.children = unaltered_dict
			node.issue = unaltered_issue

	# If we've got a prune to make that's better or equal to our current state:
	pruned = best_prune[0]
	if pruned:
		# Clear that node
		pruned.children = BLANK_DICT
		pruned.issue = None
		# Recurse
		prune_tree(base, tuning_set)
	else :
		return

def leave_one_out_cross_validation(reps):
	"""
	Loops through the set of Representatives given, leaving one out and creating a tree
	then testing the left-out Representative on the tree.
	Returns the percentage of Representatives that pass that test.
	"""
	matches = {True:[], False:[]}	

	for rep in reps:
		print(len(reps) - reps.index(rep), "tests remaining.")
		left_out = list(reps)
		del left_out[reps.index(rep)]

		test_tree = create_tree(left_out, 4)
		does_match = rep.test_on_node(test_tree)
		matches[does_match].append(rep)

	return get_percentage(len(matches[True]), len(matches[False]))

################ ACTUAL INPUT ################

TESTING_INTERVAL = 4

file = sys.argv[1]
lines = read_file(file)
reps = []
for line in lines:
	reps.append(line_to_representative(line))

tree = create_tree(reps, TESTING_INTERVAL)
print(tree.to_string()[:-1])
print("Performing leave-one-out cross validation...")
print(leave_one_out_cross_validation(reps),"% correct.")