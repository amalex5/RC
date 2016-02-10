
import operator,pdb

ops = {"+": operator.add,
		"*": operator.mul,
		"-": operator.sub,
		"/": operator.div}


def calc(input):
	if type(input) == list: 
		return ops[input[0]](calc(input[1]),calc(input[2]))
	else: return int(input)



print calc([ "*", ["+","5","4"], "7"])