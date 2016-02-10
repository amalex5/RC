

import operator,pdb
import inspect


def product(a,b,symbol):
	return ['+',['*',a,differentiate(b,symbol)],['*',differentiate(a,symbol),b]]

def add(a,b,symbol):
	return ['+',differentiate(a,symbol),differentiate(b,symbol)]

def difference(a,b,symbol):
	return ["+",differentiate(a,symbol),negate(b,symbol)]

def negate(a,symbol):
	return ['-',differentiate(a,symbol)]

def sin(a,symbol):
	return ['*',['cos',a],differentiate(a,symbol)]

def power(a,b,symbol):
	return str(b) + str(a) + "^" + str(int(b) -1)



ops = {"+": add,
		"*": product,
		"^": power,
		"sin": sin,
		"-": difference
		}

def differentiate(expr,symbol):
	if type(expr) == str:
		if expr == symbol: return 1
		else: return 0
	elif type(expr) == list:
		operation = ops[expr[0]]
		number_of_args = len(inspect.getargspec(operation).args)
		if number_of_args == 3:
			try:
				return operation(expr[1],expr[2],symbol)
			except: pdb.set_trace()
		else:
			return operation(expr[1],symbol)
	#return ops[input[0]](calc(input[1]),calc(input[2]))

def printSExpr(sexpr):
	def printer(triple):
		if type(triple) == list:
			if len(triple) == 3: return "(" + str(printer(triple[1])) + str(triple[0]) + str(printer(triple[2])) + ")"
			else: return  str(printer(triple[0])) + str(printer(triple[1]))

		else: return triple

	output = printer(sexpr)
	return output

#input = ['+',['*','a',['^','x','5']],['-',['sine',['^','x',7]]]]

input = [ '+', ['*', 'a', ['^','x','5'] ] , ['*', 'x', ['sin',['*','2','x']] ] ]

#print printSExpr(differentiate(["*", "y",['^',"x",4]],'x'))

print differentiate(input,'x')
print "function: " + printSExpr(input)
print "its derivative: " + printSExpr(differentiate(input,'x'))





