

operators = ['*','/','+','-']




def parse(expr,operatorStack,operandStack):

	for char in expr:
		if char == ' ':
			pass
		elif char in operators:
			print "operator stack: " + str(operatorStack)
			if operatorStack == []:
				operatorStack.append(char)
			elif operators.index(operatorStack[-1]) < operators.index(char):
				print char + " has lower precedence than " + operatorStack[-1]
				operatorStack.append(char)
			else:
				print char + " has higher precedence than " + operatorStack[-1]

				operatorStack.append(char)
				#operatorStack.append(char)
		else:
			# if operatorStack != [] and operandStack != []:
			# 	temp = [char,operandStack.pop()]
			# 	temp.reverse()
			# 	operandStack.append([operatorStack.pop()] + temp )


			# else: 
			operandStack.append(char)


		print "operator stack: " + str(operatorStack)
		print "operand stack: " + str(operandStack)
		#print "output: " + str(output)
		print "\n\n"

		#operators.push(char)

		# temp2 = [operandStack.pop(),operandStack.pop()]
		# temp2.reverse()
		# operandStack.append([operatorStack.pop()] + temp2 )
		#output += operatorStack.pop()


	print "original expression: " + expr
	#print "prefix version: " + str(output)

	#return operandStack

parse("a + b * c * d + e",[],[])

