
var wordsDictionary = // all valid words
var phoneDict = { 2: ['a','b','c'],
				3: ['d','e','f'],
				4: ['g','h','i'],
				5: ['j','k','l'],
				6: ['m','n','o'],
				7: ['p','q','r'],
				8: ['s','t','u'],
				9: ['v','w','x'],
				}

var appender = function(seeds,number){
	
	if (number = []) {
		// see what words are dictionary compliant
		return seeds.filter(function(c) {return c in wordsDictionary})

	 }
	
	else{

		var currentDigit = number[0]
		var newSeeds = []

		for (var i in seeds){
			stem = seeds[i]
			newSeeds.push(
				stem + phoneDict[currentDigit][0],
				stem + phoneDict[currentDigit][1],
				stem + phoneDict[currentDigit][2]
				)
			}

			appender(newSeeds, number.slice(1))
		
	}
}