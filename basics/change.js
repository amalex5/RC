

coins = [
	{name: "penny", val: 1},
		{name: "nickel", val: 5},
	{name: "dime", val: 10},
	{name: "quarter", val: 25}
]

changeList = [ ]

var coinsLessThan = function(amount){
	return coins.filter(function(c){return c.val <= amount})
}

var changeSum = function(coinList){
	return coinList.reduce(function(curr,prev){
		return prev + curr.val
	}, 0)
}

// takes a single list of coins and returns several
var addCoin = function(coinList,amount){
	newCoinLists = []
	newCoinLists = coinsLessThan(amount - changeSum(coinList)).map(function (c){return coinList.concat([c]) }  )
	return newCoinLists
}

var makeChange = function(amount){
	if (amount == 0){
		pass
	}

	else {

		incompleteLists = coinLists.filter(function(c){return changeSum(c) < amount})
		


		for (var i = 0; i <changeList.length; i++)
			coinsLessThan( totalAmount - changeSum(l) ).map


		})

		coinsLessThan(amount)

	}

}