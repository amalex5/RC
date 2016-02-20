// random walk



var randomWalk = function(steps){
	var listOfPositions = [0]
	for (var step = 0; step < steps; step ++){
		currentPosition = listOfPositions[listOfPositions.length -1]
		var move
		(Math.random() >= .5) ?  move = 1 :  move = -1
		listOfPositions.push(currentPosition + move)
			}
	return listOfPositions

}