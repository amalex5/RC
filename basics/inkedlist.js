// linkedlist.js
// an implementation of a (pseudo) linked list in javascript
// using an array as "memory"

var linkedListNode = function(){

}


// skip list

var linkedList = function(){
	this.array = []
	this.firstPointer

	this.prepend = function(obj){
		// get new random pointer
		var pointer = getNewPointer()
		var oldFirstPointer = this.firstPointer
		this.array[pointer] = {val: val; next: firstPointer}
		this.firstPointer = getNewPointer("nope")

	}

	this.getNewPointer = function(){
		var pointer = null

	}

	this.gnpHelper = function (tmpPointer){
		if (tmpPointer == "nope"){
			var newTmpPointer
			for (var i=0; i<5; i++){
				newTmpPointer = Math.floor(Math.random * this.array.length)
				if (typeof this.array[tmpPointer] == 'undefined') {
					this.array.length = this.array.length * 2
					gnpHelper("nope")
					}
				else{ 

				}
			}

		else {

		}

	}


}

//if after five tries you can't insert
//double the array length
//array.length = array.length * 2

