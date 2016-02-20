// linkedlist.js
// an implementation of a (pseudo) linked list in javascript
// using an array as "memory"

var linkedListNode = function(){

}


var linkedList = function(){
	this.array = []
	this.firstPointer

	this.prepend = function(obj){
		// get new random pointer
		var pointer = getNewPointer()
		var oldFirstPointer = this.firstPointer
		this.array[pointer] = {val: val; next: firstPointer}
		this.firstPointer = pointer

	}

	this.getNewPointer = function(){


	}


}

//if after five tries you can't insert
//double the array length
//array.length = array.length * 2

