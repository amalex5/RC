// pseudo hash table implementation in js

var hashTable = function(hashFunction,size) {
	this.hashFunction = hashFunction(size)
	this.size = size
	this.array = new Array(this.size) // we don't really need to initialize it in JS, but...

	this.hashedKey = function(unhashedKey){
		return this.hashFunction(unhashedKey)
		}
	this.insert = function (unhashedKey,val){
		this.array[loc] = {unhashedKey: unhashedKey, val: val}
		// BUT WHAT IF THERE'S ALREADY SOMETHING THERE?!?!?
		return this
		}
	this.lookup = function(unhashedKey){
		return this.array[this.hashedKey(unhashedKey)].val
		}
	this.remove = function(unhashedKey,val){
		this.array[this.hashedKey(unhashedKey)] = null
		return this
		}

	return this
}

// we make some changes and extend our base hash table
var collidableHashTable = function(hashFunction,size) {
	this.hashFunction = hashFunction(size)
	this.size = size
	this.array = new Array(this.size) // we don't really need to initialize it in JS, but...

	this.hashedKey = function(unhashedKey){
		return this.hashFunction(unhashedKey)
		}
	this.insert = function (unhashedKey,val){
		loc = this.hashedKey(unhashedKey)
		if (this.array[loc]){
			// deal with collisions!
			currentContents = this.array[loc]

			this.array[loc] = new linkedList()

			}
		else {
			this.array[loc] = {unhashedKey: unhashedKey, val: val}
			}
		return this
		}
	this.lookup = function(unhashedKey){
		return this.array[this.hashedKey(unhashedKey)].val
		}
	this.remove = function(unhashedKey,val){
		this.array[this.hashedKey(unhashedKey)] = null
		return this
		}

	return this
}


var myStupidHashFxn = function(size){
	return function(obj){ 
		return obj.split('')
		.map(function(e){return e.charCodeAt(0)})
		.reduce(function(prev,curr){return prev+curr}) % size
	 }
 }

 var demo = function(){

 	// initialize a new hash table with 45 buckets and the stupid hash fxn
 	var myTable = new hashTable(myStupidHashFxn,45)

 	// put something in the table
 	myTable.insert("Andrew Alexander","6075924759")

 	// what was its key?
 	myTable.hashedKey("Andrew Alexander")

 	// search for it there
 	myTable.lookup("Andrew Alexander")

 	// remove it
 	myTable.remove("Andrew Alexander")





 }