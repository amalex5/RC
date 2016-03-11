// takes a decimal integer >0 and turns it into a roman numeral!

var repeater = function (inp, n){
    // basic helper fxn. takes inp and returns n copies of it.
    var output = []
    for (var i = 0; i < n; i ++){
        output.push(inp)
    }
    return output.join('')   
}

var orderDict = {
    1  : {
        1: "I",
        5: "V",
        10: "X"
    },
    2: {
        1: "X",
        5: "L",
        10: "C"
    },
    3 : {
        1: "C",
        5: "D",
        10: "M"
    },
    4 : {
        1: "M",
        5: "MMMMM",
        10: "MMMMMMMMMM"
    }
    
}

var digitize = function (input){
    // turns a decimal number into an array of its digits, in reverse order
    return digitize1(input,[])
}

var digitize1 = function(input,arr){
    // recursive helper function for digitize
    if (input === 0){
        return arr.reverse()
    }
    else {
        arr.push(input % 10)
        return digitize1( Math.floor(input / 10), arr )    
    } 
}

var romanizeDigit = function (digit,order){
    // takes in a digit and an order of magnitude and returns its roman numeral representation
    thisOrder = orderDict[order]
    if (digit < 4){
        return repeater(thisOrder[1],digit)
    }
    else if (digit == 4){
        return thisOrder[1] + thisOrder[5]
    }
    else if ( digit > 4 && digit < 9){
        return thisOrder[5] + repeater(thisOrder[1], digit - 5 )
    }
    else if (digit ==  9){
        return thisOrder[1] + thisOrder[10]
    }
    
}

var romanize = function (input){
    // takes in a decimal number and returns its roman numeral representation
    var digits = digitize(input)
    var output = ''
    for (var i = digits.length; i > 0; i--){
        // concatenate the romanized representation of each digit
        order = i
        digit = digits[i - 1] // subtracting because array indexes start at zero
        //console.log("i",i,"; digit",digit,"; order",order,"; digit array: ",digits)
        output += romanizeDigit(digit,order)
     }
    return output
}


var test = function(n) {
    for (var i = 1; i < n; i++){
    console.log(i, romanize(i))
    }
}



