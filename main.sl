fun isEven(n){
    if (n % 2 == 0){
        return 1;
    }
    return 0;
}
println("Program for checking if number is even or odd");

while(1){
    print("Enter a value : ");
    laile x = input();
    
    if(x < 0){
        break;
    }

    if(isEven(x)){
        println("x is even");
    }else {
        println("x is odd");
    }
}

