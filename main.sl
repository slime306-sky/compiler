laile a = 1;
laile b = 0;

if(a && b){
    println("should not print");
}
else {
    println("correct: false &&");
}

if (a || b){
    println("correct: true ||");
}