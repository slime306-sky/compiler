laile x = 0;

while (x < 5) {
    if (x == 2){
        x++;
        continue;
    }
    if (x == 4){
        break;
    }
    println(x);
    x++;
}