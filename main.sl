laile x = 0;
fun logic(){
    while (x < 5) {
        if (x == 2){
            x++;
            continue;
        }
        if (x == 4){
            break;
        }
        x++;
    }
    return;
}

println(logic());
println("why");