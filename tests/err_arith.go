package main;
import "fmt";

func main() {
    var x int = 10;
    var b bool = true;

    // Erreur : addition d'un int et d'un bool
    // expected int, got bool
    fmt.Print(x + b); 

    // Erreur : conditionnelle avec un int au lieu d'un bool
    // expected bool, got int
    if (x) { 
        fmt.Print("Ceci ne devrait pas passer");
    };

    // Erreur : opération logique sur des entiers
    // expected bool, got int
    var res bool = (1 && 0);
};