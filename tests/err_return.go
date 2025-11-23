package main;
import "fmt";

// Erreur : A return statement is missing in function body
// (Car le chemin 'else' implicite ne retourne rien)
func abs(n int) int {
    if (n < 0) {
        return -n;
    }
};

func main() {
    fmt.Print(abs(-5));
};