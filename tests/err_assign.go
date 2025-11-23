package main;
import "fmt";

func main() {
    // Erreur : assignation à une r-value (constante)
    // assigning an rvalue to a non-lvalue
    10 = x;

    // Erreur : variable non déclarée
    // La variable y n'est pas déclarée
    y = 5;

    // Erreur : incrémentation sur un type non int (ou non lvalue)
    var b bool = true;
    b++;
};