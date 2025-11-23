package main;
import "fmt";

func add(a, b int) int {
    return a + b;
}

func voidFunc() {
    return;
}

func main() {
    // Erreur : mauvais type d'argument
    // expected int, got bool
    add(1, true);

    // Erreur : mauvais nombre d'arguments
    // wrong number of arguments for function add
    add(1);

    // Erreur : utilisation d'une fonction void dans une expression
    // function voidFunc returns void
    var x int = 10 + voidFunc();
}