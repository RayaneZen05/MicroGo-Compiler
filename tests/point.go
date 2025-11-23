package main;
import "fmt";

type point struct { x,y int };

// Résultat attendu 10

func main() {
     a := new(point);
     b := new(point);
     a.x = 6;
     a.y = 2;
     b.x = 3;
     b.y = 4;
     fmt.Print(a.x + a.y + b.x + b.y)
};