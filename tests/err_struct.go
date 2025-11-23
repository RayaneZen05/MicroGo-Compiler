package main;
import "fmt";

type Point struct { x, y int };

func main() {
    p := new(Point);
    
    // Erreur : champ inexistant 'z'
    // type not found in struct
    p.z = 10;

    // Erreur : new sur une structure inconnue
    // Unknown structure Vaisseau
    v := new(Vaisseau);

    // Erreur : comparaison nil == nil interdite
    // Comparaison entre Nil et Nil
    if (nil == nil) {
        fmt.Print("Impossible");
    };

    // Erreur : accès à un champ sur autre chose qu'un pointeur de structure
    var i int = 0;
    i.x = 1;
};