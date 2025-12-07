[//]: # (Membres :)
[//]: # (- Alexandru DRAGULINESCU)
[//]: # (- Rayane ZENAIDI)

# Projet de Compilation MicroGo

Ce projet implémente un compilateur pour le langage MicroGo, un sous-ensemble du langage Go. Il est divisé en deux parties principales : le frontend (analyse lexicale, syntaxique et sémantique) et le backend (génération de code MIPS).

## Partie 1 : Analyse Syntaxique et Sémantique

Cette première phase transforme le code source MicroGo en un Arbre de Syntaxe Abstraite (AST) typé et vérifié.

### 1\. Fonctionnalités Implémentées

####  Analyse Lexicale (`mgolexer.mll`)

  * **Reconnaissance des tokens :** Mots-clés, identifiants, littéraux (entiers, chaînes, booléens), opérateurs.
  * **Insertion Automatique de Point-Virgule :** Implémentation fidèle de la spécification Go. Le lexer gère un état interne pour insérer virtuellement des `SEMI` après les identifiants, nombres, et parenthèses/accolades fermantes suivis d'un saut de ligne.

####  Analyse Syntaxique (`mgoparser.mly`)

  * **Gestion de la portée des variables :** Pour gérer la portée lexicale des variables (le fait qu'une variable déclarée dans `var x` soit visible dans la suite du bloc), les règles d'instructions renvoient des fonctions (`seq -> seq`). Cela permet de construire l'arbre "de bas en haut" en imbriquant la suite du programme à l'intérieur des nœuds de déclaration (`Vars`).
  * **Sucre Syntaxique :**
      * Transformation des boucles `for` (avec init, cond, step) en blocs `while` équivalents.
      * Gestion unifiée de `new(T)` et des appels de fonctions.
      * Traitement spécifique de `fmt.Print` comme une primitive variadique (accepte tout type).
  * **Priorités des opérateurs :** Respect des priorités arithmétiques et logiques standard via les directives `%left` et `%nonassoc`.

####  Vérification de Types (`typechecker.ml`)

  * **Passe en 3 étapes :**
    1.  Collecte des structures (vérification des doublons de champs).
    2.  Collecte des signatures de fonctions.
    3.  Vérification des corps de fonctions.
  * **Inférence de types :** Support de l'opérateur walrus `:=` et de `var x = e`. Le typechecker déduit le type de la variable à partir de l'expression d'initialisation.
  * **Typage strict :** Vérification rigoureuse des types (pas de conversion implicite `int` \<-\> `bool`), vérification des arités de fonctions et des types de retour à l'aide des jugements de type fournis dans le sujet.
  * **Analyse de contrôle de flux :** Vérifie statiquement que toutes les branches d'une fonction non-void se terminent par un `return`.

### 2\. Tests Ajoutés

Une suite de tests spécifiques a été créée pour valider le comportement du typechecker face aux erreurs :

  * `tests/err_arith.go` : Teste les incompatibilités de types dans les opérations binaires (ex: `1 + true`).
  * `tests/err_struct.go` : Teste l'accès à des champs inexistants et l'instanciation de structures inconnues.
  * `tests/err_func.go` : Teste le nombre et le type des arguments, ainsi que l'utilisation incorrecte de fonctions `void`.
  * `tests/err_assign.go` : Teste l'affectation de valeurs incorrectes et les assignations sur des non-lvalues (ex: `1 = x`).
  * `tests/err_return.go` : Teste la détection de `return` manquant dans les fonctions à branchements complexes.

### 3\. Problèmes Rencontrés et Solutions

#### Problème 1 : La portée des variables dans l'AST

**Défi :** Comment représenter le fait qu'une variable déclarée à la ligne 1 est visible à la ligne 2, si l'AST est construit séquentiellement ?
**Solution :** Utilisation d'une approche fonctionnelle dans le parser. Chaque instruction renvoie une fonction prenant "la suite du code" en argument.

  * Une instruction simple renvoie `fun s -> Instr :: s`.
  * Une déclaration renvoie `fun s -> [ Vars(..., s) ]`.
    Cela capture mécaniquement la suite du bloc à l'intérieur du nœud de déclaration.

#### Problème 2 : Le point-virgule automatique

**Défi :** Go permet d'omettre les `;` en fin de ligne, mais pas tout le temps (ex: pas après un `+`).
**Solution :** Ajout d'une variable d'état `ref semi` dans le lexer. Elle passe à `true` après les tokens pouvant finir une instruction (`IDENT`, `INT`, `RPAR`, `RETURN`, etc.). Lors de la rencontre d'un `\n`, si `!semi` est vrai, un token `SEMI` est émis.

#### Problème 3 : Analyse des retours dans les blocs `Vars`

**Défi :** Le typechecker signalait "Missing return" pour une fonction valide comme `if (...) { x:=1; return x }`.
**Cause :** L'instruction `Vars` (générée par `x:=1`) "cachait" le `return` à l'intérieur de son champ `seq`. Notre fonction d'analyse s'arrêtait au `Vars`.
**Solution :** Modification de la fonction récursive `terminates` pour qu'elle descende et inspecte le contenu des nœuds `Vars`.

#### Problème 4 : Appels de fonctions multivalués

**Défi :** Go permet `x, y := f()` où `f` renvoie deux valeurs. Notre fonction `type_expr` ne renvoyait qu'un seul type.
**Solution :**

  * `type_expr` lève une erreur si on utilise une fonction multiple dans une expression simple (ex: `1 + f()`).
  * Les instructions `Set` et `Return` gèrent un cas particulier "Appel de fonction" pour accepter et vérifier la correspondance multiple des types.


## Partie 2 : Génération de Code (Backend MIPS)

Cette seconde phase traduit l'Arbre de Syntaxe Abstraite (AST) en langage assembleur MIPS 32 bits, exécutable sur simulateur (Mars).

### 1. Fonctionnalités Implémentées

Le backend couvre l'ensemble des fonctionnalités du langage MicroGo, avec une gestion manuelle de la mémoire et de la pile.

####  Gestion des Expressions et Opérateurs
* **Arithmétique et Logique :** Traduction directe des opérateurs binaires (`+`, `-`, `*`, `/`, `%`) et logiques (`&&`, `||`, `!`).
* **Comparaisons :** Utilisation astucieuse des instructions MIPS pour simuler tous les opérateurs de comparaison :
    * `x < y` : `slt`
    * `x > y` : `slt` (en inversant les opérandes)
    * `x >= y` : `slt` + `xori 1` (négation de `<`)
    * `x == y` : `xor` + `sltiu 1` (vérifie si la différence est nulle)

####  Convention d'Appel et Gestion de la Pile
L'implémentation respecte une convention d'appel stricte basée sur la pile :
* **Stack Frame :** Utilisation systématique du registre `$fp` (Frame Pointer) pour accéder aux variables locales et arguments. Cela garantit la stabilité des accès mémoire même lorsque la pile varie (push/pop temporaires).
* **Prologue/Épilogue :** Sauvegarde et restauration automatiques de `$fp` et `$ra` (Return Address) à chaque appel.
* **Arguments :** Passés intégralement sur la pile par l'appelant.

####  Allocation Dynamique et Structures
* **Tas (Heap) :** Implémentation de `new(T)` via l'appel système `sbrk` (syscall 9).
* **Accès aux champs (`Dot`) :** Calcul des offsets à la compilation grâce à une table de hachage pré-calculée (`field_offsets`). L'instruction `p.x` génère un accès mémoire à l'adresse `p + offset_x`.
* **Incrémentation en place :** Support de `p.x++` en mémoire (lecture-modification-écriture sans perdre le pointeur de base).

####  Retours Multiples (Spécificité Go)
MicroGo permettant de renvoyer plusieurs valeurs (`return a, b`), nous avons opté pour une stratégie d'allocation sur le tas :
* L'instruction `return e1, e2...` alloue un bloc mémoire contigu contenant les valeurs.
* L'adresse de ce bloc est renvoyée dans le registre `$t0`.
* L'appelant "déballe" ce bloc pour affecter les valeurs aux variables de destination (ex: `x, y := f()`).

### 2. Tests Backend Ajoutés

Un fichier de test additionnel a été concu pour valider un aspect critique du code, qui est la bonne gestion de la récursivité, nous avons jugé que les autres tests déja fournis testaient assez exhaustivement les fonctionnalités implémentées :

* `tests/fact.go` : **Test de la pile et récursivité.**
    * Implémente une fonction factorielle récursive.
    * Objectif : Vérifier que les registres `$ra` et `$fp` sont correctement sauvegardés/restaurés lors d'appels imbriqués profonds.

### 3. Problèmes Rencontrés et Solutions

#### Problème 1 : Désalignement de la pile (`$sp` vs `$fp`)
**Symptôme :** Lors de l'accès aux variables locales, l'utilisation du pointeur de pile `$sp` (`lw $t0, -8($sp)`) renvoyait des valeurs corrompues après l'allocation de nouvelles variables locales.
**Cause :** `$sp` bouge constamment (push/pop). Un offset calculé à un instant T devient faux dès qu'on empile autre chose.
**Solution :** Passage exclusif au registre `$fp` (Frame Pointer) pour toutes les variables locales. `$fp` reste fixe durant toute l'exécution de la fonction, garantissant des offsets stables (`-4($fp)`, `-8($fp)`...).

#### Problème 2 : Écrasement d'adresse lors de l'incrémentation (`Inc`)
**Symptôme :** Erreur d'exécution `store address not aligned` lors d'un `p.x++`.
**Cause :** Le code généré faisait `lw $t0...`, `addi $t0...`. Le registre `$t0` contenant l'adresse de la structure était écrasé par la valeur du champ, puis incrémenté. L'écriture suivante (`sw`) tentait d'écrire à une adresse invalide (adresse + 1).
**Solution :** Utilisation d'un registre temporaire distinct (`$t1`) pour manipuler la valeur, tout en préservant l'adresse de base dans `$t0`.

#### Problème 3 : Affichage des retours multiples (`fmt.Print`)
**Symptôme :** `fmt.Print(div(a,b))` affichait une adresse mémoire (ex: `268697656`) au lieu des valeurs `73`.
**Cause :** `fmt.Print` est polymorphe. Le compilateur voyait un résultat dans `$t0` et l'affichait comme un entier, alors que c'était un pointeur vers un tuple (stratégie de retour multiple).
**Solution :** Introduction d'une table globale `func_returns` remplie au début de la compilation. Lors de la génération du code pour `Print`, si la fonction appelée renvoie N > 1 valeurs, on génère une boucle qui lit et affiche les N éléments du bloc mémoire pointé par `$t0`. Cela dit, pour le test dans `div.go`, le programme généré affiche `10\n73\n73\n268697672\n`. Le problème n'est donc pas tout à fait réglé.

## Compilation et Exécution

Pour compiler le projet :

```bash
make
# ou
dune build
```

Pour lancer le typechecker sur un fichier :

```bash
./mgoc tests/filename.go
```