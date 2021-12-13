# prolog-projet-poly
Première Partie :

1.	Représentation de ces polynômes d'une variable à coefficients réels, par une liste de monomes. Chaque monome est représenté par un couple : coefficient et degré du monome. 
Exemple :  pour le polynôme :  , la lise est : [ [1.5, 0] , [-1,1] , [0, 2] , [3.5,3] ].

2.	Opérations sur les polynômes :

•	Lecture : lecture d’un polynôme.
•	Afficher : afficher un polynôme (affichage comme dans l’exemple précédent) 
•	Simplifier : simplifier un polynôme
•	Evaluation : évaluer le polynôme, pour une valeur réelle x donnée.
•	Dérivation : calculer le polynôme dérivé.
•	Somme : calculer la somme de deux polynômes
•	Soustraction : calculer la différence entre deux polynômes
•	Produit : calculer le produit de deux polynômes

Seconde partie : (redéfinition des opérateurs) 

Ecrire et évaluer des opérations sur les polynômes tel que
 
-Priorités : produit > addition > difference > simplification = derivation> affectation 
-Nom des opérateurs : produit, (‘*’), addition (‘+’), différence (‘-’ ), simplification ( ‘simp’ ) = derivation (‘deri’)> affectation (‘est’).
-Associativité : intersection, union  et  différence (associativité à gauche). Simplification, derivation et affectation (non associative)

L’opérateur d’affectation (est) permet d’évaluer l’expression des polynômes. Il correspond à l’opération is de Prolog.
