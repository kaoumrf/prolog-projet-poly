/*verifier si liste ou pas*/
islist([_]).
islist([_|T]) :- islist(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Partie1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Afficher
myWrite(0,_) :-
    !,
    write('').
myWrite(X,0) :-
    !,
    write(X).
myWrite(X,1) :-
    !,
    write(' + '),
    write(X),
    write('x').
myWrite(X,P) :-
    write(' + '),
    write(X),
    write('x^'),
    write(P).
afficher([[X,X1]]) :-
    myWrite(X,X1).
afficher([[X,X1]|L]):-
    islist(L),
    myWrite(X,X1),
    afficher(L),
    !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%derivative
derivative(F,DF) :-
    der(F,G),
    simplifier(G,DF).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%der
der(N,0) :-
    number(N).
der(x,1).
der(-x,-1).
der(F^0,0).
der(F^N, N*F^M*DF) :-
    N>=1,
    M is N-1,
    der(F,DF).
der(E1*E2, E1*DE2 + E2*DE1) :-
    der(E1,DE1),
    der(E2,DE2).
der(E1+E2, DE1+DE2) :-
    der(E1,DE1),
    der(E2,DE2).
der(E1-E2,DE1-DE2) :-
    der(E1,DE1),
    der(E2,DE2).
der(-(E),DE) :-
    der((-1)*E,DE).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%simplifier
simplifier(P,SP) :-
    transform(P,TP),
    add-similaire(TP,S),
    supp_zero(S,S1),
    ordonner(S1,S2),
    btt(S2,SP).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%transform
transform(C,[[C,0]]) :-
    integer(C).
transform(x,[[1,1]]).
transform(-x,[[-1,1]]).
transform(A*B,L) :-
    transform(A,L1),
    transform(B,L2),
    multiplier(L1,L2,L).
transform(A^N,L) :-
     transform(A,L1),
     degree(L1,N,L).
transform(A+B,RT) :-
     transform(A,RA),
     transform(B,RB),
     append(RA,RB,RT).
transform(A-B,RT) :-
     transform(A,RA),
     transform(-B,RB),
     append(RA,RB,RT).
transform(-(A),R) :-
     transform((-1)*A,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%mult
multiplier_one(_,[],[]).
multiplier_one([C1,N1],[[C2,N2]|R],[[C,N]|T]) :-
    C is C1*C2,
    N is N1+N2,
    multiplier_one([C1,N1],R,T).

multiplier([],_,[]).
multiplier([H1|T1],T2,T) :-
    multiplier_one(H1,T2,R1),
    multiplier(T1,T2,R2),
    append(R1,R2,T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%degree

degree(L1,0,[[1,0]]).
degree(L1,1,L1).
degree(L1,N,L) :-
    N > 1,
    M is N-1,
    degree(L1,M,L2),
    multiplier(L1,L2,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%add-similaire
add-similaire([],[]).
add-similaire([[C1,N]|T],S) :-
    append(A,[[C2,N]|B],T),
    C is C1+C2,
    append(A,[[C,N]|B],R),
    add-similaire(R,S).

add-similaire([[C,N]|T],[[C,N]|ST]) :-
    not_in(N,T),
    add-similaire(T,ST).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% supp_zero(L1,L2) 

supp_zero([],[]).
supp_zero([[0,_]|T],T1) :-
    supp_zero(T,T1).
supp_zero([[C,N]|T],[[C,N]|T1]) :-
    dif(C,0),
    supp_zero(T,T1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% not_in(N,L)

not_in(_,[]).
not_in(_,[[]]).
not_in(N,[[_,M]|T]) :-
     dif(N,M),
     not_in(N,T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ordonner(L1,L2) 

ordonner([X|Xs],Ys) :-
     ordonner(Xs,Zs),
     inserer(X,Zs,Ys).
ordonner([],[]).

inserer(X,[],[X]).
inserer(X,[Y|Ys],[Y|Zs]) :-
     greater(Y,X),
     inserer(X,Ys,Zs).
inserer(X,[Y|Ys],[X,Y|Ys]) :-
     greatereq(X,Y).

greater([_,N1],[_,N2]) :-
     N1 > N2.

greatereq([_,N1],[_,N2]) :-
     N1 >= N2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%back to terms (L,T) 

btt([[1,1]],x).
btt([[-1,1],-x]).
btt([[C,0]],C) :-
     integer(C).
btt([[1,N]],x^N):-
      dif(N,0),
      dif(N,1).
btt([[-1,N]],-x^N):-
      dif(N,0),
      dif(N,1).
btt([[C,1]],C*x):-
      dif(C,1),
      dif(C,-1).
btt([[C,N]],C*x^N):-
     dif(N,1),
     dif(C,1),
     dif(C,-1).
btt(L,F+T) :-
     append(L1,[[C,N]],L),
     dif(L1,[]),
     C > 0,
     btt(L1,F),
     btt([[C,N]],T).
btt(L,F-T) :-
     append(L1,[[C,N]],L),
     dif(L1,[]),
     C < 0,
     abs(C,AC),
     btt(L1,F),
     btt([[AC,N]],T).
	 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%evaluer
	ev3(X,Y,Z) :- ev(X,Y,1,Z),!.

	ev(_,0,A,Z) :- Z is A.
	ev(X,Y,A,Z) :- 
                Y1 is Y - 1, 
                A1 is A*X, 
                ev(X,Y1,A1,Z).
	 
	evaluation([],X,0).
	evaluation([[C,N]|Poly1],X,P):-
                evaluation(Poly1,X,P1), 
                ev3(X,N,PP),
                P is (P1+C*PP).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%lecture	
	lecture(X):-
        write("Donnez un polynome: "),
        nl,
        read(X),
        nl,
        write(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%somme
	somme([],Poly,Poly) :- !.
	somme(Poly,[],Poly) :- !.
	somme([(Ai,Ni)|Poly1],[(Aj,Nj)|Poly2],[(Ai,Ni)|Poly]) :-
			Ni > Nj, 
            !, 
            somme(Poly1,[(Aj,Nj)|Poly2],Poly).
	somme([(Ai,Ni)|Poly1],[(Aj,Nj)|Poly2],[(A,Ni)|Poly]) :-
			Ni =:= Nj,
            !,
            A is Ai+Aj, 
            somme(Poly1,Poly2,Poly).
	somme([(Ai,Ni)|Poly1],[(Aj,Nj)|Poly2],[(Aj,Nj)|Poly]) :-
			Ni < Nj, 
            !, 
            somme([(Ai,Ni)|Poly1],Poly2,Poly).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%soustraction
soustraction(Poly1,Poly2,Poly) :-
            produit_one(Poly2,(-1,0),Poly3),
            somme(Poly1,Poly3,Poly), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%produit

	produit_one([(C1,N1)|Poly1],(C,N),[(C2,N2)|Poly]) :-
			C2 is C1*C, N2 is N1+N, produit_one(Poly1,(C,N),Poly).
	produit_one([],_,[]).

	produit([(C,N)|Poly1],Poly2,Poly) :-
			produit_one(Poly2,(C,N),Poly3),
			produit(Poly1,Poly2,Poly4),
			somme(Poly3,Poly4,Poly).
	produit([],_,[]).

	binomial(Poly,1,Poly).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


eq(X,X).
dif(X,Y) :-
    \+ eq(X,Y).
abs(X,X) :-
    integer(X),
    X >= 0.
abs(X,Y) :-
    integer(X),
    X < 0,
    Y is (-1)*X.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Partie 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-op(500,xfy,[+,-,*]).
:-op(900,fy,[simp,deri]).
:-op(700,xfx,est).
est(X,+(A,B)):-somme(A,B,R),write(R).
est(X,-(A,B)):-soustraction(A,B,R),write(R).
est(X,*(A,B)):-produit(A,B,R),write(R).
simp(X):-simplifier(X,XS),write(XS).
deri(X):-derivative(X,XS),write(XS).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Partie1
/*
afficher([[1,0],[3,1],[2,3],[3,3]]).
simplifier((5*x^1+x^1+3*x^2+10+5),X).
derivative((5*x^1+x^1),X).
evaluation([[-1,1],[0,2],[3,5]],3,X).
lecture(X)./*3*x^2+x^1+4.*/
produit([(3,1),(2,2)],[(2,0)],X).
somme([(3,1),(2,2)],[(1,4)],X).
soustraction([(3,1),(2,2)],[(1,1)],X).*/


%Partie2
/*
Somme est ([(3,1),(2,2)]+[(4,0),(2,3)]).
Difference est ([(3,1),(2,2)]-[(2,1)]).
Produit  est ([(4,3),(2,2)]*[(2,1)]).
deri(x^2+x^1).
simp (5*x^1+x^1+3*x^2+10+5).*/

