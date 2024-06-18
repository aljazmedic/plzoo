
- Sample, na par strani pošlji

Uvod
    Parsanje,
    Ideja, potreba po fleksibilni notaciji
    kateri jeziki to omogočajo - proof assistants
    Želiš uporabniku omogočiti, da si sam naredi okolje, ki mu ustreza
    Warning: abusable
    historically -
    Fortran : Ni imel nič
    C++ : redefinicije infiksnih operatorjev, novejše v pythonu, ne da se narediti svojih
    OCaml : https://v2.ocaml.org/manual/expr.html#ss:precedence-and-associativity
    Haskel : infixl, infixr, infix



Teorija, osnovna razlaga
    infix `_+_` , prefix `sqrt_`, postfix `_!`, assoc <- glavne osebe
    mixfix `_[_]`, `_if_then_`
    (Ne definicije)
    - Parser generators - Yacc, Menhir - ne zanjo, komplikacija <- vrh :)
    parsanje, operatorji, precedenca, asociativnost - Zgled po članku
    razlaga pojmov,opis sintakse,
    parsanje, 
    Kaj so parser combinators.

Implementacija (bitka):
    Zakaj parser generatorji ne delajo? On-the-fly generacija parsanja - Parser COMBINATORS.
    Orodja na voljo:
        - Menhir uporabimo za nadgradnjo, popravimo samo un del k je treba
        - Parser combinators - Knjižnjice, ampak naša je bolj avtentična, 
    Satisfaction:
        Future work - happily forever after

GEneriraj 5mb kode pa jo požen       

Related work:
Proofassistant:
    - Agda : https://agda.readthedocs.io/en/v2.6.1/language/mixfix-operators.html
    - Coq - Še nad našim
    - Lean

Izboljšave:
    [ _,*_ ] -> 2 ali več (to lahko naredi Coq)
