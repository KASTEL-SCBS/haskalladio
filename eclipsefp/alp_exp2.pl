/* ************************** EX2 ********************************* */
/* This is an example with NAF through abduction.                   */
/* Try this example with the following goals (G1 and G2):           */
/* G1: demo([flies(sam)],[],FinalExp).                              */
/* G2: demo([not(flies(sam))],[],FinalExp).                         */
/* **************************************************************** */

:- dynamic flies/1,abnormal/1,light/1,heavy/1,bird/1,penguin/1,eagle/1.


flies(X) :- bird(X), not(abnormal(X)).

abnormal(X) :- penguin(X).
abnormal(X) :- not(light(X)).

light(X) :- not(heavy(X)).

heavy(X) :- not(light(X)).

bird(X) :- penguin(X).
bird(X) :- eagle(X).

penguin(tweety).
eagle(sam).


/* Abducibles */

/* We don't define any abducibles */
abducible_predicate(none).
