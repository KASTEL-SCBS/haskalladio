/* ************************** EX1 ********************************* */
/* This is a  simple example with NAF through abduction.            */
/* Try this example with the following goals (G1 and G2):           */
/* G1: demo([flies(tweety)],[],FinalExp).                           */
/* G2: demo([flies(sam)],[],FinalExp).                              */
/* Note here that only the second goal (G2) succeeds.               */
/* **************************************************************** */

:- dynamic flies/1,abnormal/1,bird/1,penguin/1,eagle/1.


flies(X) :- bird(X), not(abnormal(X)).

abnormal(X) :- penguin(X).

bird(X) :- penguin(X).
bird(X) :- eagle(X).

penguin(tweety).
eagle(sam).


/* Abducibles */

/* We don't define any abducibles */
abducible_predicate(none).
